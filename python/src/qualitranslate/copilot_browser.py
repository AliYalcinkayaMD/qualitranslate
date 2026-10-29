"""Visible-browser transport for institutional Microsoft 365 Copilot Chat.

QualiTranslate opens Chrome or Edge with its own dedicated browser profile and a
loopback-only DevTools port, then works the visible chat page the way a person
would: open a new (temporary) chat, put the prepared text in the message box,
press Send, and read the reply shown on screen.

It never reads the user's ordinary browser profile, cookies, tokens, storage or
network traffic, and it never calls a Microsoft model endpoint directly.
The user signs in themselves in the dedicated window. That sign-in is kept in
the dedicated profile so it is not needed every session; "Forget sign-in"
removes the profile folder.

Behaviour checked against the signed-in copilot.cloud.microsoft/chat page on a
Mac (Chrome 156, 27 Oct 2026). Microsoft can change that page at any time.
"""
from __future__ import annotations
import base64
import hashlib
import json
import os
import re
import secrets
import shutil
import socket
import struct
import subprocess
import sys
import tempfile
import threading
import time
import urllib.parse
import urllib.request
from pathlib import Path

COPILOT_URL = 'https://copilot.cloud.microsoft/chat'
ALLOWED_HOSTS = {'copilot.cloud.microsoft', 'm365.cloud.microsoft'}
MAX_FRAME = 12_000_000
DOM_SCRIPT = (Path(__file__).with_name('copilot_dom.js')).read_text(encoding='utf-8')
_HELD_PROFILES = set()          # profile folders in use by this process
_HELD_GUARD = threading.Lock()
# Copilot's editor (Lexical) appends U+200B U+200C after typed text. Only true invisible
# formatting (ZWSP, word joiner, BOM) and that trailing artefact are ignored; ZWNJ/ZWJ and
# direction marks inside the text are meaningful and must match exactly.
INVISIBLE = re.compile('[\u200b\u2060\ufeff]')
EDITOR_TAIL = re.compile('[\u200b\u200c]+$')
from .prompts import REPLY_FORMAT  # noqa: E402  (shared with the R package)


class BrowserError(ValueError):
    def __init__(self, message, code='connection'):
        super().__init__(message)
        self.code = code


def _cancelled(cancel):
    if cancel is not None and cancel.is_set():
        raise BrowserError('Stopped. No further Copilot messages will be sent. A reply already being written may finish in the Copilot window.', 'cancelled')


# Short service refusals observed live: "Sorry, it looks like I can't respond to this. Let's try a
# different topic." / "Hmm...it looks like I can't chat about this."
REFUSAL = re.compile(r"can[’']?t (?:respond|chat|help|answer|assist)|cannot (?:respond|help|answer|assist)|unable to (?:respond|help|answer)|"
                     r"not able to (?:respond|help)|different topic|kan ikke (?:svare|hjælpe)|yardımcı olamam|yanıt veremem", re.I)


def editor_text(text):
    """The composer text without the editor's trailing zero-width artefact."""
    return EDITOR_TAIL.sub('', (text or '').rstrip())


def normalise(text):
    """Compare composer text the way a reader would: ignore spacing and invisible formatting."""
    return re.sub(r'\s+', ' ', INVISIBLE.sub('', editor_text(text))).strip()


def extract_receipt_json(text, receipt):
    """Only accept an object carrying this run's unpredictable receipt; never an input echo."""
    decoder = json.JSONDecoder()
    for match in re.finditer(r'\{', text):
        try:
            value, end = decoder.raw_decode(text[match.start():])
        except (ValueError, RecursionError):
            continue
        if isinstance(value, dict) and value.get('qt_receipt') == receipt and isinstance(value.get('segments'), list):
            return text[match.start():match.start() + end]
    return None


def default_profile_dir():
    if sys.platform == 'darwin':
        base = Path.home() / 'Library' / 'Application Support'
    elif sys.platform == 'win32':
        base = Path(os.environ.get('LOCALAPPDATA') or Path.home())
    else:
        base = Path(os.environ.get('XDG_DATA_HOME') or Path.home() / '.local' / 'share')
    return base / 'QualiTranslate' / 'copilot-browser-profile'


class DevToolsSocket:
    def __init__(self, url, expected_port, cancel=None, deadline=None):
        u = urllib.parse.urlsplit(url)
        if u.scheme != 'ws' or u.hostname not in {'127.0.0.1', 'localhost'} or u.port != expected_port or not re.fullmatch(r'/devtools/(?:page|browser)/[A-Za-z0-9-]+', u.path):
            raise BrowserError('Unexpected browser control address. Open a new Copilot window from this app.')
        self.cancel = cancel; self.deadline = deadline; self.next_id = 0; self.buffer = b''
        self.sock = socket.create_connection(('127.0.0.1', expected_port), timeout=3)
        self.sock.settimeout(1)
        key = base64.b64encode(secrets.token_bytes(16)).decode()
        request = ('GET ' + u.path + ' HTTP/1.1\r\nHost: 127.0.0.1:' + str(expected_port) + '\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: ' + key + '\r\nSec-WebSocket-Version: 13\r\n\r\n')
        self.sock.sendall(request.encode())
        deadline = min(time.monotonic() + 5, self.deadline or float('inf'))
        data = b''
        while b'\r\n\r\n' not in data:
            _cancelled(cancel)
            if time.monotonic() > deadline:
                raise BrowserError('Browser control handshake timed out.')
            try:
                chunk = self.sock.recv(4096)
                if not chunk:
                    raise BrowserError('Browser control connection closed.')
                data += chunk
            except socket.timeout:
                continue
            if not data or len(data) > 65536:
                raise BrowserError('Browser control connection closed.')
        header, self.buffer = data.split(b'\r\n\r\n', 1)
        accept = base64.b64encode(hashlib.sha1((key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11').encode()).digest()).decode()
        if not header.split(b'\r\n', 1)[0].startswith(b'HTTP/1.1 101') or accept.lower() not in header.decode(errors='replace').lower():
            self.close(); raise BrowserError('Browser control handshake was rejected.')

    def close(self):
        try:
            self.sock.close()
        except OSError:
            pass

    def _read(self, n, deadline):
        _cancelled(self.cancel)
        if time.monotonic() > deadline:
            raise BrowserError('Browser control timed out. Check the Copilot window before resuming.')
        while len(self.buffer) < n:
            _cancelled(self.cancel)
            if time.monotonic() > deadline:
                raise BrowserError('Browser control timed out. Check the Copilot window before resuming.')
            try:
                part = self.sock.recv(min(65536, n - len(self.buffer)))
            except socket.timeout:
                continue
            if not part:
                raise BrowserError('The Copilot browser window was closed.', 'browser_closed')
            self.buffer += part
        result, self.buffer = self.buffer[:n], self.buffer[n:]
        return result

    def _send(self, payload, opcode=1):
        if isinstance(payload, str):
            payload = payload.encode()
        if len(payload) > MAX_FRAME:
            raise BrowserError('Browser message is too large. Reduce the batch size.')
        n = len(payload); head = bytes([0x80 | opcode])
        head += bytes([0x80 | n]) if n < 126 else bytes([0x80 | 126]) + struct.pack('!H', n) if n < 65536 else bytes([0x80 | 127]) + struct.pack('!Q', n)
        mask = secrets.token_bytes(4)
        self.sock.sendall(head + mask + bytes(v ^ mask[i % 4] for i, v in enumerate(payload)))

    def _message(self, deadline):
        collected = b''
        while True:
            a, b = self._read(2, deadline); opcode = a & 15; n = b & 127
            if n == 126:
                n = struct.unpack('!H', self._read(2, deadline))[0]
            elif n == 127:
                n = struct.unpack('!Q', self._read(8, deadline))[0]
            if n > MAX_FRAME or len(collected) + n > MAX_FRAME:
                raise BrowserError('Browser response is too large.')
            mask = self._read(4, deadline) if b & 128 else None
            body = self._read(n, deadline)
            if mask:
                body = bytes(v ^ mask[i % 4] for i, v in enumerate(body))
            if opcode == 8:
                raise BrowserError('The Copilot browser connection closed.', 'browser_closed')
            if opcode == 9:
                self._send(body, 10); continue
            if opcode == 10:
                continue
            if opcode not in {0, 1}:
                raise BrowserError('Unsupported browser control frame.')
            collected += body
            if a & 128:
                return collected.decode('utf-8')

    def call(self, method, params=None, timeout=12):
        _cancelled(self.cancel); self.next_id += 1; request_id = self.next_id
        self._send(json.dumps({'id': request_id, 'method': method, 'params': params or {}}, ensure_ascii=False))
        deadline = min(time.monotonic() + timeout, self.deadline or float('inf'))
        while True:
            message = json.loads(self._message(deadline))
            if message.get('id') != request_id:
                continue
            if 'error' in message:
                raise BrowserError('Copilot page changed or is not ready. Check its window and reconnect.', 'page_changed')
            return message.get('result', {})

    def evaluate(self, expression, context_id=None):
        params = {'expression': expression, 'returnByValue': True, 'awaitPromise': True}
        if context_id is not None:
            params['contextId'] = context_id
        result = self.call('Runtime.evaluate', params)
        if result.get('exceptionDetails'):
            raise BrowserError('The Copilot page changed. No reply was imported; check the browser and reconnect.', 'page_changed')
        return result.get('result', {}).get('value')


class CopilotBrowser:
    """One dedicated Chrome/Edge window driven through its loopback DevTools port."""

    def __init__(self, executable=None, start_url=COPILOT_URL, allow_fixture=False, extra_args=None, environment=None,
                 timeout=300, profile_dir=None, temporary_chat=True, new_chat_timeout=45, settle_seconds=6, exclusive=True):
        # Injection hooks are Python-only test facilities, never HTTP or UI parameters.
        self.executable = executable; self.start_url = start_url; self.allow_fixture = allow_fixture
        self.extra_args = extra_args or []; self.environment = environment; self.timeout = timeout
        self.new_chat_timeout = new_chat_timeout
        # Copilot keeps initialising for a few seconds after the page appears; a message sent
        # too early can be silently cleared (observed live, 27 Oct 2026).
        self.settle_ms = int(settle_seconds * 1000)
        self.decline_quiet_seconds = 12   # a finished non-JSON reply this long is treated as a refusal
        self.exclusive = exclusive        # only one QualiTranslate copy may drive the window
        self._lock_file = None
        self.temporary_chat = temporary_chat
        self.profile_dir = Path(profile_dir) if profile_dir else None   # None = default persistent dedicated profile
        self._temp_profile = None
        self.port = None; self.browser_ws = None; self.process = None; self.target_id = None
        self.browser_version = ''; self.last_receipt = ''
        self.lock = threading.RLock()
        self.cached = {'open': False, 'ready': False, 'experimental': True, 'error_code': '',
                       'message': 'Not connected yet. Click Connect & translate with Copilot.',
                       'step': 'idle', 'step_detail': '', 'page_host': '', 'temporary_chat': temporary_chat}

    # ----- status -------------------------------------------------------------------------
    def summary(self):
        result = dict(self.cached)
        since = result.pop('step_since', None)
        result['step_elapsed'] = int(time.monotonic() - since) if since else 0
        return result

    def _step(self, step, detail=''):
        self.cached.update(step=step, step_detail=detail, step_since=time.monotonic())

    # ----- launching / attaching ----------------------------------------------------------
    @staticmethod
    def _find_browser():
        paths = []
        if sys.platform == 'darwin':
            for base in (Path('/Applications'), Path.home() / 'Applications'):
                paths.extend([base / 'Google Chrome.app/Contents/MacOS/Google Chrome', base / 'Microsoft Edge.app/Contents/MacOS/Microsoft Edge'])
        elif sys.platform == 'win32':
            for env in ('PROGRAMFILES', 'PROGRAMFILES(X86)', 'LOCALAPPDATA'):
                if os.environ.get(env):
                    paths.extend([Path(os.environ[env]) / 'Google/Chrome/Application/chrome.exe', Path(os.environ[env]) / 'Microsoft/Edge/Application/msedge.exe'])
        else:
            paths = [Path(p) for name in ('google-chrome', 'chromium', 'chromium-browser', 'microsoft-edge') if (p := shutil.which(name))]
        for path in paths:
            if path.is_file():
                return str(path)
        raise BrowserError('Google Chrome or Microsoft Edge is needed for automatic Copilot mode. Install one from its official website, then reopen this app.', 'no_browser')

    def _profile(self):
        if self.profile_dir is None:
            self.profile_dir = default_profile_dir()
        if str(self.profile_dir) == 'temporary':
            if self._temp_profile is None:
                self._temp_profile = tempfile.TemporaryDirectory(prefix='QualiTranslate-Copilot-')
            return Path(self._temp_profile.name)
        self.profile_dir.mkdir(parents=True, exist_ok=True)
        try:
            os.chmod(self.profile_dir, 0o700)
        except OSError:
            pass
        return self.profile_dir

    def _http(self, path, port=None):
        port = port or self.port
        if not port:
            raise BrowserError('Open the Copilot window first.')
        opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
        try:
            with opener.open('http://127.0.0.1:' + str(port) + path, timeout=3) as response:
                raw = response.read(2_000_001)
            if len(raw) > 2_000_000:
                raise BrowserError('Unexpected browser control response.')
            return json.loads(raw)
        except (OSError, ValueError):
            raise BrowserError('The Copilot window is not connected. Click Connect & translate with Copilot (or Resume with Copilot) to open it again.', 'browser_closed') from None

    # Backwards-compatible name used by older tests.
    def _json(self, path):
        return self._http(path)

    def _allowed(self, url):
        u = urllib.parse.urlsplit(url)
        return (u.scheme == 'https' and u.hostname in ALLOWED_HOSTS and u.port in {None, 443}) or (self.allow_fixture and u.scheme == 'http' and u.hostname == '127.0.0.1')

    def _read_portfile(self, portfile):
        lines = portfile.read_text(encoding='utf-8').splitlines(); port = int(lines[0]); ws_path = lines[1]
        if not 1 <= port <= 65535 or not re.fullmatch(r'/devtools/browser/[A-Za-z0-9-]+', ws_path):
            raise ValueError('bad port file')
        return port, 'ws://127.0.0.1:' + str(port) + ws_path

    def _attach(self, port, browser_ws):
        info = self._http('/json/version', port)
        self.port = port; self.browser_ws = browser_ws
        self.browser_version = str(info.get('Browser', ''))[:60]

    def _acquire(self, profile):
        """Hold an OS lock in the profile folder while this app copy uses the window."""
        if not self.exclusive or self._lock_file is not None:
            return
        busy = BrowserError('Another QualiTranslate copy is already using the Copilot window. Save progress in that copy and close its Terminal window, '
                            'then try again here. Two copies must not type into the same Copilot window.', 'in_use')
        key = str(Path(profile).resolve())
        with _HELD_GUARD:
            # POSIX record locks belong to the process, so a second user of the same profile
            # inside this process is refused here, before the lock file is opened again
            # (closing another handle to the file would silently drop the lock).
            if key in _HELD_PROFILES:
                raise busy
            try:
                import fcntl
            except ImportError:   # Windows: no cross-process lock (not a supported platform yet)
                fcntl = None
            handle = None
            if fcntl is not None:
                handle = open(profile / 'QualiTranslate-in-use.lock', 'a+')
                try:
                    # POSIX record lock (fcntl), the same kind the R version takes with filelock::lock(),
                    # so a Python and an R session cannot drive the same Copilot window at once.
                    fcntl.lockf(handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
                except OSError:
                    handle.close()
                    raise busy from None
            _HELD_PROFILES.add(key)
            self._lock_key = key
            self._lock_file = handle if handle is not None else True

    def _release(self):
        with _HELD_GUARD:
            if self._lock_file is not None:
                if self._lock_file is not True:
                    try:
                        self._lock_file.close()
                    except OSError:
                        pass
                _HELD_PROFILES.discard(getattr(self, '_lock_key', ''))
                self._lock_file = None

    def open(self):
        with self.lock:
            if self.port:
                try:
                    self._http('/json/version'); self._ensure_tab(); return self.summary()
                except BrowserError:
                    self._forget_connection()
            self._step('browser_starting', 'Opening the dedicated Copilot browser window')
            profile = self._profile(); portfile = profile / 'DevToolsActivePort'
            self._acquire(profile)
            # Re-use the dedicated window if it is still open from an earlier session.
            if portfile.exists():
                try:
                    port, ws = self._read_portfile(portfile); self._attach(port, ws)
                    self._ensure_tab()
                    self.cached.update(open=True, ready=False, error_code='', message='Reconnected to the open Copilot window.')
                    return self.summary()
                except (OSError, ValueError, IndexError, BrowserError):
                    self._forget_connection()
                    try:
                        portfile.unlink()
                    except OSError:
                        pass
            executable = self.executable or self._find_browser()
            args = [executable, '--remote-debugging-address=127.0.0.1', '--remote-debugging-port=0', '--user-data-dir=' + str(profile),
                    '--no-first-run', '--no-default-browser-check', '--new-window',
                    # Keep the chat page responsive while the user works in other windows.
                    '--disable-background-timer-throttling', '--disable-renderer-backgrounding', '--disable-backgrounding-occluded-windows',
                    *self.extra_args, self.start_url]
            try:
                self.process = subprocess.Popen(args, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, env=self.environment)
            except OSError:
                self.process = None
                raise BrowserError('The Copilot browser could not start. Check that Chrome or Edge is installed.', 'no_browser') from None
            deadline = time.monotonic() + 25
            while time.monotonic() < deadline:
                if portfile.exists():
                    try:
                        port, ws = self._read_portfile(portfile); self._attach(port, ws)
                        self.cached.update(open=True, ready=False, error_code='', message='Copilot window opened. Sign in there if Microsoft asks you to.')
                        return self.summary()
                    except (OSError, ValueError, IndexError, BrowserError):
                        pass
                if self.process.poll() is not None and self.process.returncode != 0:
                    break
                time.sleep(.15)
            self._forget_connection()
            raise BrowserError('The browser opened without its local control connection. Close every window titled with the QualiTranslate Copilot profile and try again. '
                               'If this repeats, your organisation may block browser automation.', 'no_control')

    def _forget_connection(self):
        self.port = None; self.browser_ws = None; self.process = None; self.target_id = None
        self.cached.update(open=False, ready=False)

    def _browser_call(self, method, params=None, timeout=8):
        cdp = None
        try:
            cdp = DevToolsSocket(self.browser_ws, self.port)
            return cdp.call(method, params or {}, timeout=timeout)
        finally:
            if cdp:
                cdp.close()

    def _ensure_tab(self):
        pages = [p for p in self._http('/json/list') if p.get('type') == 'page']
        if not pages:
            result = self._browser_call('Target.createTarget', {'url': self.start_url})
            self.target_id = result.get('targetId')

    # ----- page and frame inspection -----------------------------------------------------
    def _page(self):
        pages = [p for p in self._http('/json/list') if p.get('type') == 'page']
        eligible = [p for p in pages if self._allowed(p.get('url', ''))]
        chosen = next((p for p in eligible if p.get('id') == self.target_id), None)
        if chosen is None and eligible:
            chat = [p for p in eligible if urllib.parse.urlsplit(p.get('url', '')).path.startswith('/chat')]
            chosen = (chat or eligible)[0]
        if chosen is not None:
            self.target_id = chosen.get('id')
            self.cached['page_host'] = urllib.parse.urlsplit(chosen.get('url', '')).hostname or ''
            return chosen
        if not pages:
            try:
                self._ensure_tab()
            except BrowserError:
                pass
            raise BrowserError('Opening Copilot in its window…', 'loading')
        hosts = sorted({urllib.parse.urlsplit(p.get('url', '')).hostname or '' for p in pages} - {''})
        self.cached['page_host'] = ', '.join(hosts)[:120]
        if any('login' in h or 'microsoftonline' in h or 'sts.' in h or 'adfs' in h or 'wayf' in h for h in hosts):
            raise BrowserError('Waiting for you to finish signing in to Microsoft in the Copilot window (' + ', '.join(hosts)[:80] + ').', 'sign_in')
        raise BrowserError('The Copilot window is showing another page (' + ', '.join(hosts)[:80] + '). Finish sign-in there; the app will open Copilot Chat when it can.', 'other_page')

    def _connect_page(self, cancel=None, deadline=None):
        page = self._page()
        return DevToolsSocket(page['webSocketDebuggerUrl'], self.port, cancel, deadline)

    def _inspect(self, cdp):
        tree = cdp.call('Page.getFrameTree').get('frameTree', {})
        frames = []
        def collect(node):
            frame = node.get('frame', {})
            if self._allowed(frame.get('url', '')):
                frames.append(frame)
            for child in node.get('childFrames', []):
                collect(child)
        collect(tree)
        best = None
        for frame in frames:
            try:
                context = cdp.call('Page.createIsolatedWorld', {'frameId': frame['id'], 'worldName': 'QualiTranslate', 'grantUniveralAccess': False}).get('executionContextId')
                info = cdp.evaluate(DOM_SCRIPT, context)
            except BrowserError as exc:
                if exc.code == 'cancelled':
                    raise
                continue
            if not isinstance(info, dict):
                continue
            if info.get('challenge') or info.get('limit'):
                self._attention(info)
            if info.get('has_editor'):
                return context, info
            best = best or (context, info)
        if best:
            return best
        raise BrowserError('Copilot Chat is still loading or its message box is not visible yet.', 'loading')

    @staticmethod
    def _attention(info):
        if info.get('authentication_required'):
            raise BrowserError('Sign in to Copilot yourself in its browser window.', 'sign_in')
        if info.get('challenge'):
            raise BrowserError('Copilot is asking for human verification. Resolve it yourself in that window; the app will not bypass it.', 'verification')
        if info.get('limit'):
            raise BrowserError('Copilot reports a usage limit, a message-length limit, or a temporary restriction. Stop and resume later or reduce the batch size; completed batches are retained.', 'usage_limit')

    def _wait_for(self, cdp, test, seconds, cancel, step_message):
        deadline = time.monotonic() + seconds; last = None
        while time.monotonic() < deadline:
            _cancelled(cancel)
            try:
                context, info = self._inspect(cdp)
                self._attention(info); last = info
                if test(info):
                    return context, info
            except BrowserError as exc:
                if exc.code in {'cancelled', 'verification', 'usage_limit', 'sign_in', 'browser_closed', 'reloaded'}:
                    raise
            time.sleep(.3)
        raise BrowserError(step_message, 'timeout')

    # ----- connection check (no prompt is ever sent) ---------------------------------------
    def check(self):
        with self.lock:
            cdp = None
            try:
                if self.process is not None and self.process.poll() is not None and not self._alive():
                    self._forget_connection()
                    raise BrowserError('The Copilot window was closed. Click Connect & translate with Copilot (or Resume with Copilot) to open it again.', 'browser_closed')
                cdp = self._connect_page(deadline=time.monotonic() + 10)
                context, info = self._inspect(cdp); self._attention(info)
                if not info.get('has_editor'):
                    raise BrowserError('Copilot Chat is open but its message box is not visible yet.', 'loading')
                self.cached.update(open=True, ready=True, error_code='', message='Connected: you are signed in and the Copilot message box was found. Nothing has been sent yet.',
                                   model_label=info.get('model_label', ''), browser=self.browser_version)
            except BrowserError as exc:
                self.cached.update(ready=False, error_code=exc.code, message=str(exc))
            finally:
                if cdp:
                    cdp.close()
            return self.summary()

    def _alive(self):
        try:
            self._http('/json/version'); return True
        except BrowserError:
            return False

    # ----- one structured request ---------------------------------------------------------
    def _fresh_chat(self, cdp, cancel):
        """Navigate to a new chat, switch on Temporary chat if requested, and verify it is empty."""
        self._step('new_chat', 'Opening a new empty Copilot chat')
        try:
            old_load = self._inspect(cdp)[1].get('page_load_id')
        except BrowserError as exc:
            if exc.code == 'cancelled':
                raise
            old_load = None
        cdp.call('Page.navigate', {'url': self.start_url}, timeout=15)
        start_path = urllib.parse.urlsplit(self.start_url).path.rstrip('/') or '/'
        def empty(info):
            # Only the newly loaded page counts: the previous page can look empty while it unloads.
            return (info.get('page_load_id') and info.get('page_load_id') != old_load
                    and info.get('has_editor') and info.get('composer_empty') and info.get('turn_count') == 0 and not info.get('stopping')
                    and not info.get('previous_receipt_present') and (info.get('path', '').rstrip('/') or '/') == start_path)
        def settled(info):
            return empty(info) and info.get('ready_state') == 'complete' and info.get('page_age_ms', 0) >= self.settle_ms
        context, info = self._wait_for(cdp, settled, self.new_chat_timeout, cancel,
                                       'A new empty Copilot chat could not be confirmed. Nothing was sent. Check the Copilot window, then use Retry.')
        isolation = 'new_empty_chat_verified'; notice = ''
        if self.temporary_chat:
            self._step('new_chat', 'Switching on Copilot temporary chat')
            if not info.get('has_temporary'):
                raise BrowserError('Copilot did not show its Temporary chat switch, so nothing was sent. To continue with ordinary (saved) chats, '
                                   'turn off "Use Copilot temporary chats" in Advanced settings and record that change.', 'temporary_unavailable')
            if not info.get('temporary_on'):
                cdp.evaluate('globalThis.__qtUI.temporaryButton.click(); true', context)
                time.sleep(1.5)
            context, info = self._wait_for(cdp, lambda i: i.get('temporary_on') and i.get('temporary_heading') and empty(i), 15, cancel,
                                           'Copilot temporary chat could not be switched on and confirmed. Nothing was sent.')
            notice = cdp.evaluate('globalThis.__qtUI.temporaryNotice()', context) or ''
            isolation = 'new_temporary_chat_verified'
        return context, info, isolation, notice

    def complete(self, task, cancel=None):
        with self.lock:
            cdp = None; submitted = False
            try:
                cdp = self._connect_page(cancel)
                context, info, isolation, notice = self._fresh_chat(cdp, cancel)
                load_id = info.get('page_load_id')
                receipt = 'QT_RECEIPT_' + secrets.token_hex(16)
                packet = task['system'] + '\n\nINPUT JSON:\n' + task['user'] + '\n\n' + REPLY_FORMAT.format(receipt=receipt)
                task['transport_prompt'] = packet
                self.last_receipt = receipt
                cdp.evaluate('globalThis.__qtReceipt=' + json.dumps(receipt) + '; true', context)
                self._step('inserting', 'Putting the prepared text in the Copilot message box')
                cdp.evaluate('globalThis.__qtUI.editor.focus(); true', context)
                cdp.call('Input.insertText', {'text': packet}, timeout=60)
                wanted = normalise(packet); visible = ''
                deadline = time.monotonic() + 15; filled = {}
                while time.monotonic() < deadline:
                    _cancelled(cancel)
                    context, filled = self._inspect(cdp); self._attention(filled)
                    if filled.get('page_load_id') != load_id:
                        raise BrowserError('The Copilot page reloaded while the text was being prepared. Nothing was sent. Use Retry.', 'reloaded')
                    visible = cdp.evaluate('globalThis.__qtUI.composerText()', context) or ''
                    if filled.get('send_enabled') and normalise(visible) == wanted:
                        break
                    time.sleep(.3)
                if normalise(visible) != wanted:
                    got = len(normalise(visible))
                    raise BrowserError('The Copilot message box did not accept the full text (' + str(got) + ' of ' + str(len(wanted)) +
                                       ' characters visible). Nothing was sent. Reduce the batch size in Advanced settings, then use Retry.', 'composer_mismatch')
                if not filled.get('send_enabled'):
                    raise BrowserError('The text is in the Copilot message box, but its Send button was not available. Nothing was sent.', 'no_send')
                task['transport_prompt'] = editor_text(visible)
                _cancelled(cancel)
                self._step('sending', 'Pressing Send in Copilot')
                cdp.evaluate('globalThis.__qtUI.sendButton.click(); true', context)
                submitted = True; task['submitted_at'] = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
                def sent(i):
                    if i.get('page_load_id') != load_id:
                        raise BrowserError('The Copilot page reloaded just after Send. The message may or may not have reached Copilot. Nothing was imported and it will not be resent automatically.', 'reloaded')
                    return i.get('composer_empty') and (i.get('user_turns', 0) >= 1 or i.get('reply_count', 0) >= 1 or i.get('stopping'))
                context, info = self._wait_for(cdp, sent, 25, cancel,
                                               'Send was pressed, but Copilot did not show the message as sent. Check the Copilot window. The app will not resend it automatically.')
                # Confirm Copilot actually started answering, and notice if the page silently cleared the message.
                self._step('waiting_reply', 'Waiting for Copilot to start its reply')
                start_deadline = time.monotonic() + 60; cleared_since = None
                while True:
                    _cancelled(cancel)
                    if time.monotonic() > start_deadline:
                        raise BrowserError('Copilot showed the message but did not start a reply within 60 seconds. Nothing was imported and nothing was resent. Check the Copilot window, then use Retry.', 'no_reply_started')
                    try:
                        context, info = self._inspect(cdp)
                    except BrowserError as exc:
                        if exc.code in {'cancelled', 'verification', 'usage_limit', 'browser_closed'}:
                            raise
                        time.sleep(.4); continue
                    self._attention(info)
                    if info.get('page_load_id') != load_id:
                        raise BrowserError('The Copilot page reloaded just after Send. The message may or may not have reached Copilot. Nothing was imported and it will not be resent automatically.', 'reloaded')
                    if info.get('stopping') or info.get('reply_count', 0) >= 1:
                        break
                    if info.get('user_turns', 0) == 0 and info.get('turn_count', 0) == 0:
                        cleared_since = cleared_since or time.monotonic()
                        if time.monotonic() - cleared_since > 3:
                            raise BrowserError('Copilot cleared the message before replying, so it was probably not processed. Nothing was imported and it was not resent automatically. Use Retry.', 'message_cleared')
                    else:
                        cleared_since = None
                    time.sleep(.4)
                self._step('waiting_reply', 'Waiting for Copilot to finish its reply')
                deadline = time.monotonic() + self.timeout; last = None; stable_since = time.monotonic(); cleared_since = None
                last_text = None; text_since = time.monotonic()
                while time.monotonic() < deadline:
                    _cancelled(cancel)
                    try:
                        context, info = self._inspect(cdp)
                    except BrowserError as exc:
                        if exc.code in {'cancelled', 'verification', 'usage_limit', 'browser_closed', 'reloaded'}:
                            raise
                        time.sleep(.5); continue
                    self._attention(info)
                    if info.get('page_load_id') != load_id:
                        raise BrowserError('The Copilot page reloaded while its reply was being written. Nothing was imported and nothing was resent. Use Retry.', 'reloaded')
                    if not info.get('turn_count') and not info.get('stopping'):
                        cleared_since = cleared_since or time.monotonic()
                        if time.monotonic() - cleared_since > 3:
                            raise BrowserError('The Copilot conversation was cleared before a complete reply arrived. Nothing was imported and nothing was resent. Use Retry.', 'message_cleared')
                    else:
                        cleared_since = None
                    text = cdp.evaluate('globalThis.__qtUI.lastReplyText()', context) or ''
                    if not text and not info.get('reply_count'):
                        text = '\n'.join(cdp.evaluate('globalThis.__qtUI.replyTexts()', context) or [])
                    raw = extract_receipt_json(text, receipt)
                    if text != last_text:
                        last_text = text; text_since = time.monotonic()
                    quiet = time.monotonic() - text_since
                    if not raw and text.strip() and not info.get('stopping') and (quiet >= self.decline_quiet_seconds or (info.get('reply_done') and quiet >= 3)):
                        body = re.sub(r'^\s*Copilot said:\s*', '', text).strip()
                        task['declined_reply'] = body[:2000]
                        if '{' not in body and len(body) < 600 and REFUSAL.search(body):
                            raise BrowserError('Copilot declined or could not answer this batch. Its reply was: "' + re.sub(r'\s+', ' ', body)[:220] +
                                               '" Nothing was imported and nothing was resent.', 'declined')
                        if '{' not in body:
                            raise BrowserError('Copilot answered in ordinary text instead of the structured format, so nothing was imported '
                                               '(its reply begins: "' + re.sub(r'\s+', ' ', body)[:120] + '"). Nothing was resent. '
                                               'Use Resume to try this batch again; if it repeats, reduce the batch size in Advanced settings.', 'no_json')
                        raise BrowserError('Copilot finished its reply, but it did not contain the complete structured result for this batch '
                                           '(the receipt or closing brackets are missing, for example because the reply was cut short). '
                                           'Nothing was imported. Reduce the batch size in Advanced settings, then use Retry.', 'no_receipt')
                    if raw and raw == last and not info.get('stopping'):
                        if info.get('reply_done') or time.monotonic() - stable_since >= 3:
                            self._step('validating', 'Checking the reply')
                            self.cached.update(open=True, ready=True, error_code='', message='Copilot reply received.', model_label=info.get('model_label', ''))
                            return raw, {'provider': 'copilot_browser', 'model': 'not reported by Copilot Chat',
                                         'copilot_model_selector_label': info.get('model_label') or 'not visible',
                                         'integration': 'visible_browser_ui', 'chat_isolation': isolation,
                                         'temporary_chat_notice': notice, 'receipt_verified': True,
                                         'browser': self.browser_version, 'submitted_at': task.get('submitted_at', ''),
                                         'completed_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())}
                    elif raw != last:
                        last = raw; stable_since = time.monotonic()
                    time.sleep(.5)
                raise BrowserError('Copilot did not return a complete reply with the expected receipt within ' + str(int(self.timeout)) +
                                   ' seconds. Its reply stays visible in the Copilot window. Nothing was imported and nothing was resent.', 'reply_timeout')
            except BrowserError as exc:
                if submitted and exc.code not in {'cancelled'}:
                    exc.code = exc.code or 'after_submit'
                self.cached.update(ready=self._alive(), error_code=exc.code, message=str(exc))
                raise
            finally:
                self._step('idle', '')
                if cdp:
                    cdp.close()

    # ----- closing --------------------------------------------------------------------------
    def close(self, forget=False):
        with self.lock:
            if not self.browser_ws:
                # The window may still be open from an earlier app session: reconnect so it can be closed.
                try:
                    # Look for an earlier window without creating the profile folder: an app that
                    # never used Copilot must not leave a browser profile behind when it exits.
                    existing = self.profile_dir if self.profile_dir is not None else default_profile_dir()
                    profile = self._profile() if str(existing) == 'temporary' or Path(existing).is_dir() else None
                    portfile = profile / 'DevToolsActivePort' if profile else None
                    if portfile and portfile.exists():
                        self._acquire(profile)
                        port, ws = self._read_portfile(portfile); self._attach(port, ws)
                except (OSError, ValueError, IndexError, BrowserError):
                    pass   # nothing open to reconnect to (or another copy holds it)
            if self.browser_ws:
                try:
                    self._browser_call('Browser.close', timeout=3)
                except (BrowserError, OSError):
                    pass
            if self.process:
                try:
                    self.process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    self.process.terminate()
                except OSError:
                    pass
            self._forget_connection()
            if self._temp_profile:
                try:
                    self._temp_profile.cleanup()
                except OSError:
                    pass
                self._temp_profile = None
            message = 'Copilot window closed. Your sign-in is kept in the dedicated QualiTranslate browser profile.'
            if forget:
                profile = self.profile_dir or default_profile_dir()
                portfile = profile / 'DevToolsActivePort'
                still_open = False
                if portfile.exists():
                    try:
                        port, _ = self._read_portfile(portfile)
                    except (OSError, ValueError, IndexError):
                        port = None
                    deadline = time.monotonic() + 6   # Chrome needs a moment to shut down
                    while port and time.monotonic() < deadline:
                        try:
                            self._http('/json/version', port); still_open = True; time.sleep(.3)
                        except BrowserError:
                            still_open = False; break
                if still_open:
                    self._release()
                    self.cached.update(open=False, ready=False, error_code='in_use',
                                       message='The Copilot window could not be closed, so the saved sign-in was NOT removed. Quit that Chrome window (Chrome > Quit), then use Forget again.')
                    return
                if profile.exists() and profile.name == 'copilot-browser-profile':
                    time.sleep(.5)
                    shutil.rmtree(profile, ignore_errors=True)
                message = 'Copilot window closed and the saved Copilot sign-in was removed from this computer. Copilot conversation history in Microsoft 365 is not affected.'
            self._release()
            self.cached.update(open=False, ready=False, error_code='', message=message)


class CopilotConnection:
    """Non-blocking connection watcher. Checks the page only; never sends a prompt."""

    def __init__(self, browser, timeout=600, interval=2):
        self.browser = browser; self.timeout = timeout; self.interval = interval
        self.lock = threading.Lock(); self.thread = None; self.cancel_event = threading.Event()
        self.phase = 'idle'; self.message = ''; self.last_check = ''; self.started = 0; self.waiting_for = ''

    def summary(self):
        result = self.browser.summary()
        with self.lock:
            connecting = self.phase in {'opening', 'waiting'}
            result.update(connecting=connecting, connection_phase=self.phase, waiting_for=self.waiting_for,
                          connection_elapsed=int(time.monotonic() - self.started) if connecting else 0,
                          connection_timeout=int(self.timeout), last_check=self.last_check)
            if self.phase in {'opening', 'waiting', 'cancelled', 'timeout', 'error'}:
                result.update(ready=False, message=self.message)
            return result

    def start(self):
        with self.lock:
            if self.thread and self.thread.is_alive():
                if self.cancel_event.is_set():
                    raise BrowserError('The previous connection check is stopping. Please retry in a few seconds.')
            else:
                self.cancel_event = threading.Event(); self.started = time.monotonic()
                self.phase = 'opening'; self.last_check = ''; self.waiting_for = 'browser'
                self.message = 'Opening the Copilot window…'
                self.thread = threading.Thread(target=self._watch, args=(self.cancel_event,), daemon=True)
                self.thread.start()
        return self.summary()

    def cancel(self):
        with self.lock:
            self.cancel_event.set(); self.phase = 'cancelled'
            self.message = 'Connection cancelled. Nothing was sent to Copilot. You can retry or save your project.'
        return self.summary()

    def _watch(self, cancel):
        try:
            self.browser.open()
            while not cancel.is_set():
                info = self.browser.check()
                with self.lock:
                    if cancel.is_set():
                        return
                    self.last_check = info.get('message', '')
                    if info.get('ready'):
                        self.phase = 'ready'; self.message = ''; self.waiting_for = ''; return
                    code = info.get('error_code')
                    if code in {'verification', 'usage_limit', 'browser_closed', 'no_browser', 'no_control', 'in_use'}:
                        self.phase = 'error'; self.message = 'Connection needs your attention. Nothing was sent. ' + self.last_check; return
                    if time.monotonic() - self.started >= self.timeout:
                        self.phase = 'timeout'
                        self.message = ('Still not connected after ' + str(int(self.timeout // 60)) + ' minutes. Nothing was sent. Last check: ' + self.last_check +
                                        ' Click Retry when the Copilot window shows the chat page.')
                        return
                    self.phase = 'waiting'
                    self.waiting_for = {'sign_in': 'sign_in', 'other_page': 'sign_in', 'loading': 'loading'}.get(code, 'controls')
                    self.message = self.last_check
                if cancel.wait(self.interval):
                    return
        except Exception as exc:
            with self.lock:
                if cancel.is_set():
                    return
                self.phase = 'error'
                detail = str(exc) if isinstance(exc, (BrowserError, OSError)) else 'The browser connection could not be completed.'
                self.message = 'Connection stopped. Nothing was sent. ' + detail
