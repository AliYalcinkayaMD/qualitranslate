#!/usr/bin/env python3
"""Copilot page diagnostic for maintainers (not needed for normal use).

Run with:  python -m qualitranslate.diagnostic

Opens (or re-uses) the dedicated QualiTranslate Copilot window, waits until you are
signed in, and saves copilot_diagnostic.json in the current folder. The report holds page
STRUCTURE only: host/path, control labels, element roles and test ids, counts and the
app's own readiness checks. It does not read cookies, tokens, storage or network
traffic, does not type or send anything, and skips sign-in pages entirely.
Run it with fictional or empty chats on screen; chat text is not collected, but
button labels from the visible page are.
"""
import json
import platform
import sys
import time
import urllib.parse
from pathlib import Path

from . import copilot_browser as cb

STRUCTURE_JS = r"""
(()=>{
  const vis=e=>{try{return !!(e&&e.getClientRects().length)&&getComputedStyle(e).visibility!=='hidden'}catch{return false}};
  const roots=[];(function d(r){roots.push(r);for(const el of r.querySelectorAll('*'))if(el.shadowRoot)d(el.shadowRoot)})(document);
  const all=s=>roots.flatMap(r=>[...r.querySelectorAll(s)]);
  const cut=(s,n)=>(s||'').replace(/[​-‏⁠﻿]/g,'').replace(/\s+/g,' ').trim().slice(0,n||40);
  const personal=/account|@|profile|sign out|signed in as/i;
  const inMsg=e=>!!e.closest('[data-testid="m365-chat-llm-web-ui-chat-message"],[data-testid="chatQuestion"],[data-testid="copilot-message-reply-div"]');
  const label=e=>cut(e.getAttribute('aria-label')||e.getAttribute('title')||'',40);
  const editors=all('textarea,[contenteditable],[role="textbox"]').filter(e=>!inMsg(e)).slice(0,10).map(e=>({tag:e.tagName.toLowerCase(),id:cut(e.id),role:e.getAttribute('role'),ce:e.getAttribute('contenteditable'),aria:label(e),visible:vis(e)}));
  const buttons=all('button,[role="button"]').filter(e=>vis(e)&&!e.closest('nav,aside,[role="navigation"]')&&!inMsg(e)).map(e=>({label:label(e)||cut(e.innerText,25),type:e.getAttribute('type'),pressed:e.getAttribute('aria-pressed'),testid:e.getAttribute('data-testid'),id:cut(e.id,30)})).filter(b=>!personal.test(b.label)).slice(0,60);
  const testids={};for(const e of all('[data-testid]')){const t=e.getAttribute('data-testid').slice(0,60);testids[t]=(testids[t]||0)+1}
  return {where:location.origin+location.pathname.slice(0,60),lang:document.documentElement.lang,ready:document.readyState,
    headings:all('h1,h2,[role="heading"]').filter(vis).map(e=>cut(e.innerText,50)).filter(h=>!personal.test(h)).slice(0,6),
    editors,buttons,testids:Object.entries(testids).sort((a,b)=>b[1]-a[1]).slice(0,80),
    iframes:all('iframe').map(f=>{try{const u=new URL(f.src,location.href);return u.origin+u.pathname.slice(0,40)}catch{return ''}}).slice(0,10)};
})()
"""


def main():
    out = Path.cwd() / 'copilot_diagnostic.json'
    report = {'created': time.strftime('%Y-%m-%dT%H:%M:%S'), 'python': sys.version.split()[0], 'macos': platform.mac_ver()[0],
              'machine': platform.machine(), 'steps': []}
    browser = cb.CopilotBrowser(exclusive=False)   # read-only check; may run beside the app
    def step(name, **data):
        report['steps'].append({'at': time.strftime('%H:%M:%S'), 'step': name, **data})
        out.write_text(json.dumps(report, indent=2, ensure_ascii=False), encoding='utf-8')
        print(time.strftime('%H:%M:%S'), name, data.get('message', ''), flush=True)
    try:
        browser.open()
        step('browser_open', browser=browser.browser_version, message='Window open. Sign in there if asked.')
    except cb.BrowserError as exc:
        step('browser_failed', code=exc.code, message=str(exc)); return 1
    deadline = time.monotonic() + 600; last = None
    while time.monotonic() < deadline:
        info = browser.check()
        if info.get('message') != last:
            step('check', ready=info.get('ready'), code=info.get('error_code'), page_host=info.get('page_host'), message=info.get('message'))
            last = info.get('message')
        if info.get('ready') or info.get('error_code') in {'verification', 'usage_limit', 'browser_closed', 'no_browser'}:
            break
        time.sleep(2)
    cdp = None
    try:
        cdp = browser._connect_page(deadline=time.monotonic() + 20)
        tree = cdp.call('Page.getFrameTree').get('frameTree', {})
        frames = []
        def walk(node):
            f = node.get('frame', {}); frames.append(f)
            for c in node.get('childFrames', []): walk(c)
        walk(tree)
        for f in frames:
            if not browser._allowed(f.get('url', '')):
                frames_entry = {'frame': urllib.parse.urlsplit(f.get('url', '')).hostname, 'inspected': False}
                report.setdefault('frames', []).append(frames_entry); continue
            ctx = cdp.call('Page.createIsolatedWorld', {'frameId': f['id'], 'worldName': 'QualiTranslateDiagnostic', 'grantUniveralAccess': False}).get('executionContextId')
            entry = {'frame': f.get('url', '').split('?')[0][:100], 'inspected': True,
                     'app_adapter': cdp.evaluate(cb.DOM_SCRIPT, ctx), 'structure': cdp.evaluate(STRUCTURE_JS, ctx)}
            report.setdefault('frames', []).append(entry)
        step('inspected', message='Saved ' + out.name + '. You can close this window; the Copilot window stays open.')
    except cb.BrowserError as exc:
        step('inspect_failed', code=exc.code, message=str(exc))
    finally:
        if cdp:
            cdp.close()
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
