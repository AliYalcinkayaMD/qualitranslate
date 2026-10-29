#!/usr/bin/env python3
"""Start the local QualiTranslate workspace: python3 app.py"""
from __future__ import annotations

import argparse
import json
import secrets
import threading
import webbrowser
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

from .core import (APP_VERSION, DEMO_SOURCE, WorkflowError, Workspace, export_bundle,
                  export_csv, export_html, methods_paragraph, read_document, read_upload)

ROOT = Path(__file__).resolve().parent
MAX_REQUEST = 25_000_000


def make_handler(workspace, token):
    class Handler(BaseHTTPRequestHandler):
        server_version = "QualiTranslate/" + APP_VERSION

        def log_message(self, *args):
            # The standard logger would print request URLs and project activity.
            pass

        def _host_ok(self):
            return self.headers.get("Host") in {"127.0.0.1:" + str(self.server.server_port), "localhost:" + str(self.server.server_port)}

        def _authorized(self):
            origin = self.headers.get("Origin")
            allowed = {"http://127.0.0.1:" + str(self.server.server_port), "http://localhost:" + str(self.server.server_port)}
            supplied = self.headers.get("X-QualiTranslate-Token", "")
            # Compare as bytes: compare_digest refuses non-ASCII str, which would drop the connection.
            return self._host_ok() and (origin is None or origin in allowed) and secrets.compare_digest(supplied.encode("utf-8", "surrogateescape"), token.encode("utf-8"))

        def _send(self, data, content_type="application/json; charset=utf-8", status=200, filename=None):
            if not isinstance(data, bytes):
                data = json.dumps(data, ensure_ascii=False).encode()
            self.send_response(status)
            self.send_header("Content-Type", content_type)
            self.send_header("Content-Length", str(len(data)))
            self.send_header("Cache-Control", "no-store")
            self.send_header("X-Content-Type-Options", "nosniff")
            self.send_header("Referrer-Policy", "no-referrer")
            self.send_header("X-Frame-Options", "DENY")
            self.send_header("Content-Security-Policy", "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'self'")
            if filename:
                self.send_header("Content-Disposition", 'attachment; filename="' + filename + '"')
            self.end_headers()
            self.wfile.write(data)

        def do_GET(self):
            if not self._host_ok():
                self._send({"error": "Host rejected."}, status=403)
                return
            if self.path in {"/", "/index.html"}:
                data = (ROOT / "web" / "index.html").read_text(encoding="utf-8").replace("__CSRF_TOKEN__", token)
                self._send(data.encode(), "text/html; charset=utf-8")
            elif self.path in {"/assets/app.js", "/assets/style.css"}:
                filename = self.path.rsplit("/", 1)[1]
                mime = "text/javascript; charset=utf-8" if filename.endswith(".js") else "text/css; charset=utf-8"
                self._send((ROOT / "web" / filename).read_bytes(), mime)
            elif self.path == "/api/state" and self._authorized():
                state = workspace.snapshot()
                # Exact prompt history is exported separately, not resent on each UI poll.
                runs = state["project"].pop("runs")
                state["project"]["run_count"] = len(runs)
                state["project"]["last_manual_label"] = next((r.get("provenance", {}).get("tool_model_reported_by_user", "") for r in reversed(runs) if r.get("provenance", {}).get("provider") == "manual"), "")
                state["project"]["audit_count"] = len(state["project"].pop("audit"))
                self._send(state)
            elif self.path == "/health":
                self._send({"app": "QualiTranslate", "version": APP_VERSION})
            else:
                self._send({"error": "Not found or access denied."}, status=404)

        def do_POST(self):
            if not self._authorized():
                self._send({"error": "Request rejected. Open the app from its local address."}, status=403)
                return
            try:
                length = int(self.headers.get("Content-Length", "0"))
                if not 0 < length <= MAX_REQUEST:
                    self._send({"error": "Request is empty or exceeds 25 MB."}, status=413)
                    return
                if self.headers.get_content_type() != "application/json":
                    raise WorkflowError("Requests must use application/json.")
                body = json.loads(self.rfile.read(length))
                if not isinstance(body, dict):
                    raise WorkflowError("Request must be a JSON object.")
                route = self.path
                if route == "/api/import":
                    workspace.load_text(body.get("text", ""), body.get("title", "Untitled interview"), split_speakers=body.get("split_speakers", True) is True)
                elif route == "/api/file":
                    self._send(read_document(body.get("filename", ""), body.get("data", "")))
                    return
                elif route == "/api/inspect":
                    self._send(workspace.inspect_document(body.get("text", ""), body.get("blocks")))
                    return
                elif route == "/api/import-selected":
                    workspace.import_selected(body.get("inspection_id"), body.get("included_ids"), body.get("title", "Untitled interview"), body.get("settings"), body.get("split_speakers", True) is True)
                elif route == "/api/demo":
                    workspace.load_text(DEMO_SOURCE, "Synthetic sample · treatment success", demo=True)
                elif route == "/api/settings":
                    workspace.save_settings(body.get("settings"))
                elif route == "/api/key":
                    workspace.set_key(body.get("provider"), body.get("key", ""))
                elif route == "/api/models":
                    provider = body.get("provider")
                    with workspace.lock:
                        key = workspace.keys.get(provider, "")
                    self._send({"models": workspace.client.models(provider, key, body.get("workspace_id", ""))})
                    return
                elif route == "/api/prompt":
                    task = workspace.prepare_task(body.get("stage"))
                    task["copy_text"] = task["system"] + "\n\nINPUT JSON:\n" + task["user"]
                    self._send(task)
                    return
                elif route == "/api/manual":
                    workspace.apply_manual(body.get("task_id"), body.get("response", ""), body.get("model_label", ""))
                elif route == "/api/run":
                    workspace.start_job(body.get("stage"), demo=body.get("demo") is True,
                                        expected_project_id=body.get('expected_project_id'),expected_revision=body.get('expected_revision'))
                elif route == "/api/methods":
                    self._send({"paragraph": methods_paragraph(workspace.snapshot()["project"])})
                    return
                elif route == "/api/stop":
                    workspace.cancel.set()
                elif route == "/api/edit":
                    workspace.edit(body.get("id"), body.get("stage"), body.get("text", ""), body.get("note", ""))
                elif route == "/api/review":
                    workspace.review(body.get("id"), body.get("text", ""), body.get("reviewer", ""), body.get("note", ""), body.get("uncertainty") is True, body.get("method", "individual"))
                elif route == "/api/review-batch":
                    workspace.review_batch_minor(body.get("ids"), body.get("reviewer", ""), body.get("revision"))
                elif route == "/api/review-clear":
                    workspace.clear_review(body.get("id"))
                elif route == "/api/project":
                    workspace.import_project(body.get("project"))
                elif route == "/api/reset":
                    workspace.clear()
                elif route == "/api/export":
                    project = workspace.snapshot()["project"]
                    kind = body.get("kind", "zip")
                    if kind == "json":
                        data, mime, name = json.dumps(project, ensure_ascii=False, indent=2).encode(), "application/json", "qualitranslate-project.json"
                    elif kind == "csv":
                        data, mime, name = export_csv(project), "text/csv; charset=utf-8", "qualitranslate-translations.csv"
                    elif kind == "html":
                        data, mime, name = export_html(project), "text/html; charset=utf-8", "qualitranslate-parallel-review.html"
                    elif kind == "zip":
                        data, mime, name = export_bundle(project), "application/zip", "qualitranslate-research-bundle.zip"
                    else:
                        raise WorkflowError("Unknown export format.")
                    self._send(data, mime, filename=name)
                    return
                else:
                    self._send({"error": "Unknown endpoint."}, status=404)
                    return
                self._send({"ok": True})
            except (WorkflowError, json.JSONDecodeError, TypeError, ValueError, AttributeError) as exc:
                message = str(exc) if isinstance(exc, WorkflowError) else "Invalid request format."
                self._send({"error": message}, status=400)
            except Exception:
                self._send({"error": "An internal error occurred. Completed work remains in memory; export your project before restarting."}, status=500)

        def do_OPTIONS(self):
            self._send({"error": "Cross-origin access is disabled."}, status=403)

    return Handler


def running_version(port):
    """Return the version of a QualiTranslate already serving this port, if any."""
    try:
        import urllib.request
        opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
        with opener.open("http://127.0.0.1:" + str(port) + "/health", timeout=1.5) as response:
            data = json.loads(response.read(2000))
        return str(data.get("version", "unknown")) if data.get("app") == "QualiTranslate" else ""
    except Exception:
        return ""


def main(argv=None):
    parser = argparse.ArgumentParser(description="Local qualitative translation workspace")
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--no-browser", action="store_true")
    args = parser.parse_args(argv)
    if not 0 <= args.port <= 65535:
        parser.error("Port must be between 0 and 65535.")
    workspace, token = Workspace(), secrets.token_urlsafe(32)
    server = None
    for port in [args.port] + ([p for p in range(args.port + 1, args.port + 11)] if args.port else []):
        try:
            server = ThreadingHTTPServer(("127.0.0.1", port), make_handler(workspace, token))
            break
        except OSError:
            other = running_version(port)
            if other:
                print("Note: QualiTranslate " + other + " is already running at http://127.0.0.1:" + str(port) +
                      " (probably in another Terminal window). This new copy will use a different address." +
                      ("" if other == APP_VERSION else " Close the older window when you no longer need it, so you do not use the old version by mistake."))
    if server is None:
        print("No free local port was found. Close other QualiTranslate Terminal windows and try again.")
        return 1
    url = "http://127.0.0.1:" + str(server.server_port)
    print("QualiTranslate " + APP_VERSION + "\nOpen " + url + "\nKeep this window open. Press Control+C to stop.\nProject data stays in memory until you explicitly export it. API mode sends selected text to the chosen provider.")
    if not args.no_browser:
        threading.Timer(0.7, lambda: webbrowser.open(url)).start()
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nStopping. In-memory project and credentials will be cleared.")
    finally:
        workspace.cancel.set()
        workspace.keys.clear()
        server.server_close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
