#!/usr/bin/env python3
"""Check that a running QualiTranslate server (Python or R) behaves the same way.

Usage:  python3 tools/api_conformance.py http://127.0.0.1:8765 [--save-project out.json] [--import-project in.json]

It uses only fictional text and the offline demonstration; nothing is sent to any AI service.
--save-project writes the reviewed demo project, so a project made by one implementation can be
imported by the other with --import-project.
"""
import base64
import io
import json
import re
import sys
import time
import urllib.error
import urllib.request
import zipfile

OPENER = urllib.request.build_opener(urllib.request.ProxyHandler({}))
FAILURES = []


def check(condition, label):
    print(("ok   " if condition else "FAIL ") + label)
    if not condition:
        FAILURES.append(label)


def request(base, path, body=None, token="", headers=None, raw=False):
    data = json.dumps(body).encode() if body is not None else None
    h = {"X-QualiTranslate-Token": token} if token else {}
    if data is not None:
        h["Content-Type"] = "application/json"
    h.update(headers or {})
    req = urllib.request.Request(base + path, data=data, headers=h, method="POST" if data is not None else "GET")
    try:
        with OPENER.open(req, timeout=30) as r:
            payload = r.read()
            return r.status, (payload if raw else json.loads(payload or b"{}")), r.headers
    except urllib.error.HTTPError as e:
        payload = e.read()
        try:
            return e.code, json.loads(payload or b"{}"), e.headers
        except ValueError:
            return e.code, {}, e.headers


def tiny_docx(paragraphs):
    body = "".join("<w:p><w:r><w:t xml:space=\"preserve\">" + p.replace("&", "&amp;").replace("<", "&lt;") + "</w:t></w:r></w:p>" for p in paragraphs)
    stream = io.BytesIO()
    with zipfile.ZipFile(stream, "w") as z:
        z.writestr("[Content_Types].xml", '<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="xml" ContentType="application/xml"/></Types>')
        z.writestr("word/document.xml", '<?xml version="1.0" encoding="UTF-8"?><w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>' + body + "</w:body></w:document>")
    return base64.b64encode(stream.getvalue()).decode()


def wait(base, token, seconds=30):
    end = time.time() + seconds
    while time.time() < end:
        _, state, _ = request(base, "/api/state", token=token)
        if not state["job"]["running"]:
            return state
        time.sleep(0.3)
    raise SystemExit("job did not finish")


def main(argv):
    base = argv[1].rstrip("/")
    save = argv[argv.index("--save-project") + 1] if "--save-project" in argv else None
    load = argv[argv.index("--import-project") + 1] if "--import-project" in argv else None
    status, html, headers = request(base, "/", raw=True)
    html = html.decode()
    token = re.search(r'name="csrf-token" content="([^"]+)"', html).group(1)
    check(status == 200 and len(token) >= 32, "index page carries a session token")
    check("default-src 'self'" in headers.get("Content-Security-Policy", "") and headers.get("Cache-Control") == "no-store", "security headers on the page")
    check(request(base, "/health")[1].get("version") == "1.5.0", "health reports version 1.5.0")
    check(request(base, "/api/state")[0] in (403, 404), "state needs the token")
    check(request(base, "/api/demo", {}, token, {"Origin": "https://evil.example"})[0] == 403, "foreign origin is rejected")
    check(request(base, "/api/state", token=token, headers={"Host": "evil.example"})[0] in (403, 404), "foreign Host header is rejected")
    for asset in ("/assets/app.js", "/assets/style.css"):
        check(request(base, asset, raw=True)[0] == 200, "serves " + asset)
    check(request(base, "/assets/../app.py", raw=True)[0] == 404, "no path traversal")

    # Demonstration: all three stages offline, then review and exports.
    check(request(base, "/api/demo", {}, token)[0] == 200, "load demo")
    check(request(base, "/api/run", {"stage": "all", "demo": True}, token)[0] == 200, "start demo run")
    state = wait(base, token)
    segs = state["project"]["segments"]
    check(len(segs) == 6 and all(s["reconcile"] for s in segs), "demo completes all three stages")
    check([i["severity"] for i in segs[3]["reconcile"]["issues"]] == ["critical"], "demo S0004 has a critical negation flag")
    check(state["project"]["run_count"] == 3 and "runs" not in state["project"], "state omits run history and counts runs")
    check(state.get("identifier_check", {}).get("total") == 0, "identifier check present and empty for the demo")
    check(set(state["credentials"]) == {"mistral", "anthropic"} and not any(state["credentials"].values()), "credential flags only")
    st, body, _ = request(base, "/api/review", {"id": "S0004", "text": "It was not only the pain.", "reviewer": "Fictional reviewer", "note": "Restored negation", "uncertainty": False}, token)
    check(st == 200, "record an individual review")
    st, body, _ = request(base, "/api/review", {"id": "S0005", "text": segs[4]["reconcile"]["text"], "reviewer": "R", "note": "", "method": "quick_accept"}, token)
    check(st == 400 and "Quick approval" in body.get("error", ""), "quick approval refused for a major flag")
    st, body, _ = request(base, "/api/methods", {}, token)
    check(st == 200 and body["paragraph"].startswith("[SYNTHETIC DEMONSTRATION") and "1 after individual review" in body["paragraph"], "methods paragraph")
    st, data, h = request(base, "/api/export", {"kind": "zip"}, token, raw=True)
    names = sorted(zipfile.ZipFile(io.BytesIO(data)).namelist()) if st == 200 else []
    check(names == ["audit.json", "methods_record.txt", "parallel_review.html", "project.json", "prompts_and_runs.json", "translations.csv"], "six-file research bundle")
    if names:
        z = zipfile.ZipFile(io.BytesIO(data))
        record = z.read("methods_record.txt").decode()
        check(record.startswith("DRAFT METHODS PARAGRAPH") and "Local identifier check" in record, "methods record has paragraph and identifier line")
        check(z.read("translations.csv").startswith(b"\xef\xbb\xbfid,speaker,source"), "CSV has BOM and header")
        project = json.loads(z.read("project.json"))
        check(project["format"] == "qualitranslate-project" and project["segments"][3]["review"]["reviewer"] == "Fictional reviewer", "project.json holds the review")
        if save:
            open(save, "w", encoding="utf-8").write(json.dumps(project, ensure_ascii=False))
    st, data, h = request(base, "/api/export", {"kind": "csv"}, token, raw=True)
    check(st == 200 and 'filename="qualitranslate-translations.csv"' in h.get("Content-Disposition", ""), "CSV download name")

    # Settings validation and model options.
    settings = state["project"]["settings"]
    bad = json.loads(json.dumps(settings)); bad["target_language"] = "danish"
    check(request(base, "/api/settings", {"settings": bad}, token)[0] == 400, "same source and target language rejected")
    good = json.loads(json.dumps(settings))
    for stage in good["stages"].values():
        stage.update(provider="anthropic", model="claude-x", temperature=0.2, effort="high")
    good["stages"]["back"].update(provider="mistral", model="mistral-y", temperature=None, top_p=0.9, effort="none", random_seed=7)
    st, body, _ = request(base, "/api/settings", {"settings": good}, token)
    check(st == 200, "Claude temperature + effort and Mistral top-p + reasoning effort accepted " + str(body))
    state = request(base, "/api/state", token=token)[1]
    check(state["project"]["settings"]["stages"]["back"]["effort"] == "none" and state["project"]["settings"]["stages"]["forward"]["temperature"] == 0.2, "model options stored")
    check(all(s["forward"] is None for s in state["project"]["segments"]), "a model change clears dependent work")
    bad = json.loads(json.dumps(good)); bad["stages"]["forward"]["effort"] = "none"
    check(request(base, "/api/settings", {"settings": bad}, token)[0] == 400, "effort level checked per provider")
    bad = json.loads(json.dumps(good)); bad["stages"]["forward"]["random_seed"] = 3
    check(request(base, "/api/settings", {"settings": bad}, token)[0] == 400, "no seed for Claude")

    # Import with identifiers, manual copy/paste round.
    st, _, _ = request(base, "/api/import", {"text": "P: Ring til mig på 12345678\nI: Tak", "title": "Fictional"}, token)
    state = request(base, "/api/state", token=token)[1]
    check(st == 200 and state["identifier_check"]["segments"] == [{"id": "S0001", "kinds": ["phone"]}], "identifier warning for a phone number")
    manual = json.loads(json.dumps(state["project"]["settings"]))
    for stage in manual["stages"].values():
        stage.update(provider="manual", model="", temperature=None, top_p=None, random_seed=None, effort="", chat_tool="Other", chat_model="")
    check(request(base, "/api/settings", {"settings": manual}, token)[0] == 200, "switch to manual mode")
    st, task, _ = request(base, "/api/prompt", {"stage": "forward"}, token)
    check(st == 200 and "INPUT JSON:" in task.get("copy_text", "") and task["ids"] == ["S0001", "S0002"], "manual prompt packet")
    reply = json.dumps({"segments": [{"id": "S0001", "text": "Call me on 12345678", "notes": []}, {"id": "S0002", "text": "Thanks", "notes": []}]})
    check(request(base, "/api/manual", {"task_id": task["task_id"], "response": "```json\n" + reply + "\n```", "model_label": "Other / test"}, token)[0] == 200, "manual reply applied")
    st, task2, _ = request(base, "/api/prompt", {"stage": "back"}, token)
    check(st == 200 and "Ring til mig" not in task2["user"], "blind back-translation packet excludes the original")
    st, body, _ = request(base, "/api/manual", {"task_id": task2["task_id"], "response": json.dumps({"segments": [{"id": "S0002", "text": "x"}]}), "model_label": "x"}, token)
    check(st == 400 and "Segment ids" in body.get("error", ""), "wrong ids rejected")

    # Documents.
    st, doc, _ = request(base, "/api/file", {"filename": "fictional.docx", "data": tiny_docx(["Interviewer: Hello", "Participant: Hej & farvel"])}, token)
    check(st == 200 and "Participant: Hej & farvel" in doc.get("text", ""), "reads a .docx")
    st, insp, _ = request(base, "/api/inspect", {"text": doc.get("text", "x"), "blocks": doc.get("blocks")}, token)
    check(st == 200 and insp.get("blocks") and insp.get("inspection_id"), "inspects a document")
    bad_docx = base64.b64encode(b"not a zip").decode()
    check(request(base, "/api/file", {"filename": "x.docx", "data": bad_docx}, token)[0] == 400, "rejects a broken .docx")

    # Project round trip (optionally from the other implementation).
    if load:
        project = json.load(open(load, encoding="utf-8"))
        st, body, _ = request(base, "/api/project", {"project": project}, token)
        state = request(base, "/api/state", token=token)[1]
        check(st == 200 and state["project"]["segments"][3]["review"]["reviewer"] == "Fictional reviewer", "imports a project saved by the other implementation " + str(body))
        st, body, _ = request(base, "/api/methods", {}, token)
        check("1 after individual review" in body.get("paragraph", ""), "imported project keeps its review history")
    check(request(base, "/api/reset", {}, token)[0] == 200, "reset")
    print("\n" + ("ALL CHECKS PASSED" if not FAILURES else str(len(FAILURES)) + " CHECK(S) FAILED"))
    return 1 if FAILURES else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
