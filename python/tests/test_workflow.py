import base64
import io
import json
import sys
import threading
import time
import unittest
import urllib.error
import urllib.request
import zipfile
from http.server import ThreadingHTTPServer
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'src'))
from qualitranslate.app import make_handler
from qualitranslate.core import (APIClient, DEMO_SOURCE, WorkflowError, Workspace, build_task,
                  defaults, digest, export_bundle, export_csv, export_html,
                  make_batches, parse_response, parse_transcript, read_upload)


def wait_for_job(ws, seconds=5):
    deadline = time.monotonic() + seconds
    while ws.job["running"] and time.monotonic() < deadline:
        time.sleep(0.01)
    if ws.job["running"]:
        raise AssertionError("Test job did not finish")


class FakeClient:
    def __init__(self):
        self.requests = []
        self.fail_once = False

    def complete(self, task, key):
        self.requests.append(json.loads(json.dumps(task)))
        if self.fail_once and len(self.requests) == 2:
            self.fail_once = False
            raise WorkflowError("Simulated provider failure")
        data = json.loads(task["user"])
        rows = []
        for s in data["segments"]:
            if task["stage"] == "reconcile":
                rows.append({"id": s["id"], "text": s["forward_translation"], "issues": [], "rationale": "Checked in fixture", "needs_human": False})
            else:
                rows.append({"id": s["id"], "text": "fixture " + s["text"], "notes": []})
        return json.dumps({"segments": rows}), {"provider": "fake", "returned_model": "test-model", "usage": {"input_tokens": 10}}


class PipelineTests(unittest.TestCase):
    def setUp(self):
        self.ws = Workspace(FakeClient())
        self.ws.load_text("Interviewer: SOURCE_SENTINEL_91\nParticipant A: Pain mattered to me.", "Test")

    def api_config(self, batch_segments=12):
        settings = defaults()
        settings["batch_segments"] = batch_segments
        settings["study_context"] = "CONTEXT_SENTINEL_92"
        settings["glossary"] = "GLOSSARY_SENTINEL_93"
        settings["prompt_additions"]["forward"] = "FORWARD_ADDITION_SENTINEL_94"
        for stage in settings["stages"]:
            settings["stages"][stage] = {"provider": "mistral", "model": "fixture-model", "workspace_id": ""}
        self.ws.save_settings(settings)
        self.ws.set_key("mistral", "NEVER_EXPORT_THIS_KEY")

    def manual_forward(self):
        task = self.ws.prepare_task("forward")
        data = {"segments": [{"id": sid, "text": "TARGET_SENTINEL_" + sid, "notes": ["FORWARD_NOTE_SENTINEL_95"]} for sid in task["ids"]]}
        self.ws.apply_manual(task["task_id"], json.dumps(data), "Institutional interface / version not reported")

    def test_blind_request_contains_no_original_context_or_notes(self):
        self.api_config()
        self.manual_forward()
        task = self.ws.prepare_task("back")
        request = task["system"] + task["user"]
        for secret in ["SOURCE_SENTINEL_91", "CONTEXT_SENTINEL_92", "GLOSSARY_SENTINEL_93", "FORWARD_ADDITION_SENTINEL_94", "FORWARD_NOTE_SENTINEL_95", "Pain mattered", "Participant A", "NEVER_EXPORT_THIS_KEY"]:
            self.assertNotIn(secret, request)
        self.assertIn("TARGET_SENTINEL_S0001", request)

    def test_back_rejects_custom_source_context(self):
        settings = defaults()
        settings["prompt_additions"]["back"] = "Here is the original interview"
        with self.assertRaises(WorkflowError):
            self.ws.save_settings(settings)

    def test_back_requires_all_forward_segments(self):
        with self.assertRaises(WorkflowError):
            self.ws.prepare_task("back")

    def test_reconcile_requires_back(self):
        self.manual_forward()
        with self.assertRaises(WorkflowError):
            self.ws.prepare_task("reconcile")

    def test_complete_pipeline_uses_separate_requests(self):
        self.api_config()
        self.ws.start_job("all")
        wait_for_job(self.ws)
        self.assertFalse(self.ws.job["error"])
        self.assertEqual([r["stage"] for r in self.ws.client.requests], ["forward", "back", "reconcile"])
        for segment in self.ws.project["segments"]:
            self.assertTrue(segment["reconcile"]["needs_human"])
            self.assertIsNone(segment["review"])
        payload = json.loads(self.ws.client.requests[-1]["user"])
        self.assertEqual(payload["segments"][0]["original_source"], "SOURCE_SENTINEL_91")
        self.assertIn("blind_back_translation", payload["segments"][0])

    def test_partial_run_resumes_without_repeating_completed_batches(self):
        self.api_config(batch_segments=1)
        self.ws.client.fail_once = True
        self.ws.start_job("forward")
        wait_for_job(self.ws)
        self.assertIn("Simulated", self.ws.job["error"])
        self.assertTrue(self.ws.project["segments"][0]["forward"])
        self.assertIsNone(self.ws.project["segments"][1]["forward"])
        first_ids = self.ws.client.requests[0]["ids"]
        self.ws.start_job("forward")
        wait_for_job(self.ws)
        self.assertNotEqual(self.ws.client.requests[-1]["ids"], first_ids)
        self.assertTrue(all(s["forward"] for s in self.ws.project["segments"]))

    def test_pending_prompt_cannot_apply_after_source_changes(self):
        task = self.ws.prepare_task("forward")
        self.ws.edit("S0001", "source", "A different source")
        with self.assertRaises(WorkflowError):
            self.ws.apply_manual(task["task_id"], '{"segments":[]}', "Manual")

    def test_forward_edit_invalidates_all_downstream_context(self):
        self.api_config()
        self.ws.start_job("all")
        wait_for_job(self.ws)
        self.ws.review("S0002", "Reviewed", "Reviewer", "")
        self.ws.edit("S0001", "forward", "Changed translation")
        self.assertTrue(all(s["forward"] for s in self.ws.project["segments"]))
        self.assertTrue(all(s["back"] is None and s["reconcile"] is None and s["review"] is None for s in self.ws.project["segments"]))
        self.assertEqual(len(self.ws.project["runs"]), 3)

    def test_prompt_change_invalidates_outputs(self):
        self.manual_forward()
        settings = self.ws.project["settings"].copy()
        settings["glossary"] = "New glossary"
        self.ws.save_settings(settings)
        self.assertTrue(all(s["forward"] is None for s in self.ws.project["segments"]))

    def test_batch_size_change_keeps_existing_translations(self):
        self.manual_forward()
        settings = self.ws.project["settings"].copy()
        settings["batch_segments"] = 1
        self.ws.save_settings(settings)
        self.assertTrue(all(s["forward"] for s in self.ws.project["segments"]))

    def test_demo_is_synthetic_and_never_calls_provider(self):
        self.ws.load_text(DEMO_SOURCE, "Example", demo=True)
        self.ws.start_job("all", demo=True)
        wait_for_job(self.ws)
        self.assertFalse(self.ws.job["error"])
        self.assertEqual(self.ws.client.requests, [])
        row = self.ws.project["segments"][3]
        self.assertIn("not only", row["reconcile"]["text"])
        self.assertEqual(row["reconcile"]["issues"][0]["category"], "negation")
        with self.assertRaises(WorkflowError):
            self.ws.review(row["id"], row["reconcile"]["text"], "AY", "")
        self.ws.review(row["id"], row["reconcile"]["text"], "AY", "Restored missing negation after source check.")
        self.assertEqual(row["review"]["status"], "reviewed")

    def test_demo_refuses_real_data(self):
        with self.assertRaises(WorkflowError):
            self.ws.start_job("all", demo=True)

    def test_keys_are_not_in_state_project_or_exports(self):
        self.api_config()
        self.ws.start_job("all")
        wait_for_job(self.ws)
        self.assertNotIn("NEVER_EXPORT_THIS_KEY", json.dumps(self.ws.snapshot()))
        with zipfile.ZipFile(io.BytesIO(export_bundle(self.ws.project))) as archive:
            for filename in archive.namelist():
                self.assertNotIn(b"NEVER_EXPORT_THIS_KEY", archive.read(filename))

    def test_restore_keeps_results_prompts_and_human_review(self):
        self.api_config()
        self.ws.start_job("all")
        wait_for_job(self.ws)
        self.ws.review("S0001", "Reviewed target", "AY", "Checked")
        restored = Workspace()
        restored.import_project(json.loads(json.dumps(self.ws.project)))
        self.assertEqual(restored.project["segments"][0]["review"]["text"], "Reviewed target")
        self.assertEqual(restored.project["runs"][0]["system_prompt"], self.ws.project["runs"][0]["system_prompt"])
        self.assertIn("raw_response", restored.project["runs"][0])
        self.assertFalse(restored.keys)

    def test_audit_hash_chain_covers_recorded_actions(self):
        self.manual_forward()
        previous = ""
        for event in self.ws.project["audit"]:
            self.assertEqual(event["previous_hash"], previous)
            self.assertEqual(event["hash"], digest({k: v for k, v in event.items() if k != "hash"}))
            previous = event["hash"]

    def test_invalid_transcript_title_preserves_existing_work(self):
        self.manual_forward()
        before = json.dumps(self.ws.project)
        with self.assertRaises(WorkflowError):
            self.ws.load_text("Participant: Replacement text", "")
        self.assertEqual(json.dumps(self.ws.project), before)

    def test_invalid_project_metadata_preserves_existing_work(self):
        self.manual_forward()
        before = json.dumps(self.ws.project)
        for field in ("title", "project_id"):
            value = json.loads(before)
            value[field] = ""
            with self.subTest(field=field), self.assertRaises(WorkflowError):
                self.ws.import_project(value)
            self.assertEqual(json.dumps(self.ws.project), before)


class ValidationTests(unittest.TestCase):
    def test_ids_missing_duplicated_reordered_or_invented_rejected_atomically(self):
        for ids in [["S0001"], ["S0001", "S0001"], ["S0002", "S0001"], ["S0001", "S9999"]]:
            with self.subTest(ids=ids), self.assertRaises(WorkflowError):
                parse_response(json.dumps({"segments": [{"id": sid, "text": "Text", "notes": []} for sid in ids]}), "forward", ["S0001", "S0002"])

    def test_empty_translation_or_non_json_rejected(self):
        for raw in ['{"segments":[{"id":"S0001","text":""}]}', 'Here is a translation: hello']:
            with self.subTest(raw=raw), self.assertRaises(WorkflowError):
                parse_response(raw, "forward", ["S0001"])

    def test_code_fenced_json_is_supported(self):
        fence = chr(96) * 3
        raw = fence + 'json\n{"segments":[{"id":"S0001","text":"Text","notes":[]}]}\n' + fence
        self.assertEqual(parse_response(raw, "forward", ["S0001"])[0]["text"], "Text")

    def test_unicode_turns_and_multiline_speech_preserved(self):
        segments = parse_transcript('Görüşmeci: Ağrı?\nHasta α: Çok değildi.\nAma bazen zordu.\n\n[duraklama]')
        self.assertEqual(len(segments), 3)
        self.assertEqual(segments[1]["speaker"], "Hasta α")
        self.assertEqual(segments[1]["source"], "Çok değildi.\nAma bazen zordu.")
        self.assertEqual(segments[2]["speaker"], "Hasta α")

    def test_batching_covers_each_segment_once(self):
        segments = parse_transcript("\n".join("P: " + "ö" * 250 for _ in range(15)))
        settings = defaults()
        settings["batch_chars"] = 500
        groups = make_batches(segments, settings, "forward")
        self.assertEqual([i for batch in groups for i in batch], [s["id"] for s in segments])
        self.assertTrue(all(len(batch) <= 2 for batch in groups))

    def test_paragraph_mode_keeps_inline_colon_as_content(self):
        segments = parse_transcript('Pain: it affected my life.\nI could not sleep.\n\nA separate paragraph.', split_speakers=False)
        self.assertEqual(len(segments), 2)
        self.assertEqual(segments[0]["speaker"], "")
        self.assertTrue(segments[0]["source"].startswith("Pain: it affected"))

    def test_docx_text_and_breaks_without_external_entities(self):
        xml = '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body><w:p><w:r><w:t>Participant A: Pain</w:t><w:br/><w:t>was important.</w:t></w:r></w:p></w:body></w:document>'
        data = io.BytesIO()
        with zipfile.ZipFile(data, "w") as archive:
            archive.writestr("word/document.xml", xml)
        text = read_upload("test.docx", base64.b64encode(data.getvalue()).decode())
        self.assertEqual(text, "Participant A: Pain\nwas important.")

    def test_docx_rejects_doctype(self):
        data = io.BytesIO()
        with zipfile.ZipFile(data, "w") as archive:
            archive.writestr("word/document.xml", '<!DOCTYPE foo [<!ENTITY a "bad">]><foo/>')
        with self.assertRaises(WorkflowError):
            read_upload("test.docx", base64.b64encode(data.getvalue()).decode())

    def test_exports_escape_html_and_excel_formulas(self):
        ws = Workspace()
        ws.load_text('P: =HYPERLINK("https://example.com")\nQ: <script>alert(1)</script>', '<img src=x onerror=alert(1)>')
        csv = export_csv(ws.project).decode()
        report = export_html(ws.project).decode()
        self.assertIn("'=HYPERLINK", csv)
        self.assertNotIn("<script>", report)
        self.assertIn("&lt;script&gt;", report)
        self.assertNotIn("<img src=x", report)


class ProviderTests(unittest.TestCase):
    def test_no_temperature_and_no_original_in_anthropic_back_request(self):
        class Capture(APIClient):
            def _request(self, provider, key, workspace, path, payload=None):
                self.captured = payload
                return {"content": [{"type": "thinking", "thinking": "ignore"}, {"type": "text", "text": '{"segments":[]}'}], "model": "returned-model", "stop_reason": "end_turn"}, "request-id"
        ws = Workspace()
        ws.load_text("P: Original source")
        ws.project["segments"][0]["forward"] = {"text": "Translated target", "notes": []}
        ws.project["settings"]["stages"]["back"] = {"provider": "anthropic", "model": "chosen-model", "workspace_id": "workspace"}
        task = build_task(ws.project, "back", ["S0001"])
        client = Capture()
        raw, meta = client.complete(task, "dummy")
        self.assertNotIn("temperature", client.captured)
        self.assertNotIn("Original source", json.dumps(client.captured))
        self.assertEqual(meta["returned_model"], "returned-model")

    def test_truncated_provider_response_is_never_accepted(self):
        class Capture(APIClient):
            def _request(self, *args, **kwargs):
                return {"choices": [{"finish_reason": "length", "message": {"content": "partial"}}]}, ""
        ws = Workspace()
        ws.load_text("P: Text")
        ws.project["settings"]["stages"]["forward"] = {"provider": "mistral", "model": "test", "workspace_id": ""}
        with self.assertRaisesRegex(WorkflowError, "truncated"):
            Capture().complete(build_task(ws.project, "forward", ["S0001"]), "dummy")


class HTTPTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workspace = Workspace()
        cls.token = "test-token-private"
        cls.server = ThreadingHTTPServer(("127.0.0.1", 0), make_handler(cls.workspace, cls.token))
        cls.url = "http://127.0.0.1:" + str(cls.server.server_port)
        cls.thread = threading.Thread(target=cls.server.serve_forever, daemon=True)
        cls.thread.start()

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()
        cls.server.server_close()

    def test_root_has_token_and_no_store_security_headers(self):
        with urllib.request.urlopen(self.url) as response:
            self.assertIn(self.token, response.read().decode())
            self.assertEqual(response.headers["Cache-Control"], "no-store")
            self.assertIn("frame-ancestors 'none'", response.headers["Content-Security-Policy"])

    def test_private_state_requires_token(self):
        with self.assertRaises(urllib.error.HTTPError) as error:
            urllib.request.urlopen(self.url + "/api/state")
        self.assertEqual(error.exception.code, 404)

    def test_foreign_origin_cannot_mutate_even_with_token(self):
        req = urllib.request.Request(self.url + "/api/demo", data=b"{}", headers={"Content-Type": "application/json", "X-QualiTranslate-Token": self.token, "Origin": "https://evil.example"})
        with self.assertRaises(urllib.error.HTTPError) as error:
            urllib.request.urlopen(req)
        self.assertEqual(error.exception.code, 403)

    def test_bad_host_is_blocked_for_dns_rebinding(self):
        req = urllib.request.Request(self.url, headers={"Host": "evil.example"})
        with self.assertRaises(urllib.error.HTTPError) as error:
            urllib.request.urlopen(req)
        self.assertEqual(error.exception.code, 403)

    def test_static_path_traversal_is_blocked(self):
        with self.assertRaises(urllib.error.HTTPError) as error:
            urllib.request.urlopen(self.url + "/assets/../core.py")
        self.assertEqual(error.exception.code, 404)

    def test_authorized_demo_endpoint_and_state(self):
        req = urllib.request.Request(self.url + "/api/demo", data=b"{}", headers={"Content-Type": "application/json", "X-QualiTranslate-Token": self.token, "Origin": self.url})
        with urllib.request.urlopen(req) as response:
            self.assertTrue(json.load(response)["ok"])
        req = urllib.request.Request(self.url + "/api/state", headers={"X-QualiTranslate-Token": self.token})
        with urllib.request.urlopen(req) as response:
            state = json.load(response)
        self.assertEqual(len(state["project"]["segments"]), 6)
        self.assertNotIn("runs", state["project"])
        self.assertNotIn("keys", state)


if __name__ == "__main__":
    unittest.main()
