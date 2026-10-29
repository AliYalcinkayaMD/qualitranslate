"""QualiTranslate core. Python standard library only; no network activity on import."""
from __future__ import annotations

import base64
import copy
import csv
import hashlib
import html
import io
import json
import math
import re
import secrets
import threading
import time
import urllib.error
import urllib.request
import uuid
import zipfile
from datetime import datetime, timezone
from xml.etree import ElementTree

from .prompts import PROMPTS, PROMPT_VERSION, CATEGORIES as _PROMPT_CATEGORIES
from .document_tools import DocumentError, read_document as _read_document, scan_document
from .quality import severity, terminology_issues
from .privacy import identifier_scan, identifier_summary

APP_VERSION = "1.5.1"
STAGES = ("forward", "back", "reconcile")
STAGE_LABELS = {"forward": "Forward translation", "back": "Blind back-translation", "reconcile": "Reconciliation"}
MAX_TEXT = 1_500_000
MAX_SEGMENTS = 4000
PROVIDERS = {"manual", "mistral", "anthropic"}
# Reasoning effort is a per-model API option. Claude: output_config.effort (Opus 4.5 and later);
# Mistral: reasoning_effort on models with adjustable reasoning. "" means provider default.
EFFORT_LEVELS = {"anthropic": ("low", "medium", "high", "xhigh", "max"), "mistral": ("none", "high")}
CATEGORIES = set(_PROMPT_CATEGORIES)
DEMO_SOURCE = """Interviewer: Hvornår kan man vurdere, om behandlingen var en succes?
Participant A: Jeg tror først, man kan sige det, når man er tilbage i sin hverdag.
Interviewer: Hvad var vigtigst for dig efter operationen?
Participant A: Det var ikke kun smerterne. Jeg ville også kunne gå en tur uden at planlægge det hele.
Participant B: Et år efter operationen havde jeg stadig ondt, men jeg var glad for, at jeg havde gjort det.
Participant A: [pause] Jeg ved ikke helt, hvad "normal" betyder længere."""
DEMO_FORWARD = [
    "When can we assess whether the treatment was a success?",
    "I think you can only say that once you are back to your everyday life.",
    "What mattered most to you after the operation?",
    "It was only the pain. I also wanted to be able to go for a walk without planning everything.",
    "A month after the operation, I was still in pain, but I was glad I had done it.",
    '[pause] I do not really know what "normal" means anymore.',
]
DEMO_BACK = [
    "Hvornår kan vi vurdere, om behandlingen var en succes?",
    "Jeg tror, at man først kan sige det, når man er tilbage i sin hverdag.",
    "Hvad var vigtigst for dig efter operationen?",
    "Det var kun smerterne. Jeg ville også kunne gå en tur uden at planlægge alt.",
    "En måned efter operationen havde jeg stadig smerter, men jeg var glad for at have gjort det.",
    '[pause] Jeg ved ikke rigtig, hvad "normal" betyder længere.',
]
DEMO_CORRECT = "It was not only the pain. I also wanted to be able to go for a walk without planning everything."


class WorkflowError(ValueError):
    pass


def now():
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def canonical(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def digest(value):
    return hashlib.sha256(canonical(value).encode("utf-8")).hexdigest()


def text_value(value, label, maximum=MAX_TEXT, allow_empty=False):
    if not isinstance(value, str) or len(value) > maximum or (not value.strip() and not allow_empty):
        raise WorkflowError(label + " is missing, invalid, or too long.")
    if "\x00" in value:
        raise WorkflowError(label + " contains a null character.")
    return value


def defaults():
    return {
        "source_language": "Danish", "target_language": "English",
        "study_context": "",
        "glossary": "", "batch_chars": 5000, "batch_segments": 12,
        "max_tokens": 16384, "terminology": [],
        "reporting": {"translation_scope": "Selected interview transcript before analysis", "dialect_context": "", "reviewer_background": "", "decision_summary": ""},
        "stages": {s: {"provider": "manual", "model": "", "workspace_id": "", "temperature": None, "top_p": None, "random_seed": None, "effort": "", "chat_tool": "", "chat_model": ""} for s in STAGES},
        "prompt_additions": {s: "" for s in STAGES},
    }


def validate_settings(value):
    if not isinstance(value, dict):
        raise WorkflowError("Settings must be an object.")
    result = defaults()
    for key in ("source_language", "target_language", "study_context", "glossary"):
        result[key] = text_value(value.get(key, result[key]), key, 15000 if key in {"study_context", "glossary"} else 120, allow_empty=key in {"study_context", "glossary"})
    if result["source_language"].strip().casefold() == result["target_language"].strip().casefold():
        raise WorkflowError("Choose different source and target languages.")
    for key, low, high in (("batch_chars", 500, 12000), ("batch_segments", 1, 40), ("max_tokens", 1024, 32768)):
        n = value.get(key, result[key])
        if type(n) is not int or not low <= n <= high:
            raise WorkflowError(key + " is outside the allowed range.")
        result[key] = n
    for stage in STAGES:
        item = value.get("stages", {}).get(stage, result["stages"][stage])
        if isinstance(item, dict) and item.get("provider") == "copilot":
            # Projects saved by pre-release versions may contain "copilot" stages; they become
            # copy-and-paste stages with the same chat tool.
            item = {**item, "provider": "manual", "chat_tool": item.get("chat_tool") or "Microsoft Copilot", "model": "", "workspace_id": "",
                    "temperature": None, "top_p": None, "random_seed": None, "effort": ""}
        if not isinstance(item, dict) or item.get("provider") not in PROVIDERS:
            raise WorkflowError("Invalid provider for " + stage)
        result["stages"][stage] = {
            "provider": item["provider"],
            "model": text_value(item.get("model", ""), "model", 200, True),
            "workspace_id": text_value(item.get("workspace_id", ""), "workspace id", 150, True),
            "chat_tool": text_value(item.get("chat_tool", ""), "Chat tool", 150, True) if item["provider"] == "manual" else "",
            "chat_model": text_value(item.get("chat_model", ""), "Chat model", 150, True) if item["provider"] == "manual" else "",
        }
        config = result["stages"][stage]
        for name in ("temperature", "top_p"):
            n = item.get(name)
            if n is not None and (type(n) not in {int, float} or not math.isfinite(n) or not 0 <= n <= 1 or (name == "top_p" and n == 0)):
                raise WorkflowError(name + " must be between 0 and 1 (top-p must be greater than 0).")
            config[name] = n
        seed = item.get("random_seed")
        if seed is not None and (type(seed) is not int or not 0 <= seed <= 2147483647):
            raise WorkflowError("Random seed must be an integer from 0 to 2147483647.")
        config["random_seed"] = seed
        if config["temperature"] is not None and config["top_p"] is not None:
            raise WorkflowError("Choose temperature or top-p, rather than setting both.")
        effort = item.get("effort") or ""
        if effort and effort not in EFFORT_LEVELS.get(config["provider"], ()):
            raise WorkflowError("Reasoning effort " + repr(effort) + " is not available for " + stage + " with this provider." +
                                (" Choose one of: " + ", ".join(EFFORT_LEVELS[config["provider"]]) + "." if config["provider"] in EFFORT_LEVELS else ""))
        config["effort"] = effort
        if config["provider"] == "anthropic" and config["random_seed"] is not None:
            raise WorkflowError("The Claude API has no random seed setting. Leave the seed empty for Claude.")
        result["prompt_additions"][stage] = text_value(value.get("prompt_additions", {}).get(stage, ""), "prompt addition", 5000, True)
    terms = value.get("terminology", [])
    if not isinstance(terms, list) or len(terms) > 300:
        raise WorkflowError("The terminology glossary supports up to 300 entries.")
    result["terminology"] = []
    for t in terms:
        if not isinstance(t, dict) or type(t.get("required", True)) is not bool:
            raise WorkflowError("Invalid terminology entry.")
        alternatives = t.get("alternatives", [])
        if not isinstance(alternatives, list) or len(alternatives) > 10:
            raise WorkflowError("Use up to ten accepted alternatives per term.")
        result["terminology"].append({"source": text_value(t.get("source"), "Source term", 300), "target": text_value(t.get("target"), "Preferred translation", 300),
                                      "alternatives": [text_value(a, "Alternative term", 300) for a in alternatives],
                                      "notes": text_value(t.get("notes", ""), "Terminology notes", 1500, True), "required": t.get("required", True)})
    reporting = value.get("reporting", {})
    if not isinstance(reporting, dict):
        raise WorkflowError("Invalid reporting information.")
    result["reporting"] = {k: text_value(reporting.get(k, default), k, 10000, True) for k, default in result["reporting"].items()}
    # Custom blind-back instructions are deliberately disallowed: they could leak source context.
    if result["prompt_additions"]["back"].strip():
        raise WorkflowError("Blind back-translation does not accept custom source context or instructions.")
    return result


def parse_transcript(text, split_speakers=True):
    text_value(text, "Transcript")
    text = text.replace("\r\n", "\n").replace("\r", "\n").lstrip("\ufeff")
    result, speaker, collected = [], "", []

    def flush():
        nonlocal collected
        body = "\n".join(collected).strip()
        if body:
            if len(body) > 24000:
                raise WorkflowError("A single turn exceeds 24,000 characters. Add paragraph breaks before importing.")
            result.append({"id": "S%04d" % (len(result) + 1), "speaker": speaker, "source": body,
                           "forward": None, "back": None, "reconcile": None, "review": None})
        collected = []

    # A line starting with a short label followed by a colon starts a new turn.
    # Speaker labels are metadata, never regenerated by the language model.
    for line in text.split("\n"):
        match = re.match(r"^\s*([^:\n]{1,60}):[ \t]*(.*)$", line) if split_speakers else None
        if match and not re.match(r"^(https?|\d+)$", match.group(1).strip(), re.I):
            flush()
            speaker = match.group(1).strip()
            collected = [match.group(2)]
        elif not line.strip():
            flush()
        else:
            collected.append(line)
    flush()
    if not result or len(result) > MAX_SEGMENTS:
        raise WorkflowError("Transcript must contain 1–4,000 segments.")
    return result


def read_document(filename, encoded):
    try:
        return _read_document(filename, encoded)
    except DocumentError as exc:
        raise WorkflowError(str(exc)) from exc


def read_upload(filename, encoded):
    return read_document(filename, encoded)["text"]


def parse_response(raw, stage, ids):
    text_value(raw, "Model response", 500_000)
    cleaned = raw.strip()
    fence = chr(96) * 3
    if cleaned.startswith(fence) and cleaned.endswith(fence):
        cleaned = cleaned[len(fence):-len(fence)].strip()
        if cleaned.startswith("json"):
            cleaned = cleaned[4:].strip()
    try:
        value = json.loads(cleaned)
    except json.JSONDecodeError as exc:
        raise WorkflowError("The response is not valid JSON. Paste the complete JSON object, without commentary.") from exc
    if not isinstance(value, dict) or not isinstance(value.get("segments"), list):
        raise WorkflowError("Expected a JSON object with a segments array.")
    items = value["segments"]
    if [item.get("id") if isinstance(item, dict) else None for item in items] != list(ids):
        raise WorkflowError("Segment ids are missing, duplicated, reordered, or unexpected. No results were applied.")
    out = []
    for item in items:
        segment = {"id": item["id"], "text": text_value(item.get("text"), "Translated text", 80000)}
        if stage in {"forward", "back"}:
            notes = item.get("notes", [])
            if not isinstance(notes, list) or len(notes) > 30:
                raise WorkflowError("notes must be an array of at most 30 strings.")
            segment["notes"] = [text_value(x, "note", 5000) for x in notes]
        else:
            segment["rationale"] = text_value(item.get("rationale"), "Reconciliation rationale", 10000)
            issues = item.get("issues")
            if not isinstance(issues, list) or len(issues) > 350:
                raise WorkflowError("Reconciliation requires an issues array.")
            segment["issues"] = []
            for issue in issues:
                if not isinstance(issue, dict) or issue.get("category") not in CATEGORIES or issue.get("severity") not in {"minor", "major", "critical"}:
                    raise WorkflowError("Invalid reconciliation issue category or severity.")
                segment["issues"].append({"category": issue["category"], "severity": issue["severity"],
                                          "detail": text_value(issue.get("detail"), "Issue detail", 10000), "needs_human": True, "origin": "model"})
            segment["needs_human"] = True
        out.append(segment)
    return out


def result_text(segment, stage):
    result = segment.get(stage)
    return result["text"] if result else ""


def make_batches(segments, settings, stage):
    batches, batch, count = [], [], 0
    for seg in segments:
        if stage == "forward":
            cost = len(seg["source"])
        elif stage == "back":
            if not seg.get("forward"):
                raise WorkflowError("Complete all forward translations before blind back-translation.")
            cost = len(result_text(seg, "forward"))
        else:
            if not seg.get("forward") or not seg.get("back"):
                raise WorkflowError("Complete forward and blind back-translations before reconciliation.")
            cost = len(seg["source"]) + len(result_text(seg, "forward")) + len(result_text(seg, "back"))
        if batch and (count + cost > settings["batch_chars"] or len(batch) >= settings["batch_segments"]):
            batches.append(batch)
            batch, count = [], 0
        batch.append(seg["id"])
        count += cost
    if batch:
        batches.append(batch)
    return batches


def build_task(project, stage, ids, include_context=True):
    if stage not in STAGES:
        raise WorkflowError("Invalid stage.")
    settings, all_segments = project["settings"], project["segments"]
    by_id = {x["id"]: x for x in all_segments}
    if not ids or any(x not in by_id for x in ids):
        raise WorkflowError("Invalid segment selection.")
    rows = [by_id[x] for x in ids]
    indices = [i for i, x in enumerate(all_segments) if x["id"] in ids]
    before = all_segments[max(0, min(indices) - 1):min(indices)] if include_context else []
    after = all_segments[max(indices) + 1:max(indices) + 2] if include_context else []
    if stage == "back":
        if any(not s.get("forward") for s in rows + before + after):
            raise WorkflowError("Forward translations are incomplete.")
        # Construct an allowlisted back-translation payload from target text only.
        # Never pass a source segment object, source glossary, source notes, or study context.
        payload = {
            "task": "blind_back_translation", "translated_language": settings["target_language"],
            "original_language": settings["source_language"],
            "segments": [{"id": s["id"], "text": result_text(s, "forward")} for s in rows],
            "context_only": [{"text": result_text(s, "forward")} for s in before + after],
        }
    elif stage == "forward":
        payload = {"source_language": settings["source_language"], "target_language": settings["target_language"],
                   "study_context": settings["study_context"], "glossary": settings["glossary"],
                   "segments": [{"id": s["id"], "text": s["source"]} for s in rows],
                   "context_only": [{"text": s["source"]} for s in before + after]}
    else:
        if any(not s.get("forward") or not s.get("back") for s in rows):
            raise WorkflowError("Forward or back-translation is missing.")
        payload = {"source_language": settings["source_language"], "target_language": settings["target_language"],
                   "study_context": settings["study_context"], "glossary": settings["glossary"],
                   "segments": [{"id": s["id"], "original_source": s["source"],
                                 "forward_translation": result_text(s, "forward"),
                                 "forward_notes": s["forward"].get("notes", []),
                                 "blind_back_translation": result_text(s, "back"),
                                 "back_notes": s["back"].get("notes", [])} for s in rows],
                   "context_only": [{"original_source": s["source"]} for s in before + after]}
    if stage != "back":
        payload["terminology_lexicon"] = copy.deepcopy(settings.get("terminology", []))
        payload["dialect_context"] = settings.get("reporting", {}).get("dialect_context", "")
    system = PROMPTS[stage]
    addition = settings["prompt_additions"][stage]
    if stage != "back" and addition.strip():
        system += "\nADDITIONAL TEAM INSTRUCTIONS (subject to the fidelity and JSON requirements above):\n" + addition
    return {"task_id": uuid.uuid4().hex, "stage": stage, "ids": list(ids), "revision": project["revision"],
            "prompt_version": PROMPT_VERSION, "system": system, "user": json.dumps(payload, ensure_ascii=False, indent=2),
            "created_at": now(), "configuration": copy.deepcopy(settings["stages"][stage]),
            "max_tokens": settings["max_tokens"], "context_omitted": not include_context}


def sampling_application(config):
    provider = config["provider"]
    if provider == "manual":
        return "manual_user_reported"
    if provider == "copilot":
        return "external_chat_not_controlled"
    if any(config.get(n) not in (None, "") for n in ("temperature", "top_p", "random_seed", "effort")):
        return "api_request"
    return "provider_default"


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


class APIClient:
    """Only fixed HTTPS provider endpoints; no credentials in exports or request logs."""
    def __init__(self):
        self.opener = urllib.request.build_opener(NoRedirect())

    def _request(self, provider, key, workspace_id, path, payload=None):
        if provider not in {"mistral", "anthropic"} or not key:
            raise WorkflowError("Add the provider API key in Settings first.")
        base = "https://api.mistral.ai" if provider == "mistral" else "https://api.anthropic.com"
        headers = {"Content-Type": "application/json", "Accept": "application/json", "Authorization": "Bearer " + key}
        if provider == "anthropic":
            headers["anthropic-version"] = "2023-06-01"
            if workspace_id:
                headers["anthropic-workspace-id"] = workspace_id
        data = json.dumps(payload, ensure_ascii=False).encode() if payload is not None else None
        for attempt in range(3):
            req = urllib.request.Request(base + path, data=data, headers=headers, method="POST" if data is not None else "GET")
            try:
                with self.opener.open(req, timeout=120) as response:
                    raw = response.read(6_000_001)
                    if len(raw) > 6_000_000:
                        raise WorkflowError("Provider response exceeds the size limit.")
                    return json.loads(raw), response.headers.get("request-id", "")
            except urllib.error.HTTPError as exc:
                if exc.code in {429, 502, 503, 504} and attempt < 2:
                    try:
                        delay = min(10.0, max(1.0, float(exc.headers.get("Retry-After", 2 ** attempt))))
                    except (ValueError, TypeError):
                        delay = 2.0
                    time.sleep(delay)
                    continue
                hints = {401: "Check the API key.", 403: "Check model/workspace permissions.",
                         404: "The model or resource was not found. Load the available models and choose its exact id.",
                         429: "Rate or spending limit reached. Wait before resuming.",
                         400: "The provider rejected these settings. Check the model id, workspace and token budget. Some models do not accept temperature, top-p or reasoning effort: set them back to Provider default in Settings and try again."}
                # Provider error bodies can echo content or credentials; do not display or log them.
                raise WorkflowError(provider + " HTTP " + str(exc.code) + ". " + hints.get(exc.code, "The provider request failed.")) from None
            except (urllib.error.URLError, TimeoutError, OSError):
                raise WorkflowError("Provider connection failed or timed out. No automatic timeout retry was made; check results before retrying.") from None
            except json.JSONDecodeError:
                raise WorkflowError("Provider did not return valid JSON.") from None

    def models(self, provider, key, workspace_id=""):
        result, _ = self._request(provider, key, workspace_id, "/v1/models")
        models = result.get("data", [])
        return sorted({m["id"] for m in models if isinstance(m, dict) and isinstance(m.get("id"), str)})

    def complete(self, task, key):
        config = task["configuration"]
        provider, model = config["provider"], config["model"].strip()
        if not model:
            raise WorkflowError("Choose an exact model id in Settings before running the API.")
        if provider == "mistral":
            payload = {"model": model, "messages": [{"role": "system", "content": task["system"]}, {"role": "user", "content": task["user"]}],
                       "response_format": {"type": "json_object"}, "max_tokens": task["max_tokens"], "stream": False}
            for option in ("temperature", "top_p", "random_seed"):
                if config.get(option) is not None:
                    payload[option] = config[option]
            if config.get("effort"):
                payload["reasoning_effort"] = config["effort"]
            result, rid = self._request(provider, key, "", "/v1/chat/completions", payload)
            choices = result.get("choices", [])
            if not choices:
                raise WorkflowError("Provider returned no completion.")
            if choices[0].get("finish_reason") == "length":
                raise WorkflowError("Output was truncated. Increase the token budget or reduce batch size, then resume.")
            content = choices[0].get("message", {}).get("content", "")
        elif provider == "anthropic":
            payload = {"model": model, "system": task["system"], "messages": [{"role": "user", "content": task["user"]}], "max_tokens": task["max_tokens"]}
            # Sampling values are sent only when the user sets them: several current Claude
            # models reject temperature/top-p, and older ones do not support effort.
            for option in ("temperature", "top_p"):
                if config.get(option) is not None:
                    payload[option] = config[option]
            if config.get("effort"):
                payload["output_config"] = {"effort": config["effort"]}
            result, rid = self._request(provider, key, config.get("workspace_id", ""), "/v1/messages", payload)
            if result.get("stop_reason") in {"max_tokens", "pause_turn", "refusal"}:
                raise WorkflowError("Provider did not finish a usable translation (" + result.get("stop_reason", "") + "). Reduce batch size or change model.")
            content = result.get("content", [])
        else:
            raise WorkflowError("Manual stages use the copy/paste workflow.")
        if isinstance(content, list):
            content = "".join(x.get("text", "") for x in content if isinstance(x, dict) and x.get("type") == "text")
        if not isinstance(content, str) or not content.strip():
            raise WorkflowError("Provider returned no text. Try a larger output budget or a different model.")
        return content, {"provider": provider, "requested_model": model, "returned_model": result.get("model", model),
                         "request_id": rid or result.get("id", ""), "usage": result.get("usage", {}), "completed_at": now()}


class Workspace:
    def __init__(self, client=None):
        self.lock = threading.RLock()
        self.client = client or APIClient()
        self.keys = {}
        self.tasks = {}
        self.inspection = None
        self._identifier_cache = (None, None)
        self.cancel = threading.Event()
        self.job = {"running": False, "message": "Ready", "error": "", "error_code": "", "done": 0, "total": 0, "stage": "", "batch": 0, "batches": 0}
        self.project = self._new_project()

    @staticmethod
    def _new_project():
        return {"format": "qualitranslate-project", "format_version": 1, "app_version": APP_VERSION,
                "project_id": uuid.uuid4().hex, "title": "Untitled interview", "created_at": now(),
                "revision": 0, "settings": defaults(), "segments": [], "runs": [], "audit": [], "demo": False}

    def _idle(self):
        if self.job["running"]:
            raise WorkflowError("A translation job is running. Stop it and wait for it to finish before editing.")

    def _audit(self, action, **details):
        events = self.project["audit"]
        event = {"at": now(), "action": action, "details": details,
                 "previous_hash": events[-1]["hash"] if events else ""}
        event["hash"] = digest(event)
        events.append(event)

    def _changed(self):
        self.project["revision"] += 1
        self.tasks.clear()

    def identifier_check(self):
        """Possible identifiers in the source (cached per project revision)."""
        with self.lock:
            key = (self.project["project_id"], self.project["revision"], len(self.project["segments"]))
            if self._identifier_cache[0] != key:
                self._identifier_cache = (key, identifier_scan(self.project["segments"]))
            return copy.deepcopy(self._identifier_cache[1])

    def snapshot(self):
        with self.lock:
            return {"project": copy.deepcopy(self.project), "job": copy.deepcopy(self.job), "identifier_check": self.identifier_check(),
                    "credentials": {k: bool(self.keys.get(k)) for k in ("mistral", "anthropic")}}

    def set_key(self, provider, key):
        with self.lock:
            self._idle()
            if provider not in {"mistral", "anthropic"}:
                raise WorkflowError("Unknown provider.")
            key = text_value(key, "API key", 1000, True).strip()
            if key:
                self.keys[provider] = key
            else:
                self.keys.pop(provider, None)

    def clear(self):
        with self.lock:
            self._idle()
            self.keys.clear()
            self.tasks.clear()
            self.inspection = None
            self.project = self._new_project()
            self.job = {"running": False, "message": "Ready", "error": "", "error_code": "", "done": 0, "total": 0, "stage": "", "batch": 0, "batches": 0}

    def load_text(self, text, title="Untitled interview", demo=False, split_speakers=True):
        segments = parse_transcript(text, split_speakers=split_speakers)
        title = text_value(title, "title", 200)
        with self.lock:
            self._idle()
            settings = defaults() if demo else self.project["settings"]
            self.project = self._new_project()
            self.project.update({"segments": segments, "settings": settings, "title": title, "demo": demo})
            self.tasks.clear()
            self.inspection = None
            self._audit("import_transcript", segments=len(segments), source_sha256=digest([s["source"] for s in segments]), synthetic=demo, split_speakers=split_speakers)

    def inspect_document(self, text, blocks=None):
        try:
            result = scan_document(text, blocks)
        except DocumentError as exc:
            raise WorkflowError(str(exc)) from exc
        with self.lock:
            self._idle()
            self.inspection = {**result, "inspection_id": uuid.uuid4().hex, "original_text": text, "original_sha256": digest(text)}
            return copy.deepcopy({k: v for k, v in self.inspection.items() if k != "original_text"})

    def import_selected(self, inspection_id, included_ids, title, settings, split_speakers=True):
        settings = validate_settings(settings)
        title = text_value(title, "Title", 200)
        with self.lock:
            self._idle()
            d = self.inspection
            if not d or d["inspection_id"] != inspection_id:
                raise WorkflowError("Document preview expired. Check the transcript again.")
            if not isinstance(included_ids, list) or not included_ids or any(not isinstance(i, str) for i in included_ids) or len(set(included_ids)) != len(included_ids):
                raise WorkflowError("Select at least one interview block, without duplicate ids.")
            chosen = set(included_ids)
            if not chosen.issubset({b["id"] for b in d["blocks"]}):
                raise WorkflowError("An unknown document block was selected.")
            text = "\n\n".join(b["text"] for b in d["blocks"] if b["id"] in chosen)
            self.load_text(text, title, split_speakers=split_speakers)
            self.project["settings"] = settings
            self.project["source_document"] = {"scan_version": d["scan_version"], "original_sha256": d["original_sha256"], "confirmed_at": now(),
                "blocks": [{**b, "included": b["id"] in chosen} for b in d["blocks"]]}
            self._audit("document_selection_confirmed", included_ids=[b["id"] for b in d["blocks"] if b["id"] in chosen], excluded_ids=[b["id"] for b in d["blocks"] if b["id"] not in chosen], scan_version=d["scan_version"])
            self.inspection = None

    def save_settings(self, value):
        settings = validate_settings(value)
        with self.lock:
            self._idle()
            old = self.project["settings"]
            if settings == old:
                return
            # Batch size and credentials do not alter completed translations. Any linguistic,
            # model, or prompt change invalidates outputs to prevent mixed, stale provenance.
            excluded = {"batch_chars", "batch_segments", "max_tokens", "reporting", "stages", "prompt_additions"}
            shared = {k: v for k, v in settings.items() if k not in excluded} != {k: v for k, v in old.items() if k not in excluded}
            shared = shared or settings["reporting"]["dialect_context"] != old.get("reporting", {}).get("dialect_context", "")
            # A change that only affects a later stage (for example a different back-translation
            # model) keeps the earlier stages; everything from the first affected stage is cleared.
            changed_stages = [st for st in STAGES if settings["stages"][st] != old.get("stages", {}).get(st) or settings["prompt_additions"][st] != old.get("prompt_additions", {}).get(st)]
            first = STAGES[0] if shared else (changed_stages[0] if changed_stages else None)
            substantive = first is not None
            if substantive:
                for s in self.project["segments"]:
                    for stage in STAGES[STAGES.index(first):]:
                        s[stage] = None
                    s["review"] = None
            self.project["settings"] = settings
            self._changed()
            self._audit("settings_changed", outputs_invalidated=substantive, invalidated_from_stage=first or "", settings_sha256=digest(settings))

    def _next_ids(self, stage):
        if not self.project["segments"]:
            raise WorkflowError("Import a transcript first.")
        batches = make_batches(self.project["segments"], self.project["settings"], stage)
        by_id = {s["id"]: s for s in self.project["segments"]}
        # Skip already completed ids, so partial runs can resume without charging twice.
        return [[i for i in batch if not by_id[i].get(stage)] for batch in batches if any(not by_id[i].get(stage) for i in batch)]

    def prepare_task(self, stage):
        with self.lock:
            self._idle()
            pending = self._next_ids(stage)
            if not pending:
                raise WorkflowError("This stage is already complete.")
            task = build_task(self.project, stage, pending[0])
            self.tasks[task["task_id"]] = task
            return copy.deepcopy(task)

    def _apply(self, task, raw, provenance):
        if task["revision"] != self.project["revision"]:
            raise WorkflowError("The project changed after this prompt was created. Generate a new prompt.")
        results = parse_response(raw, task["stage"], task["ids"])
        run_id = uuid.uuid4().hex
        by_id = {s["id"]: s for s in self.project["segments"]}
        stage = task["stage"]
        for item in results:
            seg = by_id[item["id"]]
            if stage == "reconcile":
                checks = terminology_issues(seg["source"], item["text"], self.project["settings"].get("terminology", []))
                item["issues"].extend(checks)
            seg[stage] = {k: v for k, v in item.items() if k != "id"}
            seg[stage]["run_id"] = run_id
            for downstream in STAGES[STAGES.index(stage) + 1:]:
                seg[downstream] = None
            seg["review"] = None
        self.project["runs"].append({"id": run_id, "stage": stage, "segment_ids": task["ids"],
                                     "prompt_version": PROMPT_VERSION, "system_prompt": task["system"],
                                     "user_prompt": task["user"], "response_sha256": digest(raw),
                                     "raw_response": raw, "parsed_results": results,
                                     "transport_prompt": task.get("transport_prompt", ""),
                                     "context_omitted": task.get("context_omitted", False),
                                     "request_configuration": {**task["configuration"], "max_tokens": task["max_tokens"],
                                                               "sampling_application": sampling_application(task["configuration"])},
                                     "provenance": provenance})
        self._changed()
        self._audit("stage_completed", stage=stage, segment_ids=task["ids"], run_id=run_id, provenance=provenance)

    def apply_manual(self, task_id, raw, model_label):
        with self.lock:
            self._idle()
            task = self.tasks.get(task_id)
            if not task:
                raise WorkflowError("Prompt expired. Generate a new prompt for the current project.")
            model_label = text_value(model_label, "Tool / model used", 200)
            self._apply(task, raw, {"provider": "manual", "tool_model_reported_by_user": model_label, "completed_at": now()})

    def edit(self, segment_id, stage, text, notes=""):
        if stage not in {"source", "forward", "back", "reconcile"}:
            raise WorkflowError("Unknown stage.")
        text_value(text, "Edited text", 80000)
        with self.lock:
            self._idle()
            seg = next((s for s in self.project["segments"] if s["id"] == segment_id), None)
            if not seg:
                raise WorkflowError("Unknown segment.")
            if stage == "reconcile" and seg.get("reconcile"):
                raise WorkflowError("Use human review to change a reconciled translation.")
            if stage != "source" and not seg.get(stage):
                # A section the tool could not translate (for example, a chat tool declined it) can be
                # given a human translation. It is recorded as human work, not model output.
                prior = STAGES[STAGES.index(stage) - 1] if stage != "forward" else None
                if prior and not seg.get(prior):
                    raise WorkflowError("Add the earlier stage for this section first.")
                entry = {"at": now(), "notes": text_value(notes, "Edit notes", 5000, True)}
                if stage == "reconcile":
                    seg[stage] = {"text": text, "issues": [], "rationale": "Proposed by a person because the model did not produce a reconciliation for this section.",
                                  "needs_human": True, "run_id": "human_entry", "human_entry": entry}
                else:
                    seg[stage] = {"text": text, "notes": [], "run_id": "human_entry", "human_entry": entry}
                for downstream in STAGES[STAGES.index(stage) + 1:]:
                    seg[downstream] = None
                seg["review"] = None
                self.project["demo"] = False
                self._changed()
                self._audit("human_translation_entered", segment_id=segment_id, stage=stage, after_sha256=digest(text), note=notes)
                return
            before = seg[stage] if stage == "source" else seg[stage]["text"]
            if before == text:
                return
            if stage == "source":
                seg["source"] = text
                invalidated = STAGES
            else:
                seg[stage]["text"] = text
                seg[stage]["human_edit"] = {"at": now(), "notes": text_value(notes, "Edit notes", 5000, True)}
                invalidated = STAGES[STAGES.index(stage) + 1:]
            # Neighbouring context may have influenced every batch, so invalidate all
            # dependent translations, not just this row.
            for other in self.project["segments"]:
                for downstream in invalidated:
                    other[downstream] = None
                other["review"] = None
            self.project["demo"] = False
            self._changed()
            self._audit("human_edit", segment_id=segment_id, stage=stage, before_sha256=digest(before), after_sha256=digest(text), note=notes)

    def review(self, segment_id, final_text, reviewer, note, uncertainty_retained=False, method="individual"):
        if method not in {"individual", "quick_accept"}:
            raise WorkflowError("Invalid review method.")
        with self.lock:
            self._idle()
            seg = next((s for s in self.project["segments"] if s["id"] == segment_id), None)
            if not seg or not seg.get("reconcile"):
                raise WorkflowError("Reconcile this segment before human review.")
            reviewer = text_value(reviewer, "Reviewer", 150)
            final_text = text_value(final_text, "Reviewed translation", 80000)
            note = text_value(note, "Review note", 15000, True)
            term_warnings = terminology_issues(seg["source"], final_text, self.project["settings"].get("terminology", []))
            significant = any(x["severity"] == "major" for x in term_warnings) or any(x["severity"] in {"major", "critical"} for x in seg["reconcile"]["issues"])
            if method == "quick_accept" and (severity(seg) in {"major", "critical"} or uncertainty_retained or final_text != seg["reconcile"]["text"]):
                raise WorkflowError("Quick approval is available for unchanged, unflagged or minor-only drafts. Open review for other decisions.")
            if (significant or uncertainty_retained) and not note.strip():
                raise WorkflowError("Add a review note explaining the major issue or retained uncertainty.")
            seg["review"] = {"text": final_text, "reviewer": reviewer, "note": note, "at": now(),
                             "status": "reviewed_uncertainty_retained" if uncertainty_retained else "reviewed",
                             "based_on_run": seg["reconcile"]["run_id"], "method": method, "terminology_warnings": term_warnings}
            self._changed()
            self._audit("human_review", segment_id=segment_id, reviewer=reviewer, status=seg["review"]["status"], translation_sha256=digest(final_text), note=note, method=method)

    def review_batch_minor(self, ids, reviewer, expected_revision):
        reviewer = text_value(reviewer, "Reviewer", 150)
        with self.lock:
            self._idle()
            if type(expected_revision) is not int or expected_revision != self.project["revision"]:
                raise WorkflowError("The project changed. Refresh the minor approval list before continuing.")
            if not isinstance(ids, list) or not ids or any(not isinstance(i, str) for i in ids) or len(set(ids)) != len(ids):
                raise WorkflowError("Choose a non-empty list of unique minor-only sections.")
            by_id = {s["id"]: s for s in self.project["segments"]}
            selected = [by_id.get(i) for i in ids]
            if any(not s or not s.get("reconcile") or s.get("review") or severity(s) != "minor" for s in selected):
                raise WorkflowError("Batch approval accepts only pending minor-only sections. Major, critical, unflagged, and reviewed sections were not changed.")
            if any(any(w["severity"] == "major" for w in terminology_issues(s["source"], s["reconcile"]["text"], self.project["settings"].get("terminology", []))) for s in selected):
                raise WorkflowError("A required terminology rule needs an individual decision. No batch approvals were applied.")
            batch_id = uuid.uuid4().hex
            for seg in selected:
                seg["review"] = {"text": seg["reconcile"]["text"], "reviewer": reviewer, "note": "Accepted unchanged in an explicit user-selected batch of minor-only sections.",
                    "at": now(), "status": "reviewed", "method": "batch_minor", "batch_id": batch_id, "based_on_run": seg["reconcile"]["run_id"]}
            self._changed()
            self._audit("human_batch_review", method="batch_minor", batch_id=batch_id, segment_ids=ids, reviewer=reviewer, count=len(ids))

    def clear_review(self, segment_id):
        with self.lock:
            self._idle()
            seg = next((s for s in self.project["segments"] if s["id"] == segment_id), None)
            if not seg or not seg.get("review"):
                raise WorkflowError("No recorded decision to clear.")
            previous = copy.deepcopy(seg["review"])
            seg["review"] = None
            self._changed()
            self._audit("review_returned_to_pending", segment_id=segment_id, previous_review=previous)

    def demo_response(self, task):
        if not self.project["demo"] or [s["source"] for s in self.project["segments"]] != [s["source"] for s in parse_transcript(DEMO_SOURCE)]:
            raise WorkflowError("The demonstration runs only on the unchanged synthetic sample.")
        if self.project["settings"]["source_language"].casefold() != "danish" or self.project["settings"]["target_language"].casefold() != "english":
            raise WorkflowError("The demonstration requires Danish → English.")
        all_ids = [s["id"] for s in self.project["segments"]]
        rows = []
        for sid in task["ids"]:
            idx = all_ids.index(sid)
            if task["stage"] in {"forward", "back"}:
                rows.append({"id": sid, "text": (DEMO_FORWARD if task["stage"] == "forward" else DEMO_BACK)[idx], "notes": []})
            else:
                issues = []
                proposed, rationale = DEMO_FORWARD[idx], "No discrepancy identified in this synthetic example."
                if idx == 3:
                    issues = [{"category": "negation", "severity": "critical", "detail": "The forward translation omitted 'ikke' (not), reversing the meaning. Restore 'not only'.", "needs_human": True}]
                    proposed, rationale = DEMO_CORRECT, "Restored the missing negation."
                elif idx == 4:
                    issues = [{"category": "temporality", "severity": "major", "detail": "The original says one year after surgery; the forward and back-translations say one month. Restore the original time point.", "needs_human": True}]
                    proposed, rationale = "A year after the operation, I was still in pain, but I was glad I had done it.", "Restored one year rather than one month."
                elif idx == 5:
                    issues = [{"category": "source_ambiguity", "severity": "minor", "detail": "'Normal' is intentionally left undefined by the participant. Preserve the quotation marks and uncertainty; do not replace it with an objective recovery standard.", "needs_human": True}]
                    rationale = "Retained the participant's unresolved meaning of normal."
                rows.append({"id": sid, "text": proposed, "issues": issues, "rationale": rationale, "needs_human": True})
        return json.dumps({"segments": rows}, ensure_ascii=False)

    def start_job(self, stage, demo=False, expected_project_id=None, expected_revision=None):
        if stage not in STAGES + ("all",):
            raise WorkflowError("Unknown translation stage.")
        with self.lock:
            self._idle()
            if (expected_project_id is not None and expected_project_id!=self.project['project_id']) or (expected_revision is not None and expected_revision!=self.project['revision']):
                raise WorkflowError('The project changed before the run started. Nothing was sent. Start again to translate the current interview.')
            stages = STAGES if stage == "all" else (stage,)
            if not self.project["segments"]:
                raise WorkflowError("Import a transcript first.")
            if not demo:
                for s in stages:
                    config = self.project["settings"]["stages"][s]
                    if config["provider"] == "manual":
                        raise WorkflowError("Configure API providers for these stages, or use the manual prompt workflow.")
                    if not self.keys.get(config["provider"]) or not config["model"].strip():
                        raise WorkflowError("Set an API key and model id for " + s + " first.")
            else:
                self.demo_response(build_task(self.project, "forward", [self.project["segments"][0]["id"]]))
            # Validate prerequisites for a single-stage run before launching.
            if stage != "all":
                self._next_ids(stage)
            self.cancel.clear()
            self.job = {"running": True, "message": "Starting", "error": "", "error_code": "", "done": 0, "total": 0, "stage": "", "batch": 0, "batches": 0}
            thread = threading.Thread(target=self._worker, args=(stages, demo), daemon=True)
            thread.start()

    def _worker(self, stages, demo):
        try:
            for stage in stages:
                with self.lock:
                    batches = self._next_ids(stage)
                    self.job["total"] += len(batches)
                for number, ids in enumerate(batches, 1):
                    with self.lock:
                        if self.cancel.is_set():
                            raise WorkflowError("Stopped. Completed batches were retained.")
                        task = build_task(self.project, stage, ids)
                        self.job.update(stage=stage, batch=number, batches=len(batches))
                        self.job["message"] = ("Demonstration: " if demo else "") + STAGE_LABELS[stage] + " · batch " + str(number) + " of " + str(len(batches)) + " · " + ids[0] + ("–" + ids[-1] if len(ids) > 1 else "")
                        key = self.keys.get(task["configuration"]["provider"], "")
                    if demo:
                        raw, provenance = self.demo_response(task), {"provider": "synthetic_demo", "model": "deterministic-fixture", "completed_at": now()}
                    else:
                        raw, provenance = self.client.complete(task, key)
                    with self.lock:
                        # A reply that already arrived is kept even if Stop was pressed meanwhile.
                        self._apply(task, raw, provenance)
                        self.job["done"] += 1
                        if self.cancel.is_set():
                            raise WorkflowError("Stopped after the current request. Completed batches were retained.")
            with self.lock:
                self.job["message"] = "Stages complete. Human review is still required."
        except Exception as exc:
            with self.lock:
                self.job["error"] = str(exc) if isinstance(exc, WorkflowError) else "An unexpected error occurred. Completed batches were retained."
                self.job["error_code"] = ""
                self.job["message"] = "Stopped"
        finally:
            with self.lock:
                self.job["running"] = False

    def import_project(self, value):
        if not isinstance(value, dict) or value.get("format") != "qualitranslate-project" or value.get("format_version") != 1:
            raise WorkflowError("This is not a supported QualiTranslate project.")
        if len(canonical(value)) > 20_000_000:
            raise WorkflowError("Project exceeds the 20 MB import limit.")
        settings = validate_settings(value.get("settings", {}))
        title = text_value(value.get("title", "Imported interview"), "Title", 200)
        prior_project_id = text_value(value.get("project_id", "unknown"), "Project id", 150)
        segments = value.get("segments")
        if not isinstance(segments, list) or not 1 <= len(segments) <= MAX_SEGMENTS:
            raise WorkflowError("Invalid project segments.")
        clean = []
        ids = []
        for old in segments:
            if not isinstance(old, dict):
                raise WorkflowError("Invalid segment.")
            sid = text_value(old.get("id"), "Segment id", 20)
            if not re.fullmatch(r"S[0-9]{4,8}", sid) or sid in ids:
                raise WorkflowError("Invalid or duplicate segment id.")
            ids.append(sid)
            row = {"id": sid, "speaker": text_value(old.get("speaker", ""), "Speaker", 150, True),
                   "source": text_value(old.get("source"), "Source", 80000), "review": None}
            for stage in STAGES:
                if old.get(stage):
                    result = parse_response(json.dumps({"segments": [{"id": sid, **old[stage]}]}), stage, [sid])[0]
                    if stage == "reconcile":
                        for issue, archived in zip(result["issues"], old[stage].get("issues", [])):
                            issue["origin"] = "local_glossary_check" if archived.get("origin") == "local_glossary_check" else "model"
                    row[stage] = {k: v for k, v in result.items() if k != "id"}
                    row[stage]["run_id"] = text_value(old[stage].get("run_id", "imported"), "run id", 150)
                    if old[stage].get("human_entry"):
                        entry = old[stage]["human_entry"]
                        row[stage]["human_entry"] = {"at": text_value(entry.get("at", ""), "entry time", 100, True), "notes": text_value(entry.get("notes", ""), "entry note", 5000, True)}
                    if old[stage].get("human_edit"):
                        edit = old[stage]["human_edit"]
                        row[stage]["human_edit"] = {"at": text_value(edit.get("at", ""), "edit time", 100, True), "notes": text_value(edit.get("notes", ""), "edit note", 5000, True)}
                else:
                    row[stage] = None
            if row["back"] and not row["forward"] or row["reconcile"] and not row["back"]:
                raise WorkflowError("Imported stage prerequisites are inconsistent.")
            if old.get("review") and row["reconcile"]:
                r = old["review"]
                if r.get("status") not in {"reviewed", "reviewed_uncertainty_retained"}:
                    raise WorkflowError("Invalid review status.")
                row["review"] = {"text": text_value(r.get("text"), "Reviewed text", 80000), "reviewer": text_value(r.get("reviewer"), "Reviewer", 150),
                                 "note": text_value(r.get("note", ""), "Review note", 15000, True), "at": text_value(r.get("at", ""), "Review date", 100, True),
                                 "status": r["status"], "based_on_run": row["reconcile"]["run_id"], "method": r.get("method", "individual") if r.get("method", "individual") in {"individual", "quick_accept", "batch_minor"} else "individual",
                                 "batch_id": text_value(r.get("batch_id", ""), "Batch id", 100, True), "terminology_warnings": terminology_issues(row["source"], r.get("text", ""), settings.get("terminology", []))}
            clean.append(row)
        # History is retained as an imported archive and never executed. Strip any unknown
        # history keys rather than copying arbitrary uploaded content into the active state.
        runs = value.get("runs", [])
        if not isinstance(runs, list) or len(runs) > 25000:
            raise WorkflowError("Invalid run history.")
        allowed_run = {"id", "stage", "segment_ids", "prompt_version", "system_prompt", "user_prompt", "transport_prompt", "context_omitted", "response_sha256", "raw_response", "parsed_results", "provenance"}
        clean_runs = []
        for run in runs:
            if not isinstance(run, dict) or run.get("stage") not in STAGES:
                raise WorkflowError("Invalid run record.")
            allowed_provenance = {"provider", "model", "requested_model", "returned_model", "request_id", "usage", "completed_at", "tool_model_reported_by_user", "integration", "chat_isolation", "receipt_verified",
                                  "copilot_model_selector_label", "temporary_chat_notice", "browser", "submitted_at"}
            kept = {k: copy.deepcopy(v) for k, v in run.items() if k in allowed_run}
            # Archived runs are shown and summarised, never executed; normalise the fields that
            # summaries read so an odd or foreign project file cannot break the exports.
            kept["segment_ids"] = [x for x in kept.get("segment_ids", []) if isinstance(x, str)] if isinstance(kept.get("segment_ids"), list) else []
            for name in ("id", "prompt_version"):
                if name in kept and not isinstance(kept[name], str):
                    kept[name] = str(kept[name])
            kept["provenance"] = {k: copy.deepcopy(v) for k, v in (run.get("provenance") if isinstance(run.get("provenance"), dict) else {}).items() if k in allowed_provenance}
            config = run.get("request_configuration", {})
            if isinstance(config, dict):
                kept["request_configuration"] = {k: copy.deepcopy(v) for k, v in config.items() if k in {"provider", "model", "workspace_id", "max_tokens", "temperature", "top_p", "random_seed", "effort", "sampling_application", "chat_tool", "chat_model"}}
            clean_runs.append(kept)
        source_document = value.get("source_document")
        clean_document = None
        if source_document is not None:
            if not isinstance(source_document, dict) or not isinstance(source_document.get("blocks"), list) or len(source_document["blocks"]) > 10000:
                raise WorkflowError("Invalid document-selection record.")
            blocks = []
            for b in source_document["blocks"]:
                if not isinstance(b, dict) or type(b.get("included")) is not bool:
                    raise WorkflowError("Invalid document block record.")
                blocks.append({"id": text_value(b.get("id"), "Block id", 30), "text": text_value(b.get("text"), "Block text", MAX_TEXT), "kind": text_value(b.get("kind", "paragraph"), "Block kind", 40),
                    "reason": text_value(b.get("reason", ""), "Block reason", 2000, True), "included": b["included"], "needs_check": b.get("needs_check") is True, "suggested_exclude": b.get("suggested_exclude") is True})
            clean_document = {"blocks": blocks, "scan_version": text_value(source_document.get("scan_version", "unknown"), "Scanner version", 100),
                "original_sha256": text_value(source_document.get("original_sha256", "unknown"), "Document hash", 100), "confirmed_at": text_value(source_document.get("confirmed_at", ""), "Confirmation time", 100, True)}
        with self.lock:
            self._idle()
            self.project = self._new_project()
            self.project.update({"title": title,
                                 "settings": settings, "segments": clean, "runs": clean_runs, "demo": value.get("demo") is True,
                                 "imported_from_project_id": prior_project_id})
            if clean_document is not None:
                self.project["source_document"] = clean_document
            self.tasks.clear()
            self.inspection = None
            self._audit("project_imported", imported_sha256=digest(value), prior_audit=value.get("audit", []))


def csv_safe(value):
    value = str(value)
    if value.startswith(("\t", "\r")) or value.lstrip().startswith(("=", "+", "-", "@")):
        return "'" + value
    return value


def export_csv(project):
    stream = io.StringIO(newline="")
    writer = csv.writer(stream)
    writer.writerow(["id", "speaker", "source", "forward", "blind_back_translation", "reconciled_draft", "reviewed_translation", "review_status", "reviewer", "review_note", "issues", "review_method", "human_entered_stages"])
    for s in project["segments"]:
        r = s.get("review") or {}
        writer.writerow([csv_safe(x) for x in [s["id"], s["speaker"], s["source"], result_text(s, "forward"), result_text(s, "back"),
                          result_text(s, "reconcile"), r.get("text", ""), r.get("status", "pending"), r.get("reviewer", ""), r.get("note", ""),
                          json.dumps((s.get("reconcile") or {}).get("issues", []), ensure_ascii=False), r.get("method", ""), ";".join(human_entered(s))]] )
    return ("\ufeff" + stream.getvalue()).encode("utf-8")


def audit_events(events):
    """All audit events, including those archived inside imported projects."""
    for event in events or []:
        if not isinstance(event, dict):
            continue
        yield event
        details = event.get("details")
        if isinstance(details, dict) and isinstance(details.get("prior_audit"), list):
            yield from audit_events(details["prior_audit"])


# Describes runs recorded by a pre-release version that drove Copilot Chat through its web page,
# when they appear in an imported project.
COPILOT_HISTORY_LINES = [
    "Some runs in this project were made with the Copilot browser mode of an earlier QualiTranslate version, which controlled a dedicated Chrome/Edge window through the visible Copilot Chat page, not a Microsoft model API.",
    "For each of those batches that version loaded a new chat and confirmed on screen that it was empty before sending; each run's chat_isolation field states what was verified, and temporary_chat_notice records Copilot's own on-screen notice.",
    "A new or temporary chat does not prove that the service has no other context, retrieval or memory; independence equivalent to separate human translators is not claimed. Copilot does not report its underlying model; only the label shown on its model selector is recorded. Sampling settings were not controlled in Copilot mode.",
    "The per-request receipt verifies reply correlation, not translation correctness. The exact text placed in Copilot's message box is retained as transport_prompt.",
]


def copilot_refusal_summary(project):
    events = list(audit_events(project.get("audit", [])))
    sections = sorted({(_text(e["details"].get("stage")), _text(e["details"].get("segment_id"))) for e in events
                       if e.get("action") == "copilot_section_declined" and isinstance(e.get("details"), dict)})
    attempts = sum(1 for e in events if e.get("action") == "copilot_declined")
    entered = {st: [x["id"] for x in project["segments"] if (x.get(st) or {}).get("human_entry")] for st in STAGES}
    return ("Copilot refusals: " + (str(len(sections)) + " section/stage pair(s) Copilot would not translate after isolation: " + ", ".join(i + " (" + st + ")" for st, i in sections) if sections else "none recorded") +
            "; " + str(attempts) + " declined batch attempt(s) in total while isolating them (audit.json). Batches were split and a single section was retried once without neighbouring context; text was never rephrased to pass a filter. "
            "Human-entered results (recorded as human work, not model output): " + json.dumps(entered) + ".")


def human_entered(segment):
    return [st for st in STAGES if (segment.get(st) or {}).get("human_entry")]


def _text(value):
    """A short display string from an imported record value (which may not be a string)."""
    return value.strip()[:200] if isinstance(value, str) else ""


def current_runs(project, stage):
    """The runs whose results are still in the project for this stage (not superseded or cleared)."""
    ids = {(s.get(stage) or {}).get("run_id") for s in project["segments"]} - {None, "", "human_entry"}
    return [r for r in project.get("runs", []) if r.get("stage") == stage and r.get("id") in ids]


def _tool_description(project, stage):
    """How the current results of a stage were produced (from their run records, else settings)."""
    runs = current_runs(project, stage)
    provenance = [r.get("provenance") if isinstance(r.get("provenance"), dict) else {} for r in runs]
    configs = [r.get("request_configuration") if isinstance(r.get("request_configuration"), dict) else {} for r in runs]
    config = {**project["settings"]["stages"][stage], **(configs[-1] if configs else {})}
    if provenance and all(p.get("provider") == "synthetic_demo" for p in provenance):
        return "a built-in synthetic demonstration (no model)"
    providers = sorted({_text(c.get("provider")) for c in configs} - {""}) or [config["provider"]]
    if len(providers) > 1:
        return "a mix of tools across batches (" + ", ".join(providers) + "; see prompts_and_runs.json)"
    provider = providers[0]
    if provider in {"anthropic", "mistral"}:
        requested = sorted({_text(c.get("model")) for c in configs} - {""}) or [_text(config.get("model")) or "[model id]"]
        returned = sorted({_text(p.get("returned_model")) for p in provenance} - {""})
        options = []
        for name, label in (("temperature", "temperature"), ("top_p", "top-p"), ("random_seed", "seed"), ("effort", "reasoning effort")):
            values = sorted({str(c.get(name)) for c in (configs or [config]) if c.get(name) not in (None, "")})
            if values:
                options.append(label + " " + "/".join(values))
        return ("the Claude API (Anthropic)" if provider == "anthropic" else "the Mistral API") + ", model " + " / ".join(requested) + \
            (" (the API reported " + " / ".join(returned) + ")" if returned and returned != requested else "") + \
            (" (" + ", ".join(options) + ")" if options else " (provider default sampling settings)")
    if provider == "copilot":
        labels = sorted({_text(p.get("copilot_model_selector_label")) for p in provenance} - {""})
        if provenance:
            temporary = sum(p.get("chat_isolation") == "new_temporary_chat_verified" for p in provenance)
            chats = (", with each batch in a new temporary chat" if temporary == len(provenance) else ", with each batch in a new chat" +
                     (" (a temporary chat for " + str(temporary) + " of " + str(len(provenance)) + " batches)" if temporary else ""))
        else:
            chats = ", with each batch in a new chat"
        return ("Microsoft 365 Copilot Chat under the institution's licence, operated automatically through its web interface" + chats +
                " (Copilot does not report its underlying model" + ("; its mode selector showed " + " / ".join(repr(x) for x in labels) if labels else "") + ")")
    reported = sorted({_text(p.get("tool_model_reported_by_user")) for p in provenance} - {""})
    tool = " / ".join(reported) or " / ".join(x for x in (_text(config.get("chat_tool")), _text(config.get("chat_model"))) if x) or "[AI chat tool and model]"
    return tool + " (used through its chat interface by copy and paste; model as reported by the user)"


def methods_paragraph(project):
    """A draft methods paragraph, structured after the reporting items in Yunus et al. (2022):
    timing and object of translation, who translated and reviewed, procedure, review scope,
    and how discrepancies were handled. Every statement is derived from the current results
    and their run records; anything the app cannot know is left as a [bracketed] item."""
    settings, segs = project["settings"], project["segments"]
    reporting = settings.get("reporting", {})
    n = len(segs)
    if not n:
        return "[No transcript has been prepared yet.]"
    src, tgt = settings["source_language"], settings["target_language"]
    done = {st: sum(bool(s.get(st)) for s in segs) for st in STAGES}
    human = {st: sum(bool((s.get(st) or {}).get("human_entry")) for s in segs) for st in STAGES}
    tools = {st: _tool_description(project, st) for st in STAGES if done[st] > human[st]}
    runs = {st: current_runs(project, st) for st in STAGES}
    versions = sorted({_text(r.get("prompt_version")) for st in STAGES for r in runs[st]} - {""})
    coverage = lambda st: "" if done[st] == n else " for " + str(done[st]) + " of " + str(n) + " sections"

    parts = []
    if project.get("demo"):
        parts.append("[SYNTHETIC DEMONSTRATION — not a description of real research.]")
    parts.append("Interview material in " + src + " (" + str(n) + " transcript sections) was translated into " + tgt + ". What was translated, and when: " +
                 (reporting.get("translation_scope") or "[what was translated, and at which point in the analysis]").rstrip(".") + ".")
    if reporting.get("dialect_context"):
        parts.append("Dialect or cultural context given to the translation steps: " + reporting["dialect_context"].rstrip(".") + ".")
    parts.append("Translation followed a forward translation, blind back-translation and reconciliation procedure supported by QualiTranslate" +
                 (" (prompt version " + " / ".join(versions) + ")" if versions else "") + ".")
    if not done["forward"]:
        parts.append("[Forward translation not yet run.]")
    else:
        sizes = [len(r["segment_ids"]) if isinstance(r.get("segment_ids"), list) else 0 for r in runs["forward"]]
        retried = sum(bool(r.get("context_omitted")) for st in STAGES for r in runs[st])
        parts.append("Forward translation into " + tgt + coverage("forward") + " was produced with " + tools.get("forward", "a person") +
                     (", in batches of up to " + str(max(sizes)) + " sections with the neighbouring section given as context only" if sizes and max(sizes) else "") +
                     (" (" + str(retried) + " batch(es) were retried as a single section without neighbouring context after the tool declined)" if retried else "") + ".")
    if not done["back"]:
        parts.append("[Blind back-translation not yet run.]")
    else:
        same = "back" in tools and tools.get("back") == tools.get("forward")
        parts.append("The forward translation was then translated back into " + src + coverage("back") + " " +
                     ("with the same tool as the forward translation" if same else "with " + tools.get("back", "a person")) +
                     ", in separate requests that contained only the forward translation (no original text, study context or glossary).")
    if not done["reconcile"]:
        parts.append("[Reconciliation not yet run.]")
    else:
        rec = tools.get("reconcile", "a person")
        who = ("the same tool" if rec == tools.get("forward") and rec == tools.get("back") else
               "the forward-translation tool" if rec == tools.get("forward") else
               "the back-translation tool" if rec == tools.get("back") else rec)
        parts.append("In a reconciliation step" + coverage("reconcile") + ", " + who + " compared the original, the forward translation and the back-translation, flagged possible discrepancies by category and severity, and proposed a revised translation.")
    if settings.get("terminology"):
        parts.append("An agreed bilingual lexicon of " + str(len(settings["terminology"])) + " terms was supplied to the forward and reconciliation steps and checked locally.")
    flagged = {t: sum(severity(s) == t for s in segs if s.get("reconcile")) for t in ("critical", "major", "minor")}
    categories = {}
    for s in segs:
        for issue in (s.get("reconcile") or {}).get("issues", []):
            categories[issue["category"]] = categories.get(issue["category"], 0) + 1
    top = ", ".join(k.replace("idiom_metaphor", "idiom or metaphor").replace("_", " ") for k, _ in sorted(categories.items(), key=lambda x: (-x[1], x[0]))[:3])
    if sum(flagged.values()):
        parts.append("Reconciliation flagged " + str(sum(flagged.values())) + " of " + str(done["reconcile"]) + " sections (" + str(flagged["critical"]) + " critical, " +
                     str(flagged["major"]) + " major, " + str(flagged["minor"]) + " minor)" + ("; categories flagged most often: " + top if top else "") + ".")
    entered = sum(bool(human_entered(s)) for s in segs)
    if entered:
        parts.append(str(entered) + " section(s) that the AI tool declined to translate were translated by a person and are marked as human work.")
    reviews = [s["review"] for s in segs if s.get("review")]
    background = reporting.get("reviewer_background") or "[name/role, language competence and domain expertise]"
    if reviews:
        methods = {m: sum(r.get("method", "individual") == m for r in reviews) for m in ("individual", "quick_accept", "batch_minor")}
        people = len({_text(r.get("reviewer")) for r in reviews} - {""})
        retained = sum(r.get("status") == "reviewed_uncertainty_retained" for r in reviews)
        detail = []
        if methods["individual"]:
            detail.append(str(methods["individual"]) + " after individual review against the original")
        if methods["quick_accept"]:
            detail.append(str(methods["quick_accept"]) + " approved unchanged after reading the section")
        if methods["batch_minor"]:
            detail.append(str(methods["batch_minor"]) + " accepted unchanged in a batch of sections flagged only as minor, without individual review")
        parts.append("Decisions were recorded for " + str(len(reviews)) + " of " + str(n) + " sections by " + ("one reviewer" if people <= 1 else str(people) + " reviewers") +
                     " (" + background + "): " + "; ".join(detail) + "." +
                     (" For " + str(retained) + " section(s) the reviewer recorded that uncertainty about the meaning remains." if retained else ""))
    else:
        parts.append("[Human review not yet recorded: describe who (" + background + ") reviewed the proposed translations against the original, and how.]")
    parts.append(reporting.get("decision_summary") or "[How unresolved discrepancies and translation dilemmas were discussed and decided, and any that remain.]")
    parts.append("Back-translation was used as a diagnostic aid, not as proof of equivalence. Prompts, model replies, edits and review decisions were kept in an audit record.")
    return " ".join(parts)


def methods_record(project):
    settings = project["settings"]
    counts = {s: sum(bool(x.get(s)) for x in project["segments"]) for s in STAGES}
    reviewed = sum(bool(x.get("review")) for x in project["segments"])
    provenance = [r["provenance"] for r in project["runs"]]
    return "\n".join([
        "DRAFT METHODS PARAGRAPH (generated from this project; check every statement and complete the [bracketed] items before use)",
        methods_paragraph(project), "",
        "QualiTranslate methods record — factual workflow summary, not a validation claim",
        "SYNTHETIC DEMONSTRATION" if project["demo"] else "Research project: " + project["title"],
        "Created: " + project["created_at"], "Exported: " + now(), "Application version: " + APP_VERSION,
        "Prompt version: " + PROMPT_VERSION,
        "Languages: " + settings["source_language"] + " -> " + settings["target_language"],
        "Segments: " + str(len(project["segments"])),
        "Completed stages: " + json.dumps(counts), "Recorded review decisions: " + str(reviewed),
        "Decision methods: " + json.dumps({m: sum((x.get("review") or {}).get("method", "individual") == m for x in project["segments"] if x.get("review")) for m in ("individual", "quick_accept", "batch_minor")}),
        "Batch-minor acceptance is an explicit reviewer decision, not evidence of individual bilingual examination.",
        "Team reporting: " + json.dumps(settings.get("reporting", {}), ensure_ascii=False),
        "Terminology lexicon: " + json.dumps(settings.get("terminology", []), ensure_ascii=False),
        "Local identifier check of the source sections (warning only; the app does not remove or change text, and it cannot detect names or places): " + identifier_summary(identifier_scan(project["segments"])) + ".",
        "Stage settings: " + json.dumps({st: {k: v for k, v in settings["stages"][st].items() if v not in (None, "")} for st in STAGES}, ensure_ascii=False) + ".",
        "Document selection: " + (str(sum(not b["included"] for b in project["source_document"]["blocks"])) + " blocks excluded by user confirmation; excluded text retained only in project record, not translation prompts." if project.get("source_document") else "No scanner selection record; source entered directly or demonstration."),
        "Forward translation, blind back-translation, and reconciliation are separate operations.",
        "API calls are stateless; each batch is a separate request.",
    ] + (COPILOT_HISTORY_LINES if any((r.get("provenance") or {}).get("provider") == "copilot_browser" for r in project.get("runs", []) if isinstance(r, dict)) else []) +
        ([copilot_refusal_summary(project)] if any(e.get("action") in {"copilot_declined", "copilot_section_declined"} for e in audit_events(project.get("audit", []))) else []) + [
        "Back-translation requests contain only target-language translations and the language pair; no original-source text, source glossary, study context, or translator notes.",
        "In manual mode, this blindness depends on using a fresh conversation without source files/history for every back-translation batch.",
        "Reconciliation compares the source, forward translation and blind back-translation. It remains a draft until a named reviewer records a review.",
        "Back-translation is a diagnostic aid; this workflow does not establish psychometric validity, semantic equivalence, or inter-translator independence.",
        "Actual model / tool records (manual model labels are user reported):", json.dumps(provenance, ensure_ascii=False, indent=2),
        "Exact prompts and responses are retained in project.json and prompts_and_runs.json.",
        "There is no universal prescribed percentage of bilingual checking in this application. Record the review scope actually performed.",
    ]) + "\n"


def export_html(project):
    esc = lambda x: html.escape(str(x), quote=True)
    rows = []
    for s in project["segments"]:
        review = s.get("review") or {}
        issues = (s.get("reconcile") or {}).get("issues", [])
        issue_text = "\n".join(x["severity"].upper() + " / " + x["category"] + ": " + x["detail"] for x in issues)
        rows.append("<tr>" + "".join("<td>" + esc(v).replace("\n", "<br>") + "</td>" for v in [s["id"] + "\n" + s["speaker"], s["source"]] + [("[HUMAN-ENTERED] " if st in human_entered(s) else "") + result_text(s, st) for st in STAGES] + [ review.get("text", ""), review.get("status", "PENDING HUMAN REVIEW") + " / " + review.get("method", "") + "\n" + review.get("reviewer", "") + "\n" + review.get("note", "") + "\n" + issue_text]) + "</tr>")
    banner = "SYNTHETIC DEMONSTRATION — NOT A LIVE TRANSLATION" if project["demo"] else "Drafts remain provisional until an explicit reviewer decision; batch acceptance is identified separately."
    return ("<!doctype html><html lang='en'><meta charset='utf-8'><meta name='viewport' content='width=device-width'><title>" + esc(project["title"]) + "</title><style>body{font:15px/1.55 system-ui;color:#152e34;margin:28px}h1{font-size:28px}table{border-collapse:collapse;width:100%}th,td{border:1px solid #ced8d7;padding:12px;text-align:left;vertical-align:top;min-width:160px}th{background:#eaf3ef}pre{white-space:pre-wrap;font:12px/1.5 monospace}.notice{background:#fff0d8;padding:12px}@media print{body{margin:0;font-size:10px}th,td{min-width:0;padding:6px}tr{break-inside:avoid}@page{size:landscape}}</style><h1>" + esc(project["title"]) + "</h1><p class='notice'>" + esc(banner) + "</p><p>" + esc(project["settings"]["source_language"]) + " → " + esc(project["settings"]["target_language"]) + "</p><table><thead><tr>" + "".join("<th>" + x + "</th>" for x in ["Segment / speaker", "Source", "Forward", "Blind back-translation", "Reconciled draft", "Accepted / reviewed text", "Decision method / issues"]) + "</tr></thead><tbody>" + "".join(rows) + "</tbody></table><h2>Workflow record</h2><pre>" + esc(methods_record(project)) + "</pre></html>").encode()


def export_bundle(project):
    stream = io.BytesIO()
    with zipfile.ZipFile(stream, "w", zipfile.ZIP_DEFLATED) as archive:
        archive.writestr("project.json", json.dumps(project, ensure_ascii=False, indent=2))
        archive.writestr("translations.csv", export_csv(project))
        archive.writestr("parallel_review.html", export_html(project))
        archive.writestr("methods_record.txt", methods_record(project))
        archive.writestr("prompts_and_runs.json", json.dumps(project["runs"], ensure_ascii=False, indent=2))
        archive.writestr("audit.json", json.dumps(project["audit"], ensure_ascii=False, indent=2))
    return stream.getvalue()
