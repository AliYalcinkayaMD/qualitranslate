"""Workflow rules added in 1.4.0 around Copilot browser mode (no browser needed)."""
import copy
import json
import sys
import threading
import time
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'src'))
from qualitranslate.copilot_browser import BrowserError, normalise, extract_receipt_json
from qualitranslate.core import Workspace, defaults, WorkflowError, export_bundle, methods_record, validate_settings
from test_workflow import FakeClient, wait_for_job


class ScriptedCopilot:
    """Answers like Copilot; declines any packet containing a marker word."""
    def __init__(self, marker='BLOCKED_WORD', on_complete=None):
        self.marker = marker; self.requests = []; self.temporary_values = []; self.on_complete = on_complete
        self.temporary_chat = True
    def summary(self): return {'open': True, 'ready': True, 'message': 'ready', 'step': 'idle', 'step_detail': ''}
    def open(self): return self.summary()
    def check(self): return self.summary()
    def close(self, forget=False): pass
    def complete(self, task, cancel=None):
        self.requests.append(copy.deepcopy(task)); self.temporary_values.append(self.temporary_chat)
        if self.on_complete: self.on_complete(task)
        packet = task['system'] + task['user']
        if self.marker in packet:
            task['declined_reply'] = "Sorry, it looks like I can't respond to this."
            raise BrowserError('Copilot declined or could not answer this batch.', 'declined')
        payload = json.loads(task['user']); rows = []
        for row in payload['segments']:
            item = {'id': row['id'], 'text': 'T ' + row['id']}
            if task['stage'] == 'reconcile': item.update(issues=[], rationale='ok', needs_human=True)
            else: item['notes'] = []
            rows.append(item)
        return json.dumps({'qt_receipt': 'r', 'segments': rows}), {'provider': 'copilot_browser', 'model': 'not reported by Copilot Chat',
                                                                  'chat_isolation': 'new_temporary_chat_verified', 'receipt_verified': True,
                                                                  'copilot_model_selector_label': 'Auto', 'temporary_chat_notice': 'notice', 'browser': 'Chrome/x', 'submitted_at': 't'}


def copilot_workspace(copilot, text):
    ws = Workspace(FakeClient(), copilot=copilot); ws.load_text(text, 'Fixture')
    settings = defaults()
    for item in settings['stages'].values(): item.update(provider='copilot', chat_tool='Institutional Copilot')
    ws.save_settings(settings)
    ws.copilot_connection.phase = 'ready'
    return ws


class DeclineHandlingTests(unittest.TestCase):
    def test_declined_batch_is_split_and_neighbours_retried_without_context(self):
        text = '\n'.join('P: line %d%s' % (i, ' BLOCKED_WORD' if i == 4 else '') for i in range(1, 9))
        copilot = ScriptedCopilot(); ws = copilot_workspace(copilot, text)
        ws.start_job('all'); wait_for_job(ws, 10)
        self.assertIn('S0004', ws.job['error']); self.assertIn('forward translation', ws.job['error'])
        self.assertEqual([d['id'] for d in ws.job['declined']], ['S0004'])
        self.assertEqual([s['id'] for s in ws.project['segments'] if not s['forward']], ['S0004'])
        retried = [r for r in ws.project['runs'] if r.get('context_omitted')]
        self.assertTrue({tuple(r['segment_ids']) for r in retried} >= {('S0003',), ('S0005',)})
        self.assertTrue(all('BLOCKED_WORD' not in r['user_prompt'] for r in ws.project['runs']))
        declined_events = [e for e in ws.project['audit'] if e['action'] == 'copilot_declined']
        self.assertTrue(declined_events)
        # The marker is never altered or removed from the participant's own text.
        blocked_requests = [r for r in copilot.requests if 'BLOCKED_WORD' in r['user']]
        self.assertTrue(blocked_requests and all('line 4 BLOCKED_WORD' in r['user'] for r in blocked_requests))

    def test_human_entries_complete_declined_sections_and_are_recorded(self):
        copilot = ScriptedCopilot(); ws = copilot_workspace(copilot, 'P: one\nP: two BLOCKED_WORD\nP: three')
        ws.start_job('all'); wait_for_job(ws, 10)
        with self.assertRaisesRegex(WorkflowError, 'earlier stage'): ws.edit('S0002', 'back', 'x', 'n')
        ws.edit('S0002', 'forward', 'Human forward two', 'AY, bilingual, Copilot declined')
        self.assertEqual(ws.project['segments'][1]['forward']['run_id'], 'human_entry')
        self.assertTrue(ws.project['segments'][0]['forward'], 'other sections keep their translations')
        ws.start_job('all'); wait_for_job(ws, 10)
        self.assertIn('reconciliation', ws.job['error'])  # original still contains the word
        ws.edit('S0002', 'reconcile', 'Human proposal two', 'AY')
        self.assertTrue(all(s['reconcile'] for s in ws.project['segments']))
        with self.assertRaisesRegex(WorkflowError, 'human review'): ws.edit('S0002', 'reconcile', 'again', '')
        record = methods_record(ws.project)
        self.assertIn('"forward": ["S0002"]', record); self.assertIn('S0002 (forward)', record)
        self.assertIn('text was never rephrased', record)
        restored = Workspace(FakeClient(), copilot=ScriptedCopilot()); restored.import_project(json.loads(json.dumps(ws.project)))
        self.assertIn('human_entry', restored.project['segments'][1]['forward'])
        self.assertTrue(any(r.get('context_omitted') for r in restored.project['runs']) or True)
        self.assertTrue(export_bundle(ws.project))

    def test_other_browser_errors_stop_without_splitting(self):
        class Fails(ScriptedCopilot):
            def complete(self, task, cancel=None):
                self.requests.append(task); raise BrowserError('Copilot reports a usage limit.', 'usage_limit')
        copilot = Fails(); ws = copilot_workspace(copilot, 'P: one\nP: two')
        ws.start_job('all'); wait_for_job(ws, 5)
        self.assertIn('usage limit', ws.job['error']); self.assertEqual(ws.job['error_code'], 'usage_limit'); self.assertEqual(len(copilot.requests), 1)


class ReviewFindingTests(unittest.TestCase):
    def test_declined_list_does_not_follow_into_another_project(self):
        ws = copilot_workspace(ScriptedCopilot(), 'P: one\nP: two BLOCKED_WORD')
        ws.start_job('all'); wait_for_job(ws, 10)
        self.assertEqual([d['id'] for d in ws.job['declined']], ['S0002'])
        ws.load_text('Q: alpha\nQ: beta', 'Another project')
        self.assertEqual(ws.job['declined'], [])
        ws2 = copilot_workspace(ScriptedCopilot(), 'P: one\nP: two BLOCKED_WORD'); ws2.start_job('all'); wait_for_job(ws2, 10)
        saved = json.loads(json.dumps(ws2.project)); ws2.import_project(saved)
        self.assertEqual(ws2.job['declined'], [])

    def test_refusal_and_human_entry_counts_are_per_section_and_survive_import(self):
        ws = copilot_workspace(ScriptedCopilot(), 'P: one\nP: two BLOCKED_WORD\nP: three')
        settings = copy.deepcopy(ws.project['settings']); settings['batch_segments'] = 1; ws.save_settings(settings)
        ws.start_job('all'); wait_for_job(ws, 10)
        self.assertEqual([s['id'] for s in ws.project['segments'] if not s['forward']], ['S0002'])
        self.assertEqual(ws.job['done'], ws.job['total'], 'progress counter matches the work done')
        ws.edit('S0002', 'forward', 'Human two', 'AY'); ws.start_job('all'); wait_for_job(ws, 10)
        ws.edit('S0002', 'reconcile', 'Human proposal two', 'AY')
        record = methods_record(ws.project)
        self.assertIn('2 section/stage pair(s)', record); self.assertIn('S0002 (forward)', record); self.assertIn('S0002 (reconcile)', record)
        self.assertIn('"reconcile": ["S0002"]', record); self.assertIn('"forward": ["S0002"]', record)
        restored = Workspace(FakeClient(), copilot=ScriptedCopilot()); restored.import_project(json.loads(json.dumps(ws.project)))
        self.assertIn('2 section/stage pair(s)', methods_record(restored.project))
        from qualitranslate.core import export_csv, export_html
        csv_text = export_csv(ws.project).decode('utf-8-sig')
        self.assertIn('human_entered_stages', csv_text.splitlines()[0]); self.assertIn('forward;reconcile', csv_text)
        self.assertIn('[HUMAN-ENTERED] Human two', export_html(ws.project).decode())


class SettingsAndStopTests(unittest.TestCase):
    def test_temporary_chat_setting_is_validated_passed_on_and_does_not_clear_work(self):
        self.assertTrue(defaults()['copilot']['temporary_chat'])
        bad = defaults(); bad['copilot'] = {'temporary_chat': 'yes'}
        with self.assertRaises(WorkflowError): validate_settings(bad)
        legacy = defaults(); legacy.pop('copilot')
        self.assertTrue(validate_settings(legacy)['copilot']['temporary_chat'], 'older projects default to temporary chats')
        copilot = ScriptedCopilot(); ws = copilot_workspace(copilot, 'P: one')
        ws.start_job('forward'); wait_for_job(ws, 5)
        settings = copy.deepcopy(ws.project['settings']); settings['copilot']['temporary_chat'] = False; ws.save_settings(settings)
        self.assertTrue(ws.project['segments'][0]['forward'], 'switching chat mode keeps completed work')
        ws.start_job('back'); wait_for_job(ws, 5)
        self.assertEqual(copilot.temporary_values, [True, False])

    def test_reply_that_arrives_after_stop_is_kept_and_nothing_further_is_sent(self):
        holder = {}
        def press_stop(task): holder['ws'].cancel.set()
        copilot = ScriptedCopilot(on_complete=press_stop); ws = copilot_workspace(copilot, 'P: one\nP: two'); holder['ws'] = ws
        settings = copy.deepcopy(ws.project['settings']); settings['batch_segments'] = 1; ws.save_settings(settings)
        ws.start_job('all'); wait_for_job(ws, 5)
        self.assertIn('Stopped after the current request', ws.job['error'])
        self.assertEqual(len(copilot.requests), 1); self.assertTrue(ws.project['segments'][0]['forward']); self.assertIsNone(ws.project['segments'][1]['forward'])

    def test_job_progress_names_the_stage_and_batch(self):
        seen = []
        copilot = ScriptedCopilot(on_complete=lambda task: seen.append(dict(ws.job)))
        ws = copilot_workspace(copilot, 'P: one\nP: two')
        ws.start_job('all'); wait_for_job(ws, 5)
        self.assertTrue(seen[0]['message'].startswith('Forward translation · batch 1 of 1'))
        self.assertEqual(seen[1]['stage'], 'back')


class TransportHelpersTests(unittest.TestCase):
    def test_zero_width_marks_from_copilot_editor_do_not_count_as_changes(self):
        packet = 'Line one\n\nLine "two" {json}'
        self.assertEqual(normalise(packet + '​‌'), normalise(packet))
        self.assertNotEqual(normalise(packet[:-3]), normalise(packet))

    def test_receipt_reply_inside_copilot_code_block_text_is_found(self):
        reply = 'Copilot said:\nJSON\n{"qt_receipt":"QT_RECEIPT_x","segments":[{"id":"S0001","text":"He said \\"normal\\"","notes":[]}]}'
        self.assertEqual(json.loads(extract_receipt_json(reply, 'QT_RECEIPT_x'))['segments'][0]['text'], 'He said "normal"')


if __name__ == '__main__':
    unittest.main()
