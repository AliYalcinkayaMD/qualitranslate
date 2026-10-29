"""1.5.0: per-stage temperature / effort, a separate back-translation model, the identifier
check and the draft methods paragraph."""
import copy
import json
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'src'))
from qualitranslate.core import (APIClient, DEMO_SOURCE, Workspace, WorkflowError, defaults, export_bundle,
                                 methods_paragraph, methods_record, validate_settings)
from qualitranslate.privacy import find_identifiers, identifier_scan
from test_workflow import FakeClient, wait_for_job


class Capture(APIClient):
    def __init__(self, reply):
        super().__init__(); self.reply = reply; self.payloads = []
    def _request(self, provider, key, workspace, path, payload=None):
        self.payloads.append(payload); return self.reply, 'rid'


class ProviderOptionTests(unittest.TestCase):
    def task(self, **config):
        ws = Workspace(FakeClient()); ws.load_text('P: one')
        settings = defaults(); settings['stages']['forward'].update(model='m', **config); ws.save_settings(settings)
        return ws.prepare_task('forward')

    def test_claude_sends_temperature_and_effort_only_when_set(self):
        client = Capture({'content': [{'type': 'text', 'text': '{}'}], 'stop_reason': 'end_turn'})
        client.complete(self.task(provider='anthropic'), 'k')
        self.assertNotIn('temperature', client.payloads[-1]); self.assertNotIn('output_config', client.payloads[-1])
        client.complete(self.task(provider='anthropic', temperature=0.2, effort='high'), 'k')
        self.assertEqual(client.payloads[-1]['temperature'], 0.2); self.assertEqual(client.payloads[-1]['output_config'], {'effort': 'high'})
        self.assertNotIn('random_seed', client.payloads[-1])

    def test_mistral_reasoning_effort(self):
        client = Capture({'choices': [{'finish_reason': 'stop', 'message': {'content': '{}'}}]})
        client.complete(self.task(provider='mistral', effort='high', top_p=0.9), 'k')
        self.assertEqual(client.payloads[-1]['reasoning_effort'], 'high'); self.assertEqual(client.payloads[-1]['top_p'], 0.9)
        client.complete(self.task(provider='mistral'), 'k'); self.assertNotIn('reasoning_effort', client.payloads[-1])

    def test_old_projects_without_effort_still_load(self):
        s = defaults()
        for c in s['stages'].values(): c.pop('effort')
        self.assertEqual(validate_settings(s)['stages']['back']['effort'], '')


class StageModelTests(unittest.TestCase):
    def test_changing_only_the_back_translation_model_keeps_forward_work(self):
        ws = Workspace(FakeClient()); ws.load_text('P: one\nP: two')
        settings = defaults()
        for c in settings['stages'].values(): c.update(provider='mistral', model='model-a')
        ws.save_settings(settings); ws.keys['mistral'] = 'k'
        ws.start_job('all'); wait_for_job(ws, 10)
        self.assertTrue(all(s['reconcile'] for s in ws.project['segments']))
        changed = copy.deepcopy(ws.project['settings']); changed['stages']['back']['model'] = 'model-b'; ws.save_settings(changed)
        self.assertTrue(all(s['forward'] for s in ws.project['segments']), 'forward translations are kept')
        self.assertTrue(all(s['back'] is None and s['reconcile'] is None for s in ws.project['segments']))
        self.assertEqual(ws.project['audit'][-1]['details']['invalidated_from_stage'], 'back')
        changed2 = copy.deepcopy(ws.project['settings']); changed2['target_language'] = 'German'; ws.save_settings(changed2)
        self.assertTrue(all(s['forward'] is None for s in ws.project['segments']), 'a language change clears everything')

    def test_paragraph_names_different_back_translation_model(self):
        ws = Workspace(FakeClient()); ws.load_text('P: one')
        settings = defaults()
        for c in settings['stages'].values(): c.update(provider='mistral', model='model-a')
        settings['stages']['back']['model'] = 'model-b'; settings['stages']['reconcile']['effort'] = 'high'
        ws.save_settings(settings); ws.keys['mistral'] = 'k'
        ws.start_job('all'); wait_for_job(ws, 10)
        text = methods_paragraph(ws.project)
        self.assertIn('back into Danish with the Mistral API', text)
        self.assertIn('reasoning effort high', text)
        self.assertIn('[Human review not yet recorded', text)
        self.assertIn('DRAFT METHODS PARAGRAPH', methods_record(ws.project))


class IdentifierTests(unittest.TestCase):
    def test_patterns_flag_common_identifiers_but_not_timestamps(self):
        kinds = lambda t: [k for k, _, _ in find_identifiers(t)]
        self.assertEqual(kinds('Mail anna@example.dk or +45 12 34 56 78'), ['email', 'phone'])
        self.assertEqual(kinds('CPR 010203-1234'), ['cpr'])
        self.assertEqual(kinds('[inaudible 00:12:31] in 2019, 45 minutes'), [])
        self.assertEqual(kinds('born 12.03.1985, see www.example.org'), ['date', 'url'])

    def test_scan_is_a_warning_and_never_changes_text(self):
        ws = Workspace(FakeClient()); ws.load_text('P: call me on 12345678\nP: fine')
        before = json.dumps(ws.project['segments'])
        check = ws.snapshot()['identifier_check']
        self.assertEqual(check['segments'], [{'id': 'S0001', 'kinds': ['phone']}])
        self.assertEqual(before, json.dumps(ws.project['segments']))
        self.assertIn('1 phone-like number in 1 section(s): S0001', methods_record(ws.project))
        self.assertEqual(identifier_scan([{'id': 'S1', 'source': 'nothing here'}])['total'], 0)

    def test_demo_paragraph_is_labelled_synthetic(self):
        ws = Workspace(FakeClient()); ws.load_text(DEMO_SOURCE, 'demo', demo=True)
        ws.start_job('all', demo=True); wait_for_job(ws, 5)
        self.assertTrue(methods_paragraph(ws.project).startswith('[SYNTHETIC DEMONSTRATION'))
        self.assertTrue(export_bundle(ws.project))


if __name__ == '__main__':
    unittest.main()


class ParagraphAccuracyTests(unittest.TestCase):
    """Statements in the draft paragraph must match the results that are currently in the project."""

    def run_all(self, ws, stage='all'):
        ws.start_job(stage); wait_for_job(ws, 10)

    def api_workspace(self, text='P: one\nP: two'):
        ws = Workspace(FakeClient()); ws.load_text(text); ws.keys.update(mistral='k', anthropic='k')
        return ws

    def test_superseded_runs_are_not_credited(self):
        ws = self.api_workspace(); settings = defaults()
        for c in settings['stages'].values(): c.update(provider='anthropic', model='claude-a')
        ws.save_settings(settings); self.run_all(ws)
        changed = copy.deepcopy(ws.project['settings']); changed['stages']['back'].update(provider='mistral', model='mistral-b'); ws.save_settings(changed)
        self.run_all(ws)
        text = methods_paragraph(ws.project)
        self.assertIn('back into Danish with the Mistral API, model mistral-b', text)
        self.assertNotIn('claude-a mistral-b', text); self.assertNotIn('model claude-a / mistral-b', text)
        self.assertIn('the forward-translation tool compared', text)

    def test_stages_not_run_are_not_described_as_done(self):
        ws = self.api_workspace(); settings = defaults()
        for c in settings['stages'].values(): c.update(provider='mistral', model='m')
        ws.save_settings(settings); self.run_all(ws, 'forward')
        text = methods_paragraph(ws.project)
        self.assertIn('[Blind back-translation not yet run.]', text); self.assertIn('[Reconciliation not yet run.]', text)
        self.assertEqual(methods_paragraph(Workspace(FakeClient()).project), '[No transcript has been prepared yet.]')

    def test_batch_acceptance_is_not_called_review(self):
        ws = self.api_workspace(); settings = defaults()
        for c in settings['stages'].values(): c.update(provider='mistral', model='m')
        ws.save_settings(settings); self.run_all(ws)
        for s in ws.project['segments']:
            s['reconcile']['issues'] = [{'category': 'other', 'severity': 'minor', 'detail': 'x', 'needs_human': True, 'origin': 'model'}]
        ws.review_batch_minor(['S0001'], 'Reviewer one', ws.project['revision'])
        ws.review_batch_minor(['S0002'], 'Reviewer two', ws.project['revision'])
        text = methods_paragraph(ws.project)
        self.assertIn('by 2 reviewers', text); self.assertIn('without individual review', text)
        self.assertNotIn('compared the proposed translations', text)

    def test_mixed_temporary_chats_and_odd_imported_values(self):
        ws = self.api_workspace('P: one')
        project = ws.project
        project['segments'][0]['forward'] = {'text': 'x', 'notes': [], 'run_id': 'r1'}
        project['runs'] = [{'id': 'r1', 'stage': 'forward', 'segment_ids': ['S0001'], 'prompt_version': '1.1.0',
                            'request_configuration': {'provider': 'copilot', 'model': None},
                            'provenance': {'provider': 'copilot_browser', 'chat_isolation': 'new_empty_chat_verified', 'copilot_model_selector_label': 7}}]
        text = methods_paragraph(project)
        self.assertIn('with each batch in a new chat (Copilot', text); self.assertIn('prompt version 1.1.0', text)
        restored = Workspace(FakeClient()); restored.import_project(json.loads(json.dumps(project)))
        self.assertTrue(export_bundle(restored.project))

    def test_identifier_phrases_are_plural_aware(self):
        scan = identifier_scan([{'id': 'S1', 'source': 'a@b.dk c@d.dk 12345678'}])
        self.assertEqual(scan['phrases'], ['2 e-mail addresses', '1 phone-like number'])


class RobustnessTests(unittest.TestCase):
    def test_odd_imported_records_do_not_break_exports(self):
        ws = Workspace(FakeClient()); ws.load_text('P: one')
        project = json.loads(json.dumps(ws.project))
        project['segments'][0]['forward'] = {'text': 'x', 'notes': [], 'run_id': 'r1'}
        project['runs'] = [{'id': 'r1', 'stage': 'forward', 'segment_ids': 'S0001', 'provenance': 'odd', 'prompt_version': 5}]
        project['audit'] = [{'action': 'project_imported', 'details': {'prior_audit': [{'action': 'copilot_section_declined', 'details': {'stage': 3, 'segment_id': None}}]}},
                            {'action': 'x', 'details': 'not an object'}]
        restored = Workspace(FakeClient()); restored.import_project(project)
        self.assertTrue(export_bundle(restored.project))
        self.assertIn('Forward translation', methods_paragraph(restored.project))

    def test_csv_guards_leading_tab_and_formula(self):
        from qualitranslate.core import csv_safe
        self.assertEqual(csv_safe('\t=1'), "'\t=1"); self.assertEqual(csv_safe('  =1'), "'  =1"); self.assertEqual(csv_safe('ok'), 'ok')


class CopilotRemovalTests(unittest.TestCase):
    """The automatic Copilot browser mode was removed; old projects must still load."""

    def old_copilot_project(self):
        ws = Workspace(FakeClient()); ws.load_text('P: one')
        project = json.loads(json.dumps(ws.project))
        for config in project['settings']['stages'].values():
            config.update(provider='copilot', chat_tool='Institutional Copilot')
        project['settings']['copilot'] = {'temporary_chat': True}
        project['segments'][0]['forward'] = {'text': 'x', 'notes': [], 'run_id': 'r1'}
        project['runs'] = [{'id': 'r1', 'stage': 'forward', 'segment_ids': ['S0001'], 'prompt_version': '1.1.0',
                            'request_configuration': {'provider': 'copilot', 'model': ''},
                            'provenance': {'provider': 'copilot_browser', 'chat_isolation': 'new_temporary_chat_verified', 'copilot_model_selector_label': 'Auto'}}]
        return project

    def test_old_copilot_projects_import_as_copy_and_paste(self):
        restored = Workspace(FakeClient()); restored.import_project(self.old_copilot_project())
        stages = restored.project['settings']['stages']
        self.assertTrue(all(c['provider'] == 'manual' and c['chat_tool'] == 'Institutional Copilot' for c in stages.values()))
        self.assertNotIn('copilot', restored.project['settings'])
        record = methods_record(restored.project)
        self.assertIn('Copilot browser mode of an earlier QualiTranslate version', record)
        self.assertIn('each batch in a new temporary chat', methods_paragraph(restored.project))
        self.assertTrue(export_bundle(restored.project))

    def test_projects_without_copilot_runs_do_not_mention_it(self):
        ws = Workspace(FakeClient()); ws.load_text(DEMO_SOURCE, 'demo', demo=True)
        ws.start_job('all', demo=True); wait_for_job(ws, 5)
        self.assertNotIn('Copilot', methods_record(ws.project))
        self.assertNotIn('copilot', ws.snapshot())
        self.assertNotIn('declined', ws.job)
