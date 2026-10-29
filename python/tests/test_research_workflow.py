import base64
import copy
import io
import json
import unittest
import sys
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'src'))
import zipfile
from qualitranslate.core import (APIClient,DEMO_SOURCE,Workspace,WorkflowError,build_task,defaults,export_csv,methods_record,read_document,validate_settings)
from qualitranslate.document_tools import scan_document
from qualitranslate.quality import severity
from test_workflow import wait_for_job


def completed_demo():
    ws=Workspace();ws.load_text(DEMO_SOURCE,'Synthetic research demo',demo=True)
    ws.start_job('all',demo=True);wait_for_job(ws)
    assert not ws.job['error']
    return ws


def word_file(body):
    out=io.BytesIO()
    with zipfile.ZipFile(out,'w') as z:z.writestr('word/document.xml','<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>'+body+'</w:body></w:document>')
    return base64.b64encode(out.getvalue()).decode()


class DocumentSelectionTests(unittest.TestCase):
    def setUp(self):
        self.ws=Workspace()
        self.text='Study ID: ADMIN_SECRET_91\nDate: ADMIN_SECRET_92\n\nInterviewer: What mattered?\nParticipant A: Less pain.\nAppendix: CODING_SECRET_93\nCoding interpretation follows.'

    def test_local_detection_and_confirmed_exclusions_do_not_enter_prompts(self):
        inspected=self.ws.inspect_document(self.text)
        self.assertGreaterEqual(inspected['suspected'],4)
        chosen=[b['id'] for b in inspected['blocks'] if not b['needs_check']]
        self.assertEqual(len(chosen),2)
        self.ws.import_selected(inspected['inspection_id'],chosen,'Interview',defaults())
        prompt=self.ws.prepare_task('forward')['user']
        for secret in ['ADMIN_SECRET_91','ADMIN_SECRET_92','CODING_SECRET_93','Coding interpretation follows']:
            self.assertNotIn(secret,prompt)
        self.assertIn('Less pain.',prompt)
        self.assertTrue(any(not b['included'] for b in self.ws.project['source_document']['blocks']))
        self.assertIn('blocks excluded by user confirmation',methods_record(self.ws.project))

    def test_scanning_never_mutates_current_project(self):
        self.ws.load_text('P: Existing interview','Existing')
        before=copy.deepcopy(self.ws.project)
        self.ws.inspect_document(self.text)
        self.assertEqual(self.ws.project,before)

    def test_user_can_keep_suspected_material(self):
        d=self.ws.inspect_document(self.text)
        self.ws.import_selected(d['inspection_id'],[b['id'] for b in d['blocks']],'Keep all',defaults())
        self.assertIn('ADMIN_SECRET_91',json.dumps(self.ws.project['segments']))

    def test_invalid_selection_is_atomic(self):
        self.ws.load_text('P: Existing','Existing');before=copy.deepcopy(self.ws.project)
        d=self.ws.inspect_document(self.text)
        for selected in [[],['B0001','B0001'],['B9999']]:
            with self.subTest(selected=selected),self.assertRaises(WorkflowError):
                self.ws.import_selected(d['inspection_id'],selected,'New',defaults())
            self.assertEqual(before,self.ws.project)

    def test_expired_scan_and_invalid_settings_cannot_replace_work(self):
        self.ws.load_text('P: Existing','Existing');before=copy.deepcopy(self.ws.project)
        d=self.ws.inspect_document(self.text);self.ws.inspect_document('P: Another')
        with self.assertRaises(WorkflowError):self.ws.import_selected(d['inspection_id'],['B0001'],'New',defaults())
        self.assertEqual(before,self.ws.project)

    def test_plain_narrative_is_not_automatically_removed(self):
        d=scan_document('I had pain after surgery.\n\nIt slowly improved, but I was unsure.')
        self.assertFalse(any(b['suggested_exclude'] for b in d['blocks']))

    def test_spoken_marker_and_timestamps_are_retained(self):
        d=scan_document('Participant A: [00:12:04] [pause] I do not know.\nIt is hard to explain.')
        self.assertFalse(d['suspected']);self.assertIn('[pause]',d['blocks'][0]['text'])

    def test_word_tables_are_suggested_separately(self):
        data=word_file('<w:p><w:r><w:t>Participant: My pain improved.</w:t></w:r></w:p><w:tbl><w:tr><w:tc><w:p><w:r><w:t>Age</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>42</w:t></w:r></w:p></w:tc></w:tr></w:tbl>')
        doc=read_document('interview.docx',data);d=scan_document(doc['text'],doc['blocks'])
        self.assertEqual(d['blocks'][-1]['kind'],'table');self.assertTrue(d['blocks'][-1]['suggested_exclude'])
        self.assertFalse(d['blocks'][0]['suggested_exclude'])

    def test_word_question_answer_table_can_remain_interview(self):
        data=word_file('<w:tbl><w:tr><w:tc><w:p><w:r><w:t>Participant A</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>It was difficult.</w:t></w:r></w:p></w:tc></w:tr></w:tbl>')
        doc=read_document('interview.docx',data);self.assertEqual(doc['blocks'][0]['kind'],'dialogue_table')
        d=scan_document(doc['text'],doc['blocks']);self.assertFalse(d['suggested_exclusions'])
        self.assertIn('Participant A: It was difficult.',doc['text'])

    def test_document_decisions_round_trip(self):
        d=self.ws.inspect_document(self.text)
        self.ws.import_selected(d['inspection_id'],[b['id'] for b in d['blocks'] if not b['needs_check']],'Interview',defaults())
        restored=Workspace();restored.import_project(copy.deepcopy(self.ws.project))
        self.assertEqual(restored.project['source_document']['blocks'],self.ws.project['source_document']['blocks'])


class ReviewDecisionTests(unittest.TestCase):
    def setUp(self):self.ws=completed_demo()

    def test_demo_has_distinct_severity_examples(self):
        self.assertEqual([severity(s) for s in self.ws.project['segments']],['none','none','none','critical','major','minor'])

    def test_minor_batch_is_explicit_and_separate_from_individual_review(self):
        self.ws.review_batch_minor(['S0006'],'AY',self.ws.project['revision'])
        review=self.ws.project['segments'][5]['review']
        self.assertEqual(review['method'],'batch_minor');self.assertIn('explicit user-selected batch',review['note'])
        self.assertIsNone(self.ws.project['segments'][3]['review']);self.assertIsNone(self.ws.project['segments'][4]['review'])
        self.assertIn('"batch_minor": 1',methods_record(self.ws.project))
        self.assertIn('review_method',export_csv(self.ws.project).decode())
        restored=Workspace();restored.import_project(copy.deepcopy(self.ws.project))
        self.assertEqual(restored.project['segments'][5]['review']['method'],'batch_minor')

    def test_mixed_severity_batch_is_rejected_without_partial_approval(self):
        for ids in [['S0006','S0005'],['S0006','S0004'],['S0006','S0001']]:
            before=copy.deepcopy(self.ws.project)
            with self.subTest(ids=ids),self.assertRaises(WorkflowError):self.ws.review_batch_minor(ids,'AY',self.ws.project['revision'])
            self.assertEqual(before,self.ws.project)

    def test_stale_batch_and_blank_reviewer_are_rejected(self):
        for reviewer,revision in [('AY',-1),('',self.ws.project['revision'])]:
            with self.assertRaises(WorkflowError):self.ws.review_batch_minor(['S0006'],reviewer,revision)
        self.assertIsNone(self.ws.project['segments'][5]['review'])

    def test_quick_approval_is_only_for_low_severity_unchanged_drafts(self):
        for sid in ['S0004','S0005']:
            seg=next(s for s in self.ws.project['segments'] if s['id']==sid)
            with self.assertRaises(WorkflowError):self.ws.review(sid,seg['reconcile']['text'],'AY','Accepted',method='quick_accept')
        seg=self.ws.project['segments'][5]
        self.ws.review('S0006',seg['reconcile']['text'],'AY','Accepted',method='quick_accept')
        self.assertEqual(seg['review']['method'],'quick_accept')

    def test_decision_can_return_to_pending_with_history(self):
        self.ws.review_batch_minor(['S0006'],'AY',self.ws.project['revision']);self.ws.clear_review('S0006')
        self.assertIsNone(self.ws.project['segments'][5]['review'])
        self.assertEqual(self.ws.project['audit'][-1]['action'],'review_returned_to_pending')


class SettingsAndTerminologyTests(unittest.TestCase):
    def setUp(self):
        self.ws=Workspace();self.ws.load_text('Participant: The fixator was removed.','Terms')
        settings=defaults();settings['terminology']=[{'source':'fixator','target':'external fixator','alternatives':['frame'],'notes':'Team-agreed device term','required':True}]
        settings['reporting']['dialect_context']='DIALECT_SENTINEL_92';self.ws.save_settings(settings)

    def apply(self,stage,text):
        task=self.ws.prepare_task(stage)
        row={'id':'S0001','text':text}
        if stage=='reconcile':row.update(issues=[],rationale='Fixture',needs_human=True)
        else:row['notes']=[]
        self.ws.apply_manual(task['task_id'],json.dumps({'segments':[row]}),'Manual QA')

    def test_required_terminology_mismatch_adds_major_local_flag(self):
        self.apply('forward','The cast was removed.');self.apply('back','The cast was removed.');self.apply('reconcile','The cast was removed.')
        issues=self.ws.project['segments'][0]['reconcile']['issues']
        self.assertEqual(issues[0]['severity'],'major');self.assertEqual(issues[0]['origin'],'local_glossary_check')
        restored=Workspace();restored.import_project(copy.deepcopy(self.ws.project))
        self.assertEqual(restored.project['segments'][0]['reconcile']['issues'][0]['origin'],'local_glossary_check')
        with self.assertRaises(WorkflowError):self.ws.review('S0001','The cast was removed.','AY','')

    def test_accepted_alternative_does_not_get_mismatch_flag(self):
        self.apply('forward','The frame was removed.');self.apply('back','The frame was removed.');self.apply('reconcile','The frame was removed.')
        self.assertFalse(self.ws.project['segments'][0]['reconcile']['issues'])

    def test_glossary_and_dialect_are_excluded_from_blind_back(self):
        self.apply('forward','The frame was removed.')
        task=self.ws.prepare_task('back');self.assertNotIn('DIALECT_SENTINEL_92',task['user']);self.assertNotIn('Team-agreed',task['user']);self.assertNotIn('terminology_lexicon',task['user'])

    def test_reporting_changes_preserve_outputs_but_dialect_changes_invalidate(self):
        self.apply('forward','The frame was removed.')
        s=copy.deepcopy(self.ws.project['settings']);s['reporting']['reviewer_background']='Bilingual researcher';self.ws.save_settings(s)
        self.assertTrue(self.ws.project['segments'][0]['forward'])
        s=copy.deepcopy(s);s['reporting']['dialect_context']='Different dialect';self.ws.save_settings(s)
        self.assertIsNone(self.ws.project['segments'][0]['forward'])

    def test_mistral_sampling_is_sent_and_defaults_are_omitted(self):
        class Capture(APIClient):
            def _request(self,provider,key,workspace,path,payload=None):
                self.payload=payload
                return {'model':'fixture','choices':[{'finish_reason':'stop','message':{'content':'{}'}}]},'request-fixture'
        settings=defaults();settings['stages']['forward'].update(provider='mistral',model='fixture',temperature=0.2,random_seed=42)
        self.ws.save_settings(settings);task=self.ws.prepare_task('forward');client=Capture();client.complete(task,'synthetic-key')
        self.assertEqual(client.payload['temperature'],0.2);self.assertEqual(client.payload['random_seed'],42);self.assertNotIn('top_p',client.payload)
        settings['stages']['forward']['temperature']=None;settings['stages']['forward']['random_seed']=None;self.ws.save_settings(settings)
        client.complete(self.ws.prepare_task('forward'),'synthetic-key');self.assertNotIn('temperature',client.payload);self.assertNotIn('random_seed',client.payload)

    def test_invalid_sampling_and_unsupported_claude_controls_fail_early(self):
        for option,value in [('temperature',True),('top_p',0),('random_seed',-1),('temperature',float('nan'))]:
            s=defaults();s['stages']['forward'][option]=value
            with self.subTest(option=option,value=value),self.assertRaises(WorkflowError):validate_settings(s)
        s=defaults();s['stages']['forward'].update(provider='anthropic',random_seed=3)
        with self.assertRaises(WorkflowError):validate_settings(s)
        for provider,effort in [('anthropic','none'),('mistral','max'),('copilot','high'),('manual','low')]:
            s=defaults();s['stages']['forward'].update(provider=provider,effort=effort)
            with self.subTest(provider=provider,effort=effort),self.assertRaises(WorkflowError):validate_settings(s)
        s=defaults();s['stages']['forward'].update(provider='copilot',temperature=0.3,random_seed=4)
        self.assertIsNone(validate_settings(s)['stages']['forward']['temperature'],'Copilot sampling values are dropped, not recorded as applied')
        s=defaults();s['stages']['forward'].update(provider='mistral',temperature=0.2,top_p=0.8)
        with self.assertRaises(WorkflowError):validate_settings(s)

    def test_copilot_choice_is_saved_before_any_translation(self):
        settings=defaults()
        for config in settings['stages'].values():
            config.update(chat_tool='Institutional Copilot',chat_model='')
        self.ws.save_settings(settings)
        restored=Workspace();restored.import_project(copy.deepcopy(self.ws.project))
        self.assertFalse(restored.project['runs'])
        self.assertFalse(restored.keys)
        for config in restored.project['settings']['stages'].values():
            self.assertEqual(config['provider'],'manual')
            self.assertEqual(config['chat_tool'],'Institutional Copilot')

    def test_manual_packet_contains_instructions_and_actual_selected_text(self):
        task=self.ws.prepare_task('forward')
        packet=task['system']+'\n\nINPUT JSON:\n'+task['user']
        self.assertIn('TASK: FORWARD TRANSLATION',packet)
        self.assertIn('The fixator was removed.',packet)
        self.assertIn('S0001',packet)
        self.assertFalse(self.ws.keys)

if __name__=='__main__':unittest.main()
