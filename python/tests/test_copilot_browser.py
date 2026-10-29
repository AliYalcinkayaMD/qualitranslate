import copy
import json
import os
import struct
from pathlib import Path
import sys
import threading
import time
import unittest
import urllib.request
from http.server import BaseHTTPRequestHandler,ThreadingHTTPServer
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'src'))
from qualitranslate.copilot_browser import BrowserError,CopilotBrowser,CopilotConnection,DevToolsSocket,extract_receipt_json
from qualitranslate.app import make_handler
from qualitranslate.core import Workspace,defaults,WorkflowError,export_bundle
from test_workflow import FakeClient,wait_for_job

class FakeCopilot:
    def __init__(self):self.requests=[];self.ready=False;self.opened=False
    def summary(self):return {'open':self.opened,'ready':self.ready,'experimental':True,'message':'Fixture controls ready' if self.ready else 'Sign in and check connection.'}
    def open(self):self.opened=True;return self.summary()
    def check(self):self.ready=self.opened;return self.summary()
    def close(self,forget=False):self.ready=False;self.opened=False
    def complete(self,task,cancel=None):
        self.requests.append(copy.deepcopy(task));task['transport_prompt']='VISIBLE_UI_PACKET\n'+task['system']+'\n'+task['user']
        payload=json.loads(task['user']);rows=[]
        for row in payload['segments']:
            text='Browser fixture '+row['id']
            item={'id':row['id'],'text':text}
            if task['stage']=='reconcile':item.update(issues=[],rationale='Synthetic fixture only.',needs_human=True)
            else:item['notes']=[]
            rows.append(item)
        return json.dumps({'qt_receipt':'fixture','segments':rows}),{'provider':'copilot_browser','model':'Copilot Chat / model not reported','integration':'visible_browser_ui','chat_isolation':'new_empty_conversation_checked','receipt_verified':True}

class CopilotSafetyTests(unittest.TestCase):
    def test_expired_control_deadline_cannot_be_extended_by_buffered_events(self):
        transport=DevToolsSocket.__new__(DevToolsSocket);transport.cancel=None;transport.buffer=b'12'
        with self.assertRaisesRegex(BrowserError,'timed out'):transport._read(2,time.monotonic()-1)

    def test_input_echo_without_matching_receipt_is_not_a_response(self):
        text='TASK\n{"segments":[{"id":"S0001","text":"Original"}]}\nAdd a root qt_receipt field equal to QT_RECEIPT_EXPECTED.'
        self.assertIsNone(extract_receipt_json(text,'QT_RECEIPT_EXPECTED'))

    def test_receipt_accepts_only_the_matching_complete_object(self):
        bad=json.dumps({'qt_receipt':'another request','segments':[]});good=json.dumps({'qt_receipt':'expected','segments':[{'id':'S0001','text':'Translated'}]})
        self.assertEqual(extract_receipt_json(bad+'\n```json\n'+good+'\n```','expected'),good)
        self.assertIsNone(extract_receipt_json(good[:-2],'expected'))

    def test_non_loopback_debugger_addresses_are_rejected_before_connection(self):
        for url in ['ws://example.com:9222/devtools/page/abc','ws://127.0.0.1:9999/devtools/page/abc','wss://127.0.0.1:9222/devtools/page/abc','ws://127.0.0.1:9222/arbitrary']:
            with self.subTest(url=url),self.assertRaises(BrowserError):DevToolsSocket(url,9222)

    def test_only_institutional_copilot_hosts_are_eligible_in_production(self):
        browser=CopilotBrowser()
        self.assertTrue(browser._allowed('https://copilot.cloud.microsoft/chat'))
        for url in ['http://copilot.cloud.microsoft/chat','https://copilot.cloud.microsoft.evil.test/','https://copilot.microsoft.com/','http://127.0.0.1/','https://copilot.cloud.microsoft:8443/']:
            self.assertFalse(browser._allowed(url))

    def test_large_unicode_packets_use_masked_64_bit_websocket_frames(self):
        class Sink:
            def sendall(self,data):self.data=data
        transport=DevToolsSocket.__new__(DevToolsSocket);transport.sock=Sink()
        payload=('Danish æøå · Korean 한국어 · '*4000).encode()
        transport._send(payload)
        frame=transport.sock.data
        self.assertEqual(frame[0],0x81);self.assertEqual(frame[1],0xff)
        self.assertEqual(struct.unpack('!Q',frame[2:10])[0],len(payload))
        mask=frame[10:14]
        self.assertEqual(bytes(value^mask[i%4] for i,value in enumerate(frame[14:])),payload)

    def test_authentication_verification_and_limits_pause_without_bypass(self):
        for key in ['authentication_required','challenge','limit']:
            with self.subTest(key=key),self.assertRaises(BrowserError):CopilotBrowser._attention({key:True})

    def test_browser_mode_runs_without_api_keys_and_keeps_transport_records(self):
        api=FakeClient();browser=FakeCopilot();ws=Workspace(api,copilot=browser)
        ws.load_text('Interviewer: ORIGINAL_SOURCE_91\nParticipant: Less pain.','Browser fixture')
        settings=defaults()
        for item in settings['stages'].values():item.update(provider='copilot',chat_tool='Institutional Copilot')
        ws.save_settings(settings)
        with self.assertRaises(WorkflowError):ws.start_job('all')
        browser.open();browser.check();ws.start_job('all');wait_for_job(ws)
        self.assertFalse(ws.job['error']);self.assertFalse(ws.keys);self.assertFalse(api.requests)
        self.assertEqual([r['stage'] for r in browser.requests],['forward','back','reconcile'])
        self.assertNotIn('ORIGINAL_SOURCE_91',browser.requests[1]['user'])
        self.assertTrue(all(s['reconcile'] and not s['review'] for s in ws.project['segments']))
        self.assertTrue(all(r['transport_prompt'].startswith('VISIBLE_UI_PACKET') for r in ws.project['runs']))
        self.assertTrue(all(r['request_configuration']['sampling_application']=='external_chat_not_controlled' for r in ws.project['runs']))
        imported=Workspace();imported.import_project(copy.deepcopy(ws.project))
        self.assertEqual(imported.project['runs'][0]['transport_prompt'],ws.project['runs'][0]['transport_prompt'])
        self.assertTrue(imported.project['runs'][0]['provenance']['receipt_verified'])

    def test_browser_failure_retains_completed_work_and_resumes_without_repeating_forward(self):
        class FailOnce(FakeCopilot):
            failed=False
            def complete(self,task,cancel=None):
                if task['stage']=='back' and not self.failed:
                    self.failed=True;raise BrowserError('Fixture browser interruption')
                return super().complete(task,cancel)
        browser=FailOnce();browser.open();browser.check();ws=Workspace(copilot=browser)
        ws.load_text('P: Original source','Resume fixture');settings=defaults()
        for item in settings['stages'].values():item.update(provider='copilot')
        ws.save_settings(settings);ws.start_job('all');wait_for_job(ws)
        self.assertIn('browser interruption',ws.job['error']);self.assertEqual(len(ws.project['runs']),1)
        ws.start_job('all');wait_for_job(ws)
        self.assertFalse(ws.job['error']);self.assertEqual([r['stage'] for r in browser.requests],['forward','back','reconcile'])

class ConnectionWatcherTests(unittest.TestCase):
    def test_stale_project_or_revision_cannot_start_an_automatic_translation(self):
        for replace_project in (False,True):
            with self.subTest(replace_project=replace_project):
                browser=FakeCopilot();browser.open();browser.check();ws=Workspace(copilot=browser)
                ws.load_text('P: Fictional original','Original project');config=defaults()
                for item in config['stages'].values():item.update(provider='copilot')
                ws.save_settings(config);pid=ws.project['project_id'];revision=ws.project['revision']
                if replace_project:ws.load_text('P: Different source','Different project')
                else:
                    config['study_context']='Changed while connecting';ws.save_settings(config)
                with self.assertRaisesRegex(WorkflowError,'project changed'):
                    ws.start_job('all',expected_project_id=pid,expected_revision=revision)
                self.assertFalse(browser.requests);self.assertFalse(ws.job['running'])

    def wait_for_connection(self,connection):
        deadline=time.monotonic()+3
        while connection.summary()['connecting'] and time.monotonic()<deadline:time.sleep(.005)
        self.assertFalse(connection.summary()['connecting'])

    def test_sign_in_is_rechecked_without_another_button_or_a_prompt(self):
        class Delayed(FakeCopilot):
            checks=0
            def check(self):
                self.checks+=1;self.ready=self.checks>=3;return self.summary()
        browser=Delayed();connection=CopilotConnection(browser,timeout=1,interval=.01)
        connection.start();self.wait_for_connection(connection)
        self.assertTrue(connection.summary()['ready']);self.assertEqual(browser.checks,3);self.assertFalse(browser.requests)

    def test_timeout_shows_the_last_blocker_without_sending_or_reopening(self):
        class Unready(FakeCopilot):
            def check(self):return self.summary()
        browser=Unready();connection=CopilotConnection(browser,timeout=.02,interval=.005)
        connection.start();self.wait_for_connection(connection)
        self.assertEqual(connection.summary()['connection_phase'],'timeout')
        self.assertIn('Sign in',connection.summary()['message']);self.assertFalse(browser.requests)

    def test_human_verification_stops_the_watcher_instead_of_retrying(self):
        class Verify(FakeCopilot):
            checks=0
            def check(self):self.checks+=1;return {**self.summary(),'error_code':'verification','message':'Human verification required'}
        browser=Verify();connection=CopilotConnection(browser,timeout=1,interval=.01)
        connection.start();self.wait_for_connection(connection)
        self.assertEqual(connection.summary()['connection_phase'],'error');self.assertEqual(browser.checks,1)

    def test_cancel_prevents_late_readiness_and_keeps_state_and_export_responsive(self):
        entered=threading.Event();release=threading.Event()
        class Blocking(FakeCopilot):
            def open(self):self.opened=True;entered.set();release.wait(2);return self.summary()
        browser=Blocking();ws=Workspace(copilot=browser);ws.load_text('P: Fictional source','Cancellation test')
        server=ThreadingHTTPServer(('127.0.0.1',0),make_handler(ws,'test-token'))
        thread=threading.Thread(target=server.serve_forever,daemon=True);thread.start()
        def request(path,body=None):
            req=urllib.request.Request('http://127.0.0.1:'+str(server.server_port)+'/api/'+path,
                data=None if body is None else json.dumps(body).encode(),headers={'X-QualiTranslate-Token':'test-token','Content-Type':'application/json'})
            with urllib.request.urlopen(req,timeout=1) as response:return json.load(response)
        try:
            opened=request('copilot/open',{});self.assertTrue(opened['connecting']);self.assertTrue(entered.wait(1))
            self.assertTrue(request('state')['copilot']['connecting'])
            self.assertEqual(request('export',{'kind':'json'})['title'],'Cancellation test')
            cancelled=request('copilot/cancel',{});self.assertEqual(cancelled['connection_phase'],'cancelled')
            release.set();ws.copilot_connection.thread.join(1)
            self.assertFalse(request('state')['copilot']['ready']);self.assertFalse(browser.requests)
        finally:
            release.set();ws.copilot_connection.cancel();server.shutdown();server.server_close()

# Local stand-in for the signed-in Microsoft 365 Copilot Chat page, modelled on the structure
# observed live on 27 Oct 2026 (ids, data-testid values, labels, Lexical zero-width tail,
# Send appearing only with text, Temporary chat toggle, "Copilot said:" replies in a code
# block, Copy Response when finished). Fixture behaviour is not evidence of live compatibility.
FIXTURE=r"""<!doctype html><html lang="en-gb"><meta charset="utf-8"><title>Chat | Microsoft Copilot</title><body><nav><a href="/chat" aria-label="New chat">New chat</a></nav><main>
<header><button id="gptModeSwitcher" aria-label="Auto, Model Selector">Auto</button><button id="temp" type="button" aria-label="Temporary chat" aria-pressed="false">T</button></header>
<h1 id="welcome"></h1><p id="tempnote"></p>
<div data-testid="MessageListContainer"><div role="feed" aria-label="Chat conversation" id="feed"></div></div>
<div id="composer"><span id="m365-chat-editor-target-element" contenteditable="true" role="textbox" aria-label="Message Copilot"><p><br></p></span>
<button aria-label="Add and manage sources" type="button">+</button><button aria-label="Start dictation" type="button">mic</button><span id="sendslot"></span></div></main>
<script>
const q=location.search,editor=document.getElementById('m365-chat-editor-target-element'),feed=document.getElementById('feed'),slot=document.getElementById('sendslot'),welcome=document.getElementById('welcome'),temp=document.getElementById('temp'),note=document.getElementById('tempnote');
const greetings=['Hi, how can I assist?','What can I help you with?','What can I help with?'];welcome.textContent=greetings[Math.floor(Math.random()*3)];
if(q.includes('no_temp'))temp.remove();
const loadedAt=performance.now();
const raw=()=>editor.innerText.replace(/[\u200b\u200c]+\s*$/,'');
function render(){const has=!!raw().trim();if(has&&!slot.firstChild){const b=document.createElement('button');b.type='submit';b.setAttribute('aria-label','Send');b.textContent='>';b.onclick=onSend;slot.append(b);}else if(!has)slot.replaceChildren();}
editor.addEventListener('input',()=>{if(raw().trim()&&!editor.querySelector('[data-zw]')){const z=document.createElement('span');z.dataset.zw='1';z.setAttribute('aria-hidden','true');z.textContent='​‌';editor.append(z);}render();});
temp.onclick=()=>{const on=temp.getAttribute('aria-pressed')!=='true';temp.setAttribute('aria-pressed',String(on));welcome.textContent=on?'Temporary chat':greetings[0];note.textContent=on?"This chat won't be saved or create memories. It follows your organisation's retention policy, and may still be accessible to your admin.":'';};
function log(event,data){fetch('/log?event='+event,{method:'POST',body:data||''});}
function onSend(){const packet=raw();editor.innerHTML='<p><br></p>';slot.replaceChildren();log('send',packet);
 const turn=document.createElement('div');turn.dataset.testid='m365-chat-llm-web-ui-chat-message';
 const user=document.createElement('div');user.className='fai-UserMessage';user.setAttribute('role','article');const out=document.createElement('div');out.dataset.testid='chatQuestion';out.textContent='You said:\n'+packet.slice(0,300);user.append(out);turn.append(user);feed.append(turn);welcome.textContent='';
 if(q.includes('early_reset')&&performance.now()-loadedAt<4000){setTimeout(()=>{feed.replaceChildren();welcome.textContent='Temporary chat';},300);return;}
 const stop=document.createElement('button');stop.setAttribute('aria-label','Stop generating');stop.textContent='Stop';document.getElementById('composer').append(stop);
 if(q.includes('limit')){const a=document.createElement('div');a.setAttribute('role','alert');a.textContent="You've reached your limit. Try again later.";document.body.append(a);stop.remove();return;}
 setTimeout(()=>{stop.remove();const reply=document.createElement('div');reply.dataset.testid='copilot-message-reply-div';const md=document.createElement('div');md.dataset.testid='markdown-reply';reply.append('Copilot said:\n',md);
  if(packet.includes('DECLINE_ME')){md.textContent="Sorry, it looks like I can't respond to this. Let's try a different topic.";turn.append(reply);return;}
  if(q.includes('prose')){md.textContent='Here is the translation: When can one judge whether the treatment was a success?';turn.append(reply);return;}
  const receipt=[...packet.matchAll(/QT_RECEIPT_[a-f0-9]{32}/g)].at(-1)[0];const start=packet.indexOf('INPUT JSON:\n')+'INPUT JSON:\n'.length,end=packet.lastIndexOf('\n\nREPLY FORMAT FOR THIS CHAT:');
  const payload=JSON.parse(packet.slice(start,end).replaceAll(String.fromCharCode(160),' '));const back=payload.task==='blind_back_translation',recon=!!payload.segments[0].original_source;
  const rows=payload.segments.map(s=>recon?{id:s.id,text:'Reconciled fixture '+s.id,issues:[],rationale:'Synthetic local test',needs_human:true}:{id:s.id,text:(back?'Back fixture ':'Forward fixture ')+s.id+' "quoted" *star* _under_ \u0645\u06cc\u200c\u062e\u0648\u0627\u0647\u0645',notes:[]});
  const result={segments:rows};if(!q.includes('no_receipt'))result.qt_receipt=receipt;
  const block=document.createElement('div');block.className='scriptor-component-code-block';const code=document.createElement('div');code.setAttribute('role','textbox');code.setAttribute('aria-label','Code editor');code.setAttribute('aria-readonly','true');code.textContent=JSON.stringify(result);block.append('JSON\n',code);md.append(block);turn.append(reply);
  const copy=document.createElement('button');copy.setAttribute('aria-label','Copy Response');turn.append(copy);},250);}
</script></body></html>"""

@unittest.skipUnless(os.environ.get('QT_TEST_CHROME'),'Set QT_TEST_CHROME to a test Chromium executable for browser integration checks.')
class ActualBrowserTransportTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.events=[]
        events=cls.events
        class Handler(BaseHTTPRequestHandler):
            def log_message(self,*a):pass
            def do_GET(self):
                events.append(('load',self.path))
                body=FIXTURE.encode();self.send_response(200);self.send_header('Content-Type','text/html; charset=utf-8');self.send_header('Content-Length',str(len(body)));self.end_headers();self.wfile.write(body)
            def do_POST(self):
                data=self.rfile.read(int(self.headers.get('Content-Length','0'))).decode()
                events.append((self.path.split('event=')[-1],data));self.send_response(204);self.end_headers()
        cls.server=ThreadingHTTPServer(('127.0.0.1',0),Handler);cls.thread=threading.Thread(target=cls.server.serve_forever,daemon=True);cls.thread.start()
    @classmethod
    def tearDownClass(cls):cls.server.shutdown();cls.server.server_close()
    def setUp(self):self.events.clear()
    def launch(self,query='',settle=0.3,**kw):
        import tempfile
        profile=tempfile.mkdtemp(prefix='qt-test-profile-');self.addCleanup(lambda:__import__('shutil').rmtree(profile,ignore_errors=True))
        browser=CopilotBrowser(executable=os.environ['QT_TEST_CHROME'],start_url='http://127.0.0.1:'+str(self.server.server_port)+'/'+query,allow_fixture=True,profile_dir=profile,settle_seconds=settle,
                               extra_args=['--headless=new','--no-sandbox','--disable-dev-shm-usage','--disable-gpu'],timeout=8,**kw)
        self.addCleanup(browser.close);browser.open()
        deadline=time.monotonic()+15
        while time.monotonic()<deadline:
            if browser.check()['ready']:return browser
            time.sleep(.1)
        self.fail(browser.summary()['message'])
    def sends(self):return [d for e,d in self.events if e=='send']
    def workspace(self,browser,text='Interviewer: SOURCE_SENTINEL_91\nParticipant A: ORIGINAL_DANISH_92'):
        ws=Workspace(copilot=browser);ws.load_text(text,'Fixture')
        settings=defaults()
        for config in settings['stages'].values():config.update(provider='copilot',chat_tool='Institutional Copilot')
        ws.save_settings(settings);return ws

    def test_three_stages_in_verified_temporary_chats_with_receipts_and_blind_back(self):
        browser=self.launch();ws=self.workspace(browser,'Interviewer: SOURCE_SENTINEL_91\nParticipant A: ORIGINAL_DANISH_92 \u0645\u06cc\u200c\u062e\u0648\u0627\u0647\u0645');ws.start_job('all');wait_for_job(ws,40)
        self.assertFalse(ws.job['error'],ws.job['error']);self.assertEqual(len(ws.project['runs']),3)
        sent=self.sends();self.assertEqual(len(sent),3)
        self.assertNotIn('SOURCE_SENTINEL_91',sent[1]);self.assertNotIn('ORIGINAL_DANISH_92',sent[1])
        self.assertEqual(ws.project['runs'][1]['transport_prompt'].strip(),sent[1].strip())
        for run in ws.project['runs']:
            prov=run['provenance']
            self.assertEqual(prov['chat_isolation'],'new_temporary_chat_verified');self.assertTrue(prov['receipt_verified'])
            self.assertIn("won't be saved",prov['temporary_chat_notice']);self.assertEqual(prov['copilot_model_selector_label'],'Auto')
            self.assertNotIn('​',run['transport_prompt'])
        self.assertIn('"quoted" *star* _under_',ws.project['segments'][0]['forward']['text'])
        # Zero-width non-joiner (Persian) is meaningful text: kept in replies and in the sent record.
        self.assertIn('\u06cc\u200c\u062e',ws.project['segments'][0]['forward']['text'])
        self.assertIn('\u06cc\u200c\u062e',ws.project['runs'][0]['transport_prompt'])
        self.assertFalse(ws.project['runs'][0]['transport_prompt'].endswith('\u200c'))
        self.assertGreaterEqual(sum(1 for e,_ in self.events if e=='load'),4)

    def test_ordinary_chat_when_temporary_chat_is_switched_off(self):
        browser=self.launch();ws=self.workspace(browser,'P: SOURCE')
        settings=copy.deepcopy(ws.project['settings']);settings['copilot']['temporary_chat']=False;ws.save_settings(settings)
        ws.start_job('forward');wait_for_job(ws,20)
        self.assertFalse(ws.job['error'],ws.job['error']);self.assertEqual(ws.project['runs'][0]['provenance']['chat_isolation'],'new_empty_chat_verified')

    def test_missing_temporary_switch_stops_before_sending(self):
        browser=self.launch('?no_temp');ws=self.workspace(browser,'P: SOURCE');ws.start_job('all');wait_for_job(ws,20)
        self.assertIn('Temporary chat switch',ws.job['error']);self.assertEqual(self.sends(),[]);self.assertFalse(ws.project['runs'])

    def test_message_sent_before_the_page_settles_is_not_lost_silently(self):
        # Live Copilot silently cleared a message sent ~1-2 s after the page loaded (27 Oct 2026).
        browser=self.launch('?early_reset',settle=0);ws=self.workspace(browser,'P: SOURCE');ws.start_job('forward');wait_for_job(ws,30)
        self.assertIn('cleared the message',ws.job['error']);self.assertFalse(ws.project['runs'])
        browser.settle_ms=4500;self.events.clear();ws.start_job('forward');wait_for_job(ws,30)
        self.assertFalse(ws.job['error'],ws.job['error']);self.assertEqual(len(self.sends()),1);self.assertTrue(ws.project['segments'][0]['forward'])

    def test_declined_section_is_isolated_and_the_rest_is_translated(self):
        text='\n'.join('P: Line '+str(i)+(' DECLINE_ME' if i==3 else '') for i in range(1,6))
        browser=self.launch();browser.decline_quiet_seconds=1.5;ws=self.workspace(browser,text);ws.start_job('all');wait_for_job(ws,90)
        self.assertIn('declined',ws.job['error']);self.assertIn('S0003',ws.job['error'])
        done={s['id']:bool(s['forward']) for s in ws.project['segments']}
        self.assertEqual(done,{'S0001':True,'S0002':True,'S0003':False,'S0004':True,'S0005':True})
        self.assertEqual([d['id'] for d in ws.job['declined']],['S0003'])
        neighbours=[r for r in ws.project['runs'] if r['segment_ids'] in (['S0002'],['S0004'])]
        self.assertTrue(neighbours and all(r['context_omitted'] for r in neighbours))
        self.assertTrue(all('DECLINE_ME' not in r['user_prompt'] for r in neighbours))
        self.assertFalse(any(s['back'] for s in ws.project['segments']))
        ws.edit('S0003','forward','Human translation of line 3','Entered by AY after Copilot declined')
        self.assertEqual(ws.project['segments'][2]['forward']['run_id'],'human_entry')
        self.events.clear();ws.start_job('all');wait_for_job(ws,120)
        # The human forward text passes blind back-translation; reconciliation still carries the original.
        self.assertIn('reconciliation: S0003',ws.job['error'])
        self.assertTrue(all(s['back'] for s in ws.project['segments']))
        self.assertEqual([s['id'] for s in ws.project['segments'] if not s['reconcile']],['S0003'])
        ws.edit('S0003','reconcile','Human proposed translation 3','Reconciled by AY')
        self.assertEqual(ws.project['segments'][2]['reconcile']['run_id'],'human_entry');self.assertTrue(all(s['reconcile'] for s in ws.project['segments']))
        bundle=export_bundle(ws.project);self.assertTrue(bundle)
        again=Workspace(copilot=FakeCopilot());again.import_project(json.loads(json.dumps(ws.project)))
        self.assertIn('human_entry',again.project['segments'][2]['forward']);self.assertIn('human_entry',again.project['segments'][2]['reconcile'])

    def test_a_usage_limit_stops_without_automatic_resend(self):
        browser=self.launch('?limit');ws=self.workspace(browser,'P: SOURCE');ws.start_job('all');wait_for_job(ws,30)
        self.assertIn('usage limit',ws.job['error']);self.assertFalse(ws.project['runs']);self.assertEqual(len(self.sends()),1)

    def test_missing_receipt_never_imports_the_echoed_input(self):
        browser=self.launch('?no_receipt');ws=self.workspace(browser,'P: SOURCE');task=ws.prepare_task('forward')
        with self.assertRaisesRegex(BrowserError,'complete structured result'):browser.complete(task)
        self.assertIsNone(ws.project['segments'][0]['forward']);self.assertEqual(len(self.sends()),1)

    def test_reattaches_to_the_open_dedicated_window_without_a_second_browser(self):
        browser=self.launch();port=browser.port
        again=CopilotBrowser(executable=os.environ['QT_TEST_CHROME'],start_url=browser.start_url,allow_fixture=True,profile_dir=browser.profile_dir)
        with self.assertRaisesRegex(BrowserError,'Another QualiTranslate copy'):again.open()   # one copy at a time
        browser._release()   # as if the first app copy had quit, leaving its window open
        again.open();self.addCleanup(again._release);self.assertEqual(again.port,port);self.assertTrue(again.check()['ready'])

    def test_forget_closes_a_window_left_open_by_an_earlier_session(self):
        import tempfile,shutil
        base=tempfile.mkdtemp();self.addCleanup(shutil.rmtree,base,True)
        profile=os.path.join(base,'copilot-browser-profile')
        first=CopilotBrowser(executable=os.environ['QT_TEST_CHROME'],start_url='http://127.0.0.1:'+str(self.server.server_port)+'/',allow_fixture=True,profile_dir=profile,extra_args=['--headless=new','--no-sandbox','--disable-gpu'])
        first.open();port=first.port;first._release();first._forget_connection()   # app restarted; window still open
        later=CopilotBrowser(executable=os.environ['QT_TEST_CHROME'],allow_fixture=True,profile_dir=profile)
        later.close(forget=True)
        self.assertFalse(os.path.exists(profile),later.summary()['message'])
        with self.assertRaises(Exception):urllib.request.urlopen('http://127.0.0.1:%d/json/version'%port,timeout=1)

    def test_prose_reply_is_not_treated_as_a_refusal_or_resent(self):
        browser=self.launch('?prose');browser.decline_quiet_seconds=1.5;ws=self.workspace(browser,'P: one\nP: two');ws.start_job('all');wait_for_job(ws,40)
        self.assertIn('ordinary text',ws.job['error']);self.assertEqual(ws.job['error_code'],'no_json')
        self.assertEqual(len(self.sends()),1);self.assertFalse(ws.job['declined']);self.assertFalse(ws.project['runs'])

if __name__=='__main__':unittest.main()
