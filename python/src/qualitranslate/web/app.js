'use strict';
const $ = id => document.getElementById(id);
const token = document.querySelector('meta[name="csrf-token"]').content;
const stageNames = {forward:'Forward translation', back:'Blind back-translation', reconcile:'Reconciliation'};
const stageDescriptions = {
  forward:'Translate the source into the target language, keeping context, uncertainty and participant voice.',
  back:'Translate the forward translation back into the source language. The original transcript is excluded from this request.',
  reconcile:'Compare the source, forward and blind back-translations. Identify discrepancies and propose a draft for human review.'
};
let state = null, activeView = 'source', activeStage = 'forward', currentTask = null, reviewId = null, editing = null;
let lastRevision = -1, currentProject = '', settingsDirty = false, settingsRendered = false, wasRunning = false, lastJobError = '';
let guidedModels = {}, guidedProvider = 'manual', manualLabel = '';
let currentInspection=null, includedBlocks=new Set(), uploadedBlocks=null, batchSelection=null;
let pendingCopilotStart=null, copilotStarting=false, pollInFlight=false;
let selectedSeverities=new Set();
const severityRank={none:0,minor:1,major:2,critical:3};
const issueTier=seg=>(seg.reconcile?.issues||[]).reduce((a,i)=>severityRank[i.severity]>severityRank[a]?i.severity:a,'none');
const selectedProvider=()=>{const x=document.querySelector('input[name="guidedTool"]:checked').value;return ['aau','manual'].includes(x)?'manual':x;};
const stages = Object.keys(stageNames);
const nextStage = () => stages.find(stage => state.project.segments.some(s => !s[stage])) || 'reconcile';
const complete = () => state.project.segments.length > 0 && state.project.segments.every(s => s.reconcile);
const esc = value => String(value == null ? '' : value).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const textOf = (seg, stage) => seg[stage] ? seg[stage].text : '';
function showError(message){ $('error').textContent=message; $('error').classList.remove('hidden'); }
function clearError(){ $('error').classList.add('hidden'); }
function toast(message){ $('toast').textContent=message; $('toast').classList.remove('hidden'); setTimeout(()=>$('toast').classList.add('hidden'),3800); }
async function api(path, body){
  const controller=new AbortController();
  const timeout=setTimeout(()=>controller.abort(),path==='models'?130000:15000);
  const options={headers:{'X-QualiTranslate-Token':token},signal:controller.signal};
  if(body !== undefined){options.method='POST';options.headers['Content-Type']='application/json';options.body=JSON.stringify(body);}
  try{
    const response=await fetch('/api/'+path,options);
    let data;try{data=await response.json();}catch(e){if(e.name==='AbortError')throw e;throw new Error('The local app returned an unreadable response.');}
    if(!response.ok)throw new Error(data.error||'Request failed.');
    return data;
  }catch(e){if(e.name==='AbortError')throw new Error('The local app did not reply in time. Keep Terminal open and check the connection status. No translation request was automatically repeated.');throw e;}
  finally{clearTimeout(timeout);}
}
async function act(fn){clearError();try{await fn();}catch(e){showError(e.message);}}
async function refresh(force=false){
  const next=await api('state'); state=next;
  const p=state.project;
  const projectChanged=p.project_id!==currentProject;
  if(!manualLabel||projectChanged){
    const configured=p.settings.stages[activeStage]||p.settings.stages.forward;
    manualLabel=configured.provider==='manual'&&configured.chat_tool?configured.chat_tool+' / '+(configured.chat_model||'model not reported'):(p.last_manual_label||'');
    $('manualModel').value=manualLabel;
  }
  $('demoBanner').classList.toggle('hidden',!p.demo);
  $('jobPanel').classList.toggle('hidden',!state.job.running);
  $('jobMessage').textContent=state.job.message;
  $('jobCount').textContent=state.job.done+' / '+state.job.total+' batches';
  const step=state.copilot||{};
  $('jobStep').textContent=state.job.running&&step.step&&step.step!=='idle'?'Copilot: '+step.step_detail+' ('+(step.step_elapsed||0)+' s)':'';
  renderHumanNeeded();
  if(state.job.error && state.job.error!==lastJobError)showError(state.job.error);
  lastJobError=state.job.error;
  const finished=wasRunning && !state.job.running && !state.job.error;
  wasRunning=state.job.running;
  if(force||p.revision!==lastRevision||p.project_id!==currentProject){
    const changedProject=p.project_id!==currentProject;
    lastRevision=p.revision;currentProject=p.project_id;
    if(currentTask && currentTask.revision!==p.revision){currentTask=null;$('manualBox').classList.add('hidden');}
    renderSource();renderStages();renderReview();renderExports();renderProcess();
    if(changedProject){
      settingsRendered=false;settingsDirty=false;activeStage=nextStage();renderStages();
      $('welcome').classList.toggle('hidden',!!p.segments.length);
      $('sourceForm').classList.toggle('hidden',!p.segments.length);
      $('projectTitle').value=p.title;
      $('sourceText').value=p.segments.map(s=>(s.speaker?s.speaker+': ':'')+s.source).join('\n\n');
    }
    if(!settingsDirty && (!settingsRendered||force))renderSettings();
  }
  $('mistralKeyStatus').textContent=state.credentials.mistral?'Key stored in app memory · not included in exports':'No key stored';
  $('anthropicKeyStatus').textContent=state.credentials.anthropic?'Key stored in app memory · not included in exports':'No key stored';
  const disableIds=['loadDemo','startOwn','demoToOwn','importText','generatePrompt','runStage','runAll','runDemo','saveSettings','reset','applyManual','guidedContinue','guidedConnect','confirmCleaning','approveMinors','openCopilot','checkCopilot','closeCopilot','forgetCopilot'];
  disableIds.forEach(id=>$(id).disabled=state.job.running);
  updateGuidedTool();renderProcess();renderCopilotStatus();
  $('approveMinors').disabled=state.job.running||!state.project.segments.some(s=>s.reconcile&&!s.review&&issueTier(s)==='minor');
  if(finished){
    if(complete()){go('review');toast('Translation stages complete. Start with Review next section.');}
    else {setStage(nextStage());toast('Completed work is saved in memory. You can resume the remaining stages.');}
  }
  await advanceCopilotConnection();
}
function go(view){
  if(!state)return;
  if(['connect','workflow','review'].includes(view)&&!state.project.segments.length){showError('Choose Try the demo or Use my transcript to begin.');return;}
  if(view==='review'&&!state.project.segments.some(s=>s.reconcile)){go('workflow');showError('Complete the translation stages first, then review the results.');return;}
  activeView=view;document.body.dataset.currentView=view;
  document.querySelectorAll('.view').forEach(el=>el.classList.toggle('hidden',el.id!==view));
  document.querySelectorAll('.nav').forEach(el=>el.classList.toggle('active',el.dataset.view===view));
  const titles={source:'Let’s translate an interview.',connect:'Step 2 · Choose your translation tool',workflow:state.project.title,review:'Step 4 · Review the final wording',export:'Step 5 · Download your work',settings:'Optional advanced settings'};
  $('pageTitle').textContent=titles[view];
  clearError();window.scrollTo({top:0,behavior:'instant'});
  if(view==='settings'&&!settingsDirty)renderSettings();
  if(view==='export')loadMethods();
  if(view==='workflow'){activeStage=nextStage();renderStages();}
}
function renderSource(){
  const p=state.project,segs=p.segments;
  $('sourcePreview').classList.toggle('hidden',!segs.length);
  $('previewTitle').textContent=p.title;
  $('previewCount').textContent=segs.length+' sections · '+segs.reduce((n,s)=>n+s.source.split(/\s+/).filter(Boolean).length,0)+' source words';
  $('segmentsPreview').innerHTML=segs.slice(0,100).map(s=>'<div class="source-segment"><div class="segment-label">'+esc(s.speaker||'Unlabelled')+'<small>'+esc(s.id)+'</small></div><div class="segment-text">'+esc(s.source)+'</div><button class="text-button" data-edit="'+esc(s.id)+'" data-stage="source">Edit source</button></div>').join('');
  if(segs.length>100)$('segmentsPreview').insertAdjacentHTML('beforeend','<p class="muted">Showing the first 100 sections. All sections are available in Review.</p>');
  const check=state.identifier_check||{total:0};
  $('identifierWarning').classList.toggle('hidden',!check.total||p.demo);
  if(check.total){
    const kinds=check.phrases.join(', ');
    $('identifierWarning').innerHTML='<strong>Possible identifiers: '+esc(kinds)+'.</strong> Found in '+check.segments.slice(0,12).map(r=>'<button class="text-button" data-edit="'+esc(r.id)+'" data-stage="source">'+esc(r.id)+'</button>').join(' ')+(check.segments.length>12?' and '+(check.segments.length-12)+' more':'')+
      '. This is a quick local pattern check: it does not change your text and cannot find names or places. If your approval requires pseudonymised transcripts, edit these sections before translating.';
  }
}
function renderStages(){
  const p=state.project,n=p.segments.length;
  $('languagePair').textContent=p.settings.source_language+' → '+p.settings.target_language+' → '+p.settings.source_language;
  $('stageCards').innerHTML=Object.keys(stageNames).map((stage,i)=>{
    const done=p.segments.filter(s=>s[stage]).length;
    const config=p.settings.stages[stage];
    return '<button class="stage-card '+(activeStage===stage?'active':'')+'" data-stagecard="'+stage+'"><span class="step-number">'+(done===n&&n?'✓':'0'+(i+1))+'</span><h3>'+stageNames[stage]+'</h3><p>'+esc({manual:'Copy and paste',copilot:'Copilot browser',anthropic:'Claude API',mistral:'Mistral API'}[config.provider])+'</p><progress class="progress" value="'+done+'" max="'+Math.max(1,n)+'" aria-label="'+stageNames[stage]+' progress"></progress><div class="count">'+done+' / '+n+' sections</div></button>';
  }).join('');
  $('stageHeading').textContent=stageNames[activeStage];
  $('stageDescription').textContent=stageDescriptions[activeStage];
  $('stageMode').textContent={manual:'COPY AND PASTE',copilot:'COPILOT BROWSER',anthropic:'CLAUDE API',mistral:'MISTRAL API'}[p.settings.stages[activeStage].provider];
  const allDone=complete(), manual=p.settings.stages[activeStage].provider==='manual';
  const pending=p.segments.filter(s=>!s[activeStage]).length;
  $('runDemo').classList.toggle('hidden',!p.demo||allDone);
  $('runAll').classList.toggle('hidden',p.demo||allDone||manual);
  const started=p.segments.some(s=>s.forward);
  $('runAll').textContent=p.settings.stages[activeStage].provider==='copilot'?(started?'Resume with Copilot →':'Translate with Copilot →'):(started?'Resume automatic translation →':'Start automatic translation →');
  $('generatePrompt').classList.toggle('hidden',p.demo||allDone||!manual||!pending);
  $('workflowReview').classList.toggle('hidden',!allDone);
  document.querySelector('.workflow-advanced').classList.toggle('hidden',p.demo||manual||allDone);
  $('nextAction').textContent=allDone?'The three translation stages are complete. Your next step is human review.':p.demo?'Run the offline demo to see all three stages.':manual?'Next: copy the '+stageNames[activeStage].toLowerCase()+' prompt. '+pending+' sections remain in this stage.':p.settings.stages[activeStage].provider==='copilot'?(started?'Click Resume with Copilot to continue where it stopped. Completed batches are kept.':'No copy and paste needed. Click Translate with Copilot. A separate Copilot window opens; sign in there if asked, and the app runs all three stages.'):'Ready to translate. The app will run each API stage in order and show the results for review.';
  renderChatGuide(manual&&!p.demo&&!allDone&&pending>0);
  $('runSummary').textContent=p.run_count+' batches recorded. Prompts and model records are included in your download.';
  $('reviewBadge').textContent=p.segments.filter(s=>s.reconcile&&!s.review).length;
}
function setStage(stage){
  const prior=stages.slice(0,stages.indexOf(stage));
  if(prior.some(s=>state.project.segments.some(seg=>!seg[s]))){showError('Complete the earlier translation stage first. The app will guide you to this step.');return;}
  activeStage=stage;currentTask=null;$('manualBox').classList.add('hidden');clearError();renderStages();
}
function visibleReviewSegments(){
  const filter=$('reviewFilter').value;
  return state.project.segments.filter(seg=>{
    const statusMatch=filter==='all'||filter==='pending'&&!seg.review||filter==='flagged'&&seg.reconcile&&seg.reconcile.issues.length||filter==='reviewed'&&seg.review;
    return statusMatch&&(!selectedSeverities.size||selectedSeverities.has(issueTier(seg)));
  });
}
function renderReview(){
  const segs=state.project.segments,ready=segs.filter(s=>s.reconcile),reviewed=segs.filter(s=>s.review).length;
  const flagged=ready.filter(s=>!s.review&&issueTier(s)!=='none').length;
  $('reviewMetrics').innerHTML=[[segs.length,'Interview sections'],[ready.length,'Reconciled drafts'],[flagged,'Flagged · awaiting decision'],[reviewed,'Decisions recorded']].map(x=>'<div class="metric"><strong>'+x[0]+'</strong><small>'+x[1]+'</small></div>').join('');
  const pending=ready.filter(s=>!s.review),next=visibleReviewSegments().filter(s=>s.reconcile&&!s.review).sort((a,b)=>severityRank[issueTier(b)]-severityRank[issueTier(a)])[0];
  $('reviewNext').disabled=!next;
  $('reviewNext').textContent=next?(issueTier(next)==='critical'?'Review critical section →':issueTier(next)==='major'?'Review major section →':'Review next section →'):(pending.length?'No pending sections in this filter':'All sections have decisions');
  $('demoToOwn').classList.toggle('hidden',!state.project.demo);
  if(!$('quickReviewer').value){const previous=segs.find(s=>s.review);if(previous)$('quickReviewer').value=previous.review.reviewer;}
  $('severityFilters').innerHTML=[['all','All severities'],['minor','Minor'],['major','Major'],['critical','Critical'],['none','No flags']].map(([tier,label])=>{
    const count=tier==='all'?segs.length:segs.filter(s=>issueTier(s)===tier).length;
    const active=tier==='all'?!selectedSeverities.size:selectedSeverities.has(tier);
    return '<button class="severity-filter '+tier+(active?' selected':'')+'" data-risk="'+tier+'" aria-pressed="'+active+'">'+label+'<b>'+count+'</b></button>';
  }).join('');
  const filtered=visibleReviewSegments();
  $('filterSummary').textContent='Showing '+filtered.length+' of '+segs.length+' sections'+(selectedSeverities.size?' · '+[...selectedSeverities].map(x=>x==='none'?'no flags':x).join(' + '):'')+'. Flags are review suggestions, not accuracy scores.';
  const minorCount=pending.filter(s=>issueTier(s)==='minor').length;
  $('approveMinors').textContent='Approve all minor-only ('+minorCount+')';$('approveMinors').disabled=!minorCount||state.job.running;
  $('reviewList').innerHTML=filtered.length?filtered.map(seg=>{
    const tier=issueTier(seg),r=seg.review,issues=seg.reconcile?.issues||[];
    const status=r?(r.method==='batch_minor'?'ACCEPTED IN MINOR BATCH':r.status==='reviewed_uncertainty_retained'?'DECISION · UNCERTAINTY RETAINED':'DECISION RECORDED'):seg.reconcile?(tier==='none'?'HUMAN REVIEW PENDING':tier.toUpperCase()+' · REVIEW PENDING'):'TRANSLATION INCOMPLETE';
    const columns=[['Original interview',seg.source],['Proposed translation',textOf(seg,'reconcile')||'Complete the translation stages first.']].map(([label,txt],i)=>'<div class="review-col '+(i?'target':'')+'"><h4>'+label+'</h4><p>'+esc(txt)+'</p></div>').join('');
    const flags=issues.length?'<div class="flag-notes">'+issues.map(i=>'<div class="flag-note '+i.severity+'"><span class="severity-chip '+i.severity+'">'+esc(i.severity)+'</span><p><strong>'+esc(i.category.replace(/_/g,' '))+'</strong> · '+esc(i.detail)+'</p></div>').join('')+'</div>':'';
    const history='<details class="translation-history"><summary>See forward / back-translations, rationale, and corrections</summary><div class="review-grid">'+[['forward','Forward translation'],['back','Blind back-translation']].map(([stage,label])=>'<div class="review-col"><h4>'+label+'</h4><p>'+esc(textOf(seg,stage)||'—')+'</p>'+(seg[stage]?'<button class="text-button" data-edit="'+seg.id+'" data-stage="'+stage+'">Correct '+label.toLowerCase()+'</button>':'')+'</div>').join('')+'</div><p class="issue-detail">'+esc(seg.reconcile?.rationale||'')+'</p><button class="text-button" data-edit="'+seg.id+'" data-stage="source">Correct original source</button></details>';
    const controls=r?'<button class="text-button" data-clear-review="'+seg.id+'">Mark pending</button><button class="button small subtle" data-review="'+seg.id+'">Edit decision</button>':seg.reconcile?(tier==='minor'||tier==='none'?'<button class="button small subtle" data-review="'+seg.id+'">Edit / review</button><button class="button small primary" data-approve="'+seg.id+'">Approve draft ✓</button>':'<button class="button small primary" data-review="'+seg.id+'">Review & decide →</button>'):'';
    const reviewed=r?'<div class="reviewed-text"><strong>'+esc(r.method==='batch_minor'?'Batch acceptance':r.method==='quick_accept'?'Quick approval':'Individual review')+' · '+esc(r.reviewer)+'</strong><p>'+esc(r.text)+'</p>'+(r.note?'<small>'+esc(r.note)+'</small>':'')+'</div>':'';
    return '<article class="review-card '+tier+(r?' reviewed':'')+'"><div class="review-heading"><strong>'+esc(seg.id)+'<span>'+esc(seg.speaker||'Unlabelled speaker')+'</span></strong><span class="pill '+(r?'decided':tier)+'">'+status+'</span></div><div class="review-grid">'+columns+'</div>'+flags+reviewed+history+'<div class="review-bottom"><span>'+esc(r?'Decision saved. You can edit it or return it to pending.':tier==='none'?'No issue flagged. Compare the source before approving.':'Review the flag and the proposed repair.')+'</span><div class="button-row">'+controls+'</div></div></article>';
  }).join(''):'<div class="empty">No sections match these filters. Choose another severity or review status.</div>';
}
function renderProcess(){
  if(!state)return;
  const segs=state.project.segments,n=segs.length;
  const info=[['source','Original transcript','Select the interview content.'],['forward','Forward translation','Original → target language.'],['back','Blind back-translation','Target → original language; source hidden.'],['reconcile','Reconciliation','Compare original + forward + back.'],['review','Human decision','Resolve flags and approve the wording.']];
  let active=!n?'source':nextStage();if(complete())active='review';
  $('processTrack').innerHTML=info.map(([key,title,desc],i)=>{
    const done=key==='source'?n:segs.filter(s=>s[key]).length,finished=n&&done===n;
    const status=!n?(key==='source'?'START HERE':'WAITING'):key==='source'?n+' sections selected':done+' / '+n+(key==='review'?' decisions':' complete');
    return '<li class="process-step '+(finished?'done ':key===active?'current ':'')+'"><span class="process-number">'+(finished?'✓':String(i+1).padStart(2,'0'))+'</span><div><h3>'+(key==='reconcile'?'<span class="process-desktop-label">Reconciliation</span><span class="process-mobile-label">Reconcile</span>':title)+'</h3><p>'+desc+'</p><small>'+status+'</small></div></li>';
  }).join('');
}

async function loadMethods(){
  try{$('methodsParagraph').value=(await api('methods',{})).paragraph;}catch(e){$('methodsParagraph').value='';}
}
function renderExports(){
  if(activeView==='export')loadMethods();
  const segs=state.project.segments,reviewed=segs.filter(s=>s.review).length;
  $('exportStatus').textContent=state.project.demo?'Synthetic demonstration. All exports are labelled as sample data.':segs.length?reviewed+' of '+segs.length+' sections have a recorded decision. Exports distinguish individual review, quick approval, and minor-only batch acceptance. Pending text remains a draft.':'No transcript loaded yet. You can still export the current settings as a project.';
}
function renderSettings(){
  if(!state)return;
  const s=state.project.settings;
  [['sourceLanguage','source_language'],['targetLanguage','target_language'],['studyContext','study_context'],['glossary','glossary'],['batchChars','batch_chars'],['batchSegments','batch_segments'],['maxTokens','max_tokens']].forEach(x=>$(x[0]).value=s[x[1]]);
  $('forwardAddition').value=s.prompt_additions.forward;$('reconcileAddition').value=s.prompt_additions.reconcile;
  $('copilotTemporary').checked=(s.copilot||{temporary_chat:true}).temporary_chat!==false;
  $('setupSourceLanguage').value=s.source_language;$('setupTargetLanguage').value=s.target_language;
  if(activeView!=='connect')syncGuidedFromSettings();
  $('stageSettings').innerHTML=Object.keys(stageNames).map(stage=>{
    const c=s.stages[stage];
    return '<div class="config-row"><strong>'+stageNames[stage]+'</strong><div><label for="provider-'+stage+'">Mode / provider</label><select id="provider-'+stage+'">'+[['copilot','Copilot browser · no API key'],['manual','Manual copy / paste'],['mistral','Mistral API'],['anthropic','Claude API']].map(x=>'<option value="'+x[0]+'" '+(x[0]===c.provider?'selected':'')+'>'+x[1]+'</option>').join('')+'</select></div><div><label for="model-'+stage+'">Exact API model id</label><input id="model-'+stage+'" value="'+esc(c.model)+'" placeholder="Load models, then select" list="models-'+stage+'"><datalist id="models-'+stage+'"></datalist></div><div class="workspace-field"><label for="workspace-'+stage+'">Claude workspace id · if required</label><input id="workspace-'+stage+'" value="'+esc(c.workspace_id||'')+'" placeholder="wrkspc_…"></div><button class="button subtle" data-models="'+stage+'">Load available models</button></div><div id="modelResults-'+stage+'" class="models-list hidden"></div>';
  }).join('');
  stages.forEach(stage=>{
    const c=s.stages[stage];
    const mode=c.temperature!=null?'temperature':c.top_p!=null?'top_p':'default';
    $('modelResults-'+stage).insertAdjacentHTML('afterend','<details class="sampling-controls"><summary>Temperature &amp; reasoning · '+stageNames[stage]+'</summary><div class="sampling-grid"><div><label for="sampling-'+stage+'">Sampling control</label><select id="sampling-'+stage+'"><option value="default">Provider default</option><option value="temperature">Temperature</option><option value="top_p">Top-p</option></select></div><div><label for="samplingValue-'+stage+'">Value · 0–1</label><input id="samplingValue-'+stage+'" type="number" min="0" max="1" step="0.05" placeholder="Provider default" value="'+esc(c.temperature??c.top_p??'')+'"></div><div><label for="seed-'+stage+'">Random seed · optional</label><input id="seed-'+stage+'" type="number" min="0" max="2147483647" step="1" value="'+esc(c.random_seed??'')+'"></div><div><label for="effort-'+stage+'">Reasoning effort</label><select id="effort-'+stage+'" data-saved="'+esc(c.effort||'')+'"></select></div></div><p id="samplingNote-'+stage+'" class="muted"></p></details>');
    $('sampling-'+stage).value=mode;updateSampling(stage);
  });
  renderTermRows(s.terminology||[]);
  const reporting=s.reporting||{};
  [['translationScope','translation_scope'],['dialectContext','dialect_context'],['reviewerBackground','reviewer_background'],['decisionSummary','decision_summary']].forEach(([id,key])=>$(id).value=reporting[key]||'');
  settingsRendered=true;settingsDirty=false;
}
const effortLevels={anthropic:[['','Provider default'],['low','Low'],['medium','Medium'],['high','High'],['xhigh','Extra high'],['max','Maximum']],mistral:[['','Provider default'],['none','Off'],['high','High']]};
function updateSampling(stage){
  const provider=$('provider-'+stage).value,mode=$('sampling-'+stage).value,effort=$('effort-'+stage);
  const wanted=effort.options.length?effort.value:effort.dataset.saved||'',levels=effortLevels[provider]||[['','Not available for this mode']];
  effort.innerHTML=levels.map(([v,l])=>'<option value="'+v+'">'+l+'</option>').join('');
  effort.value=levels.some(([v])=>v===wanted)?wanted:'';effort.disabled=!effortLevels[provider];
  $('sampling-'+stage).disabled=provider==='copilot';
  $('samplingValue-'+stage).disabled=provider==='copilot'||mode==='default';
  $('seed-'+stage).disabled=['anthropic','copilot'].includes(provider);
  $('samplingNote-'+stage).textContent=provider==='copilot'?'Copilot does not let other programs set temperature or reasoning. If your Copilot window offers a mode (for example Auto or Think deeper), choose it there before connecting; the label Copilot shows is recorded for every batch.':provider==='anthropic'?'Claude API: values you choose are sent with each request; blank means provider default. Some Claude models accept only their default temperature, and reasoning effort works only on models that support it. If a request is rejected, set it back to Provider default. Claude has no seed setting.':provider==='manual'?'Chat mode: temperature and seed are recorded as user-reported settings only. Set them yourself in the external chat if it offers them.':'Mistral API: values you choose are sent with each request; blank means provider default. Reasoning effort applies to Mistral models with adjustable reasoning. A seed is a sampling control, not a guarantee of identical translations.';
}
function renderTermRows(terms){
  $('termRows').innerHTML=terms.map((t,i)=>'<div class="term-row"><div><label>Source term<input data-term="source" value="'+esc(t.source||'')+'" placeholder="e.g. regenerate"></label></div><div><label>Preferred target term<input data-term="target" value="'+esc(t.target||'')+'" placeholder="Agreed translation"></label></div><div><label>Accepted alternatives · separate with |<input data-term="alternatives" value="'+esc((t.alternatives||[]).join(' | '))+'"></label></div><div><label>Context / definition<input data-term="notes" value="'+esc(t.notes||'')+'"></label></div><label class="checkbox-label"><input type="checkbox" data-term="required" '+(t.required!==false?'checked':'')+'>Required</label><button class="icon-button" type="button" data-remove-term="'+i+'" aria-label="Remove glossary entry">×</button></div>').join('')||'<p class="muted">No terms added. Use Add term to build your team’s lexicon.</p>';
}
function collectTerms(){
  return [...$('termRows').querySelectorAll('.term-row')].map(row=>{
    const get=k=>row.querySelector('[data-term="'+k+'"]').value.trim();
    return {source:get('source'),target:get('target'),alternatives:get('alternatives').split('|').map(x=>x.trim()).filter(Boolean),notes:get('notes'),required:row.querySelector('[data-term="required"]').checked};
  }).filter(t=>t.source||t.target||t.notes||t.alternatives.length);
}
function gatherSettings(){
  const configs={};stages.forEach(stage=>{
    const provider=$('provider-'+stage).value,mode=$('sampling-'+stage).value;
    const raw=$('samplingValue-'+stage).value,seed=$('seed-'+stage).value;
    if(provider!=='copilot'&&mode!=='default'&&raw==='')throw new Error('Enter a temperature / top-p value or choose Provider default.');
    configs[stage]={provider,chat_tool:['manual','copilot'].includes(provider)?(state.project.settings.stages[stage].chat_tool||''):'',chat_model:['manual','copilot'].includes(provider)?(state.project.settings.stages[stage].chat_model||''):'',model:$('model-'+stage).value.trim(),workspace_id:$('workspace-'+stage).value.trim(),temperature:provider!=='copilot'&&mode==='temperature'?Number(raw):null,top_p:provider!=='copilot'&&mode==='top_p'?Number(raw):null,random_seed:!['anthropic','copilot'].includes(provider)&&seed!==''?Number(seed):null,effort:effortLevels[provider]?$('effort-'+stage).value:''};
  });
  return {source_language:$('sourceLanguage').value,target_language:$('targetLanguage').value,study_context:$('studyContext').value,glossary:$('glossary').value,batch_chars:Number($('batchChars').value),batch_segments:Number($('batchSegments').value),max_tokens:Number($('maxTokens').value),stages:configs,prompt_additions:{forward:$('forwardAddition').value,back:'',reconcile:$('reconcileAddition').value},terminology:collectTerms(),reporting:{translation_scope:$('translationScope').value,dialect_context:$('dialectContext').value,reviewer_background:$('reviewerBackground').value,decision_summary:$('decisionSummary').value},copilot:{temporary_chat:$('copilotTemporary').checked}};
}

function updateGuidedTool(){
  const provider=selectedProvider();
  if(provider!==guidedProvider){
    guidedProvider=provider;$('guidedKey').value='';
    const ids=guidedModels[provider]||[];
    fillGuidedModels(ids);
    const configured=state.project.settings.stages.forward,back=state.project.settings.stages.back;
    if(configured.provider===provider){
      [[configured.model,'guidedModel'],[back.provider===provider&&back.model!==configured.model?back.model:'','guidedBackModel']].forEach(([m,id])=>{if(m&&![...$(id).options].some(o=>o.value===m))$(id).insertAdjacentHTML('beforeend','<option value="'+esc(m)+'">'+esc(m)+'</option>');$(id).value=m;});
      $('guidedWorkspace').value=configured.workspace_id||'';
    } else $('guidedWorkspace').value='';
  }
  const choice=document.querySelector('input[name="guidedTool"]:checked').value;
  if(choice==='aau')$('guidedManualTool').value='Mistral chat';
  if(choice==='copilot')$('guidedManualTool').value='Institutional Copilot';
  $('guidedManualTool').classList.toggle('hidden',choice!=='manual');document.querySelector('label[for="guidedManualTool"]').classList.toggle('hidden',choice!=='manual');
  $('manualToolHeading').textContent=choice==='aau'?'Use Mistral chat':choice==='copilot'?'Use Copilot without an API':'Use the AI chat you already have';
  $('institutionChatHelp').textContent=choice==='aau'?'Sign in to Mistral’s Le Chat with the account you use for this study. This option uses chat, without an API key.':choice==='copilot'?'Use your work or school Microsoft account. No API key is needed.':'Choose the external chat interface you will actually use.';
  $('openInstitutionChat').classList.toggle('hidden',!['aau','copilot'].includes(choice));
  $('openInstitutionChat').href=choice==='aau'?'https://chat.mistral.ai':'https://copilot.cloud.microsoft/chat';
  $('openInstitutionChat').textContent=choice==='aau'?'Open Mistral chat ↗':'Open Copilot chat ↗';
  $('guidedApiPanel').classList.toggle('hidden',['manual','copilot'].includes(provider));
  $('guidedCopilotPanel').classList.toggle('hidden',provider!=='copilot');
  $('guidedManualPanel').classList.toggle('hidden',provider!=='manual');
  const chatName=choice==='copilot'?'Copilot':choice==='aau'?'Mistral':'your AI chat';
  $('chatSetupSteps').textContent='1. Copy instructions + interview text. 2. Paste into '+chatName+' and send. 3. Paste its full reply back here. No separate file upload is needed.';
  $('guidedContinue').textContent=provider==='copilot'?(state.copilot?.connecting?'Waiting for Copilot sign-in…':'Connect & translate with Copilot →'):provider==='manual'?'Continue with '+chatName+' →':'Continue to translation →';
  $('guidedContinue').disabled=state.job.running||(provider==='copilot'&&(state.copilot?.connecting||copilotStarting));
  // The Copilot panel has its own start button and progress; avoid two identical buttons.
  $('guidedContinue').classList.toggle('hidden',provider==='copilot');

  $('guidedWorkspaceBox').classList.toggle('hidden',provider!=='anthropic');
  $('guidedApiTitle').textContent=provider==='anthropic'?'Connect Claude':'Connect Mistral';
  $('guidedApiDescription').textContent=provider==='anthropic'?'Use the API key you created in Claude Console. This mode uses Console API credit, rather than chat usage.':'Use a Mistral API key from the account approved for your study. A Mistral chat login alone is not an API key.';
  $('guidedKeyStatus').textContent=state&&state.credentials[provider]?'Key connected for this session. You can leave the key field empty.':'The key is kept only while this app is running.';
}
function fillGuidedModels(ids){
  // Rebuilding the lists (for example after connecting again) keeps the models already chosen.
  const keep=['guidedModel','guidedBackModel'].map(id=>$(id).value);
  const options=ids.map(m=>'<option value="'+esc(m)+'">'+esc(m)+'</option>').join('');
  $('guidedModel').innerHTML='<option value="">'+(ids.length?'Choose a model':'Connect first to see your available models')+'</option>'+options;
  $('guidedBackModel').innerHTML='<option value="">Same model as above</option>'+options;
  ['guidedModel','guidedBackModel'].forEach((id,i)=>{if(keep[i]&&ids.includes(keep[i]))$(id).value=keep[i];});
}
function syncGuidedFromSettings(){
  const c=state.project.settings.stages.forward;
  if(!state.project.run_count&&c.provider==='manual'&&!c.chat_tool)return;
  const savedTool=c.chat_tool||manualLabel;
  const choice=c.provider==='manual'?(/Mistral/.test(savedTool)?'aau':'manual'):c.provider;
  document.querySelector('input[name="guidedTool"][value="'+choice+'"]').checked=true;
  if(c.chat_tool)$('guidedManualTool').value=c.chat_tool;
  $('guidedManualModel').value=c.chat_model||'';
  guidedProvider='';updateGuidedTool();
}
function activeChat(){
  const c=state.project.settings.stages[activeStage],tool=c.chat_tool||manualLabel;
  if(tool.includes('Copilot'))return {name:'Copilot',url:'https://copilot.cloud.microsoft/chat'};
  if(tool.includes('Mistral'))return {name:'Mistral',url:'https://chat.mistral.ai'};
  if(tool.includes('Claude'))return {name:'Claude',url:'https://claude.ai'};
  if(tool.includes('ChatGPT'))return {name:'ChatGPT',url:'https://chatgpt.com'};
  return {name:'your AI chat',url:''};
}
function renderChatGuide(show){
  $('chatGuide').classList.toggle('hidden',!show);
  const chat=activeChat(),back=activeStage==='back',reconcile=activeStage==='reconcile';
  const contents=back?'translated text':reconcile?'all three versions':'interview text';
  $('generatePrompt').textContent='1. Copy instructions + '+contents;
  $('chatGuideTitle').textContent='Using '+chat.name+' without an API';
  $('chatQuickSteps').textContent='1. Copy instructions + '+contents+'. 2. Paste into '+chat.name+' and send. 3. Paste its full reply here.';
  $('chatAccessNote').textContent=back?'This packet contains the forward-translated text only. The original is excluded. Use a new, empty chat for every back-translation batch.':reconcile?'This packet includes the original, forward translation, and back-translation for comparison. No separate file upload is needed.':'The copied packet already contains the interview text for this batch. No separate file upload is needed.';
  $('pasteChatHeading').textContent='2. Paste everything into a new '+chat.name+' chat and send';
  $('chatInstruction').textContent='The text travels with the instructions. Wait for '+chat.name+' to finish, then copy its full reply back here.';
  $('openWorkflowChat').classList.toggle('hidden',!chat.url);if(chat.url)$('openWorkflowChat').href=chat.url;
  $('openWorkflowChat').textContent='Open '+chat.name+' chat ↗';
  $('manualResponse').placeholder='Paste '+chat.name+"'s full reply, including its section IDs. Do not paste the prompt back here.";
  if(currentTask)renderPacketPreview();
}
function renderPacketPreview(){
  if(!currentTask)return;
  const payload=JSON.parse(currentTask.user),rows=payload.segments,stage=currentTask.stage;
  const contents=stage==='back'?'forward-translated text only':stage==='reconcile'?'original + forward translation + back-translation':'original interview text';
  $('packetTitle').textContent='The copied packet includes '+contents;
  const context=payload.context_only||[];
  $('packetDescription').textContent=rows.length+' section'+(rows.length===1?'':'s')+' · '+currentTask.ids.join(', ')+(context.length?' + '+context.length+' neighbouring context section'+(context.length===1?'':'s'):'')+'. Paste the entire packet into '+activeChat().name+'.'+(stage==='back'?' The original interview is not included.':'');
  const versions=row=>stage==='reconcile'?[['Original',row.original_source],['Forward',row.forward_translation],['Blind back-translation',row.blind_back_translation]]:[[stage==='back'?'Translated text':'Interview text',row.text]];
  const first=versions(rows[0])[0][1]||'';$('packetExcerpt').textContent=first.length>240?first.slice(0,240)+'…':first;
  $('packetTexts').innerHTML=rows.map(row=>'<article><strong>'+esc(row.id)+'</strong>'+versions(row).map(([label,text])=>'<h4>'+label+'</h4><p>'+esc(text)+'</p>').join('')+'</article>').join('')+(context.length?'<article><h4>Neighbouring text · context only</h4>'+context.map(row=>'<p>'+esc(row.text||row.original_source||'')+'</p>').join('')+'</article>':'');
}
async function copyCurrentPrompt(){
  try{await navigator.clipboard.writeText($('promptText').value);}
  catch(e){
    document.querySelector('.prompt-details').open=true;$('promptText').select();
    if(!document.execCommand('copy'))throw new Error('The prompt is ready below. Select it and press Command+C to copy.');
  }
}
async function startOwn(){
  if(state.project.segments.length&&!state.project.demo&&!confirm('Start a new transcript? Download your current project first if you want to keep it.'))return;
  await api('reset',{});manualLabel='';guidedModels={};await refresh(true);
  const settings=structuredClone(state.project.settings);stages.forEach(stage=>settings.stages[stage]={...settings.stages[stage],provider:'copilot',chat_tool:'Institutional Copilot',model:'',chat_model:'',temperature:null,top_p:null,random_seed:null,effort:''});await api('settings',{settings});await refresh(true);
  document.querySelector('input[name="guidedTool"][value="copilot"]').checked=true;guidedProvider='';updateGuidedTool();
  $('welcome').classList.add('hidden');$('sourceForm').classList.remove('hidden');
  $('sourceText').value='';$('projectTitle').value='Untitled interview';$('fileName').textContent='Word (.docx), text (.txt), or Markdown · up to 8 MB';
  currentInspection=null;uploadedBlocks=null;$('cleaningPanel').classList.add('hidden');selectedSeverities.clear();
  go('source');$('setupSourceLanguage').focus();
}
function openNextReview(){
  const next=visibleReviewSegments().filter(s=>s.reconcile&&!s.review).sort((a,b)=>severityRank[issueTier(b)]-severityRank[issueTier(a)])[0];
  if(next)openReview(next.id);
  else if(state.project.segments.some(s=>s.reconcile&&!s.review)){toast('No pending sections in these filters. Choose All severities to continue.');}else {go('export');toast('All available sections have decisions. Download your work.');}
}
async function exportFile(kind){
  const response=await fetch('/api/export',{method:'POST',headers:{'Content-Type':'application/json','X-QualiTranslate-Token':token},body:JSON.stringify({kind:kind})});
  if(!response.ok){const data=await response.json();throw new Error(data.error||'Export failed.');}
  const blob=await response.blob(), url=URL.createObjectURL(blob);
  const match=(response.headers.get('Content-Disposition')||'').match(/filename="([^"]+)"/);
  const a=document.createElement('a');a.href=url;a.download=match?match[1]:'qualitranslate-export';document.body.appendChild(a);a.click();a.remove();setTimeout(()=>URL.revokeObjectURL(url),1500);toast('Export downloaded.');
}
function openReview(id){
  reviewId=id;const s=state.project.segments.find(x=>x.id===id);if(!s||!s.reconcile)return;
  $('dialogTitle').textContent=id+' · '+(s.speaker||'Unlabelled speaker');
  $('dialogCompare').innerHTML=[['Original source',s.source],['Forward',textOf(s,'forward')],['Blind back-translation',textOf(s,'back')]].map(x=>'<div><h4>'+x[0]+'</h4><p>'+esc(x[1])+'</p></div>').join('');
  $('dialogIssues').innerHTML=s.reconcile.issues.length?'<div class="dialog-issues">'+s.reconcile.issues.map(i=>'<p><strong>'+esc(i.severity.toUpperCase()+' / '+i.category)+'</strong> · '+esc(i.detail)+'</p>').join('')+'</div>':'<p class="muted">The model identified no issue. Compare the source before recording your decision.</p>';
  const r=s.review||{};$('finalText').value=r.text||s.reconcile.text;$('reviewerName').value=r.reviewer||$('quickReviewer').value||$('reviewerName').value;$('reviewNote').value=r.note||'';$('retainUncertainty').checked=r.status==='reviewed_uncertainty_retained';$('dialogError').classList.add('hidden');$('reviewDialog').showModal();
}
function openEdit(id,stage){
  const s=state.project.segments.find(x=>x.id===id);if(!s)return;
  editing={id:id,stage:stage,human:false};$('editTitle').textContent='Correct '+id+' · '+(stage==='source'?'source':stageNames[stage]);$('editText').value=stage==='source'?s.source:textOf(s,stage);$('editNote').value='';$('editError').classList.add('hidden');
  $('editContext').classList.add('hidden');$('editHelp').textContent='Editing the source or an earlier translation clears later stages and reviews, because context can affect other sections. Earlier run records stay in the history.';$('editNoteLabel').textContent='Reason for correction';$('editSubmit').textContent='Save correction';
  $('editDialog').showModal();
}
function openHumanEntry(id,stage){
  const s=state.project.segments.find(x=>x.id===id);if(!s)return;
  editing={id:id,stage:stage,human:true};
  const P=state.project.settings;
  $('editTitle').textContent='Add a human '+({forward:'forward translation',back:'blind back-translation',reconcile:'proposed translation'})[stage]+' · '+id;
  const shown=stage==='forward'?[['Original ('+P.source_language+')',s.source]]:stage==='back'?[['Forward translation ('+P.target_language+')',textOf(s,'forward')]]:[['Original ('+P.source_language+')',s.source],['Forward translation',textOf(s,'forward')],['Blind back-translation',textOf(s,'back')]];
  $('editContext').innerHTML=shown.map(x=>'<strong>'+esc(x[0].toUpperCase())+'</strong>'+esc(x[1])).join('<br><br>');$('editContext').classList.remove('hidden');
  $('editHelp').textContent=stage==='back'?'Copilot declined this section. To keep the back-translation blind, translate only the forward translation shown here, without looking at the original. Enter the text in '+P.source_language+'.':stage==='reconcile'?'Copilot declined to reconcile this section. Compare all three versions and enter the proposed '+P.target_language+' translation. You will still record the final decision in Review.':'Copilot declined this section. Enter a translation into '+P.target_language+'. It is recorded as human work, not model output.';
  $('editText').value='';$('editNote').value='';$('editNoteLabel').textContent='Who translated it, and how (recorded in the method log)';$('editSubmit').textContent='Save human translation';
  $('editError').classList.add('hidden');$('editDialog').showModal();
}
function renderHumanNeeded(){
  if(!state)return;
  const items=(state.job.declined||[]).filter(d=>{if(d.project_id&&d.project_id!==state.project.project_id)return false;const s=state.project.segments.find(x=>x.id===d.id);return s&&!s[d.stage]&&(d.stage==='forward'||(s.forward&&(d.stage==='back'||s.back)));});
  $('humanNeeded').classList.toggle('hidden',!items.length||state.job.running);
  $('humanNeededList').innerHTML=items.map(d=>{const s=state.project.segments.find(x=>x.id===d.id);return '<div class="human-item"><strong>'+esc(d.id)+'<small>'+esc(stageNames[d.stage])+'</small></strong><div>'+esc(d.stage==='forward'?s.source:'(blind: forward translation shown when you add it)')+(d.reply?'<small>Copilot said: '+esc(d.reply.slice(0,160))+'</small>':'')+'</div><button class="button small" data-human="'+esc(d.id)+'" data-stage="'+esc(d.stage)+'">Add translation</button></div>';}).join('');
}
document.addEventListener('click',event=>{
  const risk=event.target.closest('[data-risk]');if(risk){const tier=risk.dataset.risk;if(tier==='all')selectedSeverities.clear();else if(selectedSeverities.has(tier))selectedSeverities.delete(tier);else selectedSeverities.add(tier);renderReview();return;}
  const approve=event.target.closest('[data-approve]');if(approve){act(async()=>{const reviewer=$('quickReviewer').value.trim();if(!reviewer){$('quickReviewer').focus();throw new Error('Enter your reviewer name once above, then approve.');}const seg=state.project.segments.find(s=>s.id===approve.dataset.approve);await api('review',{id:seg.id,text:seg.reconcile.text,reviewer,note:'Accepted unchanged using quick approval.',method:'quick_accept'});await refresh(true);toast('Draft approved.');});return;}
  const clear=event.target.closest('[data-clear-review]');if(clear){act(async()=>{await api('review-clear',{id:clear.dataset.clearReview});await refresh(true);toast('Section returned to pending. The previous decision remains in the history.');});return;}
  const remove=event.target.closest('[data-remove-term]');if(remove){const terms=collectTerms();terms.splice(Number(remove.dataset.removeTerm),1);renderTermRows(terms);settingsDirty=true;return;}
  const nav=event.target.closest('button[data-view]');if(nav){go(nav.dataset.view);return;}
  const goto=event.target.closest('[data-goto]');if(goto){go(goto.dataset.goto);return;}
  const sc=event.target.closest('[data-stagecard]');if(sc){setStage(sc.dataset.stagecard);return;}
  const ex=event.target.closest('[data-export]');if(ex){act(()=>exportFile(ex.dataset.export));return;}
  const review=event.target.closest('[data-review]');if(review){openReview(review.dataset.review);return;}
  const edit=event.target.closest('[data-edit]');if(edit){openEdit(edit.dataset.edit,edit.dataset.stage);return;}
  const human=event.target.closest('[data-human]');if(human){openHumanEntry(human.dataset.human,human.dataset.stage);return;}
  const key=event.target.closest('[data-savekey]');if(key){act(async()=>{const provider=key.dataset.savekey;await api('key',{provider:provider,key:$(provider+'Key').value});$(provider+'Key').value='';await refresh();toast('Credential stored only for this running session.');});return;}
  const models=event.target.closest('[data-models]');if(models){act(async()=>{
    const stage=models.dataset.models,provider=$('provider-'+stage).value;if(['manual','copilot'].includes(provider))throw new Error('Chat modes use their own model controls, without an API model list.');
    models.disabled=true;models.textContent='Loading…';
    try{const data=await api('models',{provider:provider,workspace_id:$('workspace-'+stage).value});$('models-'+stage).innerHTML=data.models.map(m=>'<option value="'+esc(m)+'"></option>').join('');const box=$('modelResults-'+stage);box.textContent=data.models.length?data.models.join('\n'):'No models returned for this account.';box.classList.remove('hidden');toast('Model list loaded. Choose an exact id, then save settings.');}
    finally{models.disabled=false;models.textContent='Load available models';}
  });}
});
function sourceSettings(){
  const settings=structuredClone(state.project.settings);
  settings.source_language=$('setupSourceLanguage').value.trim();settings.target_language=$('setupTargetLanguage').value.trim();
  if(!settings.source_language||!settings.target_language)throw new Error('Enter both languages first.');
  if(settings.source_language.toLowerCase()===settings.target_language.toLowerCase())throw new Error('Choose two different languages.');
  return settings;
}
function renderCleaning(){
  if(!currentInspection)return;
  const blocks=currentInspection.blocks,shown=$('showAllBlocks').checked?blocks:blocks.filter(b=>b.needs_check);
  const excluded=blocks.length-includedBlocks.size;
  $('cleaningSummary').textContent=currentInspection.suspected+' blocks need a boundary check. '+excluded+' currently excluded; '+includedBlocks.size+' included.';
  $('cleaningBlocks').innerHTML=shown.map(b=>'<article class="cleaning-block '+(includedBlocks.has(b.id)?'included':'excluded')+'"><div class="cleaning-block-heading"><strong>'+esc(b.id)+' · '+esc(b.kind.replace(/_/g,' '))+'</strong><label class="checkbox-label"><input type="checkbox" data-include-block="'+b.id+'" '+(includedBlocks.has(b.id)?'checked':'')+'>Include in translation</label></div>'+(b.reason?'<p class="boundary-reason">'+esc(b.reason)+'</p>':'')+'<pre>'+esc(b.text)+'</pre></article>').join('');
  $('confirmCleaning').textContent='Prepare interview · exclude '+excluded+' block'+(excluded===1?'':'s')+' →';
  $('confirmCleaning').disabled=!includedBlocks.size;
}
async function confirmSourceSelection(){
  if(!currentInspection)throw new Error('Check your transcript first.');
  await api('import-selected',{inspection_id:currentInspection.inspection_id,included_ids:currentInspection.blocks.filter(b=>includedBlocks.has(b.id)).map(b=>b.id),title:$('projectTitle').value.trim()||'Untitled interview',settings:sourceSettings(),split_speakers:$('splitSpeakers').checked});
  currentInspection=null;$('cleaningPanel').classList.add('hidden');await refresh(true);toast('Only the selected interview blocks are prepared. Check the preview, then choose a tool.');$('sourcePreview').scrollIntoView({behavior:'smooth',block:'start'});
}
$('importText').addEventListener('click',()=>act(async()=>{
  sourceSettings();
  if(!$('sourceText').value.trim())throw new Error('Upload a transcript or paste interview text first.');
  if(state.project.segments.length&&!confirm('Replace the current transcript? Save a project copy first if needed.'))return;
  currentInspection=await api('inspect',{text:$('sourceText').value,blocks:uploadedBlocks});
  includedBlocks=new Set(currentInspection.blocks.filter(b=>!b.suggested_exclude).map(b=>b.id));
  if(currentInspection.suspected){$('cleaningPanel').classList.remove('hidden');renderCleaning();$('cleaningPanel').scrollIntoView({behavior:'smooth',block:'start'});}
  else await confirmSourceSelection();
}));
$('confirmCleaning').addEventListener('click',()=>act(confirmSourceSelection));
$('showAllBlocks').addEventListener('change',renderCleaning);
$('keepAllBlocks').addEventListener('click',()=>{includedBlocks=new Set(currentInspection.blocks.map(b=>b.id));renderCleaning();});
$('excludeSuggested').addEventListener('click',()=>{includedBlocks=new Set(currentInspection.blocks.filter(b=>!b.suggested_exclude).map(b=>b.id));renderCleaning();});
$('cancelCleaning').addEventListener('click',()=>{currentInspection=null;$('cleaningPanel').classList.add('hidden');$('sourceText').focus();});
$('sourceText').addEventListener('input',()=>{currentInspection=null;uploadedBlocks=null;$('cleaningPanel').classList.add('hidden');});
$('loadDemo').addEventListener('click',()=>act(async()=>{
  if(state.project.segments.length&&!confirm('Replace this workspace with the synthetic demonstration?'))return;
  await api('demo',{});await api('run',{stage:'all',demo:true});await refresh(true);
  if(complete())go('review');else go('workflow');
  selectedSeverities.clear();renderReview();toast('Demo: S0004 is critical, S0005 major, and S0006 minor. Try the severity filters.');
}));
$('startOwn').addEventListener('click',()=>act(startOwn));
$('demoToOwn').addEventListener('click',()=>act(startOwn));
document.querySelectorAll('input[name="guidedTool"]').forEach(el=>el.addEventListener('change',updateGuidedTool));
$('guidedConnect').addEventListener('click',()=>act(async()=>{
  const provider=selectedProvider();
  const key=$('guidedKey').value.trim();
  if(!key&&!state.credentials[provider])throw new Error('Paste your '+(provider==='anthropic'?'Claude':'Mistral')+' API key first. You can also use Mistral chat, Copilot, or Other AI chat without a key.');
  const button=$('guidedConnect');button.disabled=true;button.textContent='Connecting…';
  try{
    if(key){await api('key',{provider:provider,key:key});$('guidedKey').value='';}
    const data=await api('models',{provider:provider,workspace_id:provider==='anthropic'?$('guidedWorkspace').value.trim():''});
    guidedModels[provider]=data.models;
    fillGuidedModels(data.models);
    if(data.models.length===1)$('guidedModel').value=data.models[0];
    await refresh();toast(data.models.length?'Connected. Choose a model from the list, then continue.':'Connected, but no models were returned for this account.');
  }finally{button.disabled=false;button.textContent='Connect & show models';}
}));
$('guidedContinue').addEventListener('click',()=>act(async()=>{
  const provider=selectedProvider();
  if(provider==='copilot'){await startCopilotTranslation();return;}
  const settings=structuredClone(state.project.settings);
  if(['anthropic','mistral'].includes(provider)&&!state.credentials[provider])throw new Error('Connect your API key first, or choose a chat option without an API key.');
  if(['anthropic','mistral'].includes(provider)&&!$('guidedModel').value)throw new Error('Choose a model from the list first.');
  if(provider==='copilot'&&!state.copilot?.ready)throw new Error('Open Copilot, sign in yourself, and check the connection first.');
  const model=['manual','copilot'].includes(provider)?'':$('guidedModel').value;
  const modelFor=stage=>stage==='back'&&['anthropic','mistral'].includes(provider)?($('guidedBackModel').value||model):model;
  const workspace=provider==='anthropic'?$('guidedWorkspace').value.trim():'';
  const chatTool=provider==='copilot'?'Institutional Copilot':provider==='manual'?$('guidedManualTool').value:'';
  const chatModel=provider==='manual'?$('guidedManualModel').value.trim():'';
  const changed=stages.some(stage=>{const c=settings.stages[stage];return c.provider!==provider||c.model!==modelFor(stage)||(c.workspace_id||'')!==workspace||(c.chat_tool||'')!==chatTool||(c.chat_model||'')!==chatModel;});
  if(changed&&state.project.segments.some(s=>s.forward)&&!confirm('Changing the tool clears existing translation drafts and reviews. Save a project copy first if you want to keep them. Continue?'))return;
  stages.forEach(stage=>{const previous=settings.stages[stage];settings.stages[stage]={...previous,provider:provider,model:modelFor(stage),workspace_id:workspace,chat_tool:chatTool,chat_model:chatModel};if(previous.provider!==provider||provider==='copilot')Object.assign(settings.stages[stage],{temperature:null,top_p:null,random_seed:null,effort:''});});
  if(provider==='manual'){
    manualLabel=$('guidedManualTool').value+' / '+($('guidedManualModel').value.trim()||'model not reported');
    $('manualModel').value=manualLabel;
  }
  await api('settings',{settings:settings});await refresh(true);setStage(nextStage());go('workflow');
}));
$('transcriptFile').addEventListener('change',()=>act(async()=>{
  const file=$('transcriptFile').files[0];if(!file)return;if(file.size>8000000)throw new Error('File exceeds 8 MB.');
  const data=await new Promise((resolve,reject)=>{const reader=new FileReader();reader.onload=()=>resolve(String(reader.result).split(',')[1]);reader.onerror=reject;reader.readAsDataURL(file);});
  const result=await api('file',{filename:file.name,data:data});uploadedBlocks=result.blocks;currentInspection=null;$('cleaningPanel').classList.add('hidden');$('sourceText').value=result.text;$('fileName').textContent=file.name;$('projectTitle').value=file.name.replace(/\.[^.]+$/,'');toast('File loaded. Check & prepare interview will look for headers, notes, and tables.');
}));
$('projectFile').addEventListener('change',()=>act(async()=>{
  const file=$('projectFile').files[0];if(!file)return;if(file.size>20000000)throw new Error('Project exceeds 20 MB.');
  if(state.project.segments.length&&!confirm('Replace the current project with this saved project?'))return;
  let project;try{project=JSON.parse(await file.text());}catch(e){throw new Error('The project file is not valid JSON.');}
  await api('project',{project:project});
  const prior=[...(project.runs||[])].reverse().find(r=>r.provenance&&r.provenance.tool_model_reported_by_user);
  manualLabel=prior?prior.provenance.tool_model_reported_by_user:'';$('manualModel').value=manualLabel;
  await refresh(true);go(complete()?'review':'workflow');toast('Project restored. Reconnect your API key if needed.');
}));
$('generatePrompt').addEventListener('click',()=>act(async()=>{
  if(!currentTask){currentTask=await api('prompt',{stage:activeStage});$('manualResponse').value='';}
  $('promptText').value=currentTask.copy_text;$('promptIds').textContent=currentTask.ids.join(', ');
  $('manualBox').classList.remove('hidden');$('manualReplyError').classList.add('hidden');$('blindNotice').classList.toggle('hidden',activeStage!=='back');renderChatGuide(true);
  if(!$('manualModel').value)$('manualModel').value=manualLabel;
  await copyCurrentPrompt();toast('Instructions and text copied. Paste the entire packet into '+activeChat().name+'.');$('manualBox').scrollIntoView({behavior:'smooth',block:'start'});
}));
$('copyPrompt').addEventListener('click',()=>act(async()=>{
  await copyCurrentPrompt();toast('The full packet is copied: instructions + text. Paste it into '+activeChat().name+'.');
}));
$('applyManual').addEventListener('click',()=>act(async()=>{
  if(!currentTask)throw new Error('Prepare a new prompt first.');
  if(!$('manualModel').value.trim())throw new Error('Open Tool / model record below and enter the AI tool you actually used.');
  manualLabel=$('manualModel').value.trim();
  $('manualReplyError').classList.add('hidden');
  try{await api('manual',{task_id:currentTask.task_id,response:$('manualResponse').value,model_label:manualLabel});}
  catch(error){
    const message=/JSON|Segment ids|segments array/i.test(error.message)?activeChat().name+' must return the full structured reply with all section IDs. Copy the whole reply or code block, not just the translated sentences. '+error.message:error.message;
    $('manualReplyError').textContent=message;$('manualReplyError').classList.remove('hidden');$('manualReplyError').scrollIntoView({behavior:'smooth',block:'center'});return;
  }
  currentTask=null;$('manualBox').classList.add('hidden');await refresh(true);
  if(complete()){go('review');toast('All three stages are complete. Review the proposed translation.');}
  else {setStage(nextStage());toast('Reply saved. Next: '+stageNames[activeStage].toLowerCase()+'. Copy instructions + text to continue.');}
}));
$('runStage').addEventListener('click',()=>act(async()=>{await api('run',{stage:activeStage});await refresh();if(!state.job.running){setStage(nextStage());if(complete())go('review');}}));
$('runAll').addEventListener('click',()=>act(async()=>{
  const pending=stages.filter(stage=>state.project.segments.some(s=>!s[stage]));
  if(pending.some(stage=>state.project.settings.stages[stage].provider==='manual'))throw new Error('A later stage uses copy and paste. Use Run just this stage, or choose one tool for all stages in Step 2.');
  if(pending.some(stage=>state.project.settings.stages[stage].provider==='copilot')){await startCopilotTranslation();return;}
  if(pending.some(stage=>['anthropic','mistral'].includes(state.project.settings.stages[stage].provider)&&(!state.credentials[state.project.settings.stages[stage].provider]||!state.project.settings.stages[stage].model))){go('connect');showError('Connect your API key and choose a model to start.');return;}
  await api('run',{stage:'all'});await refresh();if(complete()&&!state.job.running)go('review');
}));
$('runDemo').addEventListener('click',()=>act(async()=>{await api('run',{stage:'all',demo:true});await refresh(true);if(complete())go('review');}));
$('stopJob').addEventListener('click',()=>act(async()=>{await api('stop',{});toast('Stopping. Completed batches are kept. A Copilot reply that is still being written will not be imported; an API request may take up to two minutes to return.');}));
$('saveSettings').addEventListener('click',()=>act(async()=>{
  if(state.project.segments.some(s=>s.forward)&&!confirm('Changes to models, languages or instructions clear completed outputs. Have you exported a copy if you need it?'))return;
  await api('settings',{settings:gatherSettings()});settingsDirty=false;await refresh(true);if(state.project.segments.length)go('workflow');toast('Settings saved.');
}));
$('settings').addEventListener('input',()=>{settingsDirty=true;});
$('settings').addEventListener('change',()=>{settingsDirty=true;});
$('clearKeys').addEventListener('click',()=>act(async()=>{await api('key',{provider:'mistral',key:''});await api('key',{provider:'anthropic',key:''});await refresh();toast('Both API keys forgotten.');}));
$('quickSave').addEventListener('click',()=>act(()=>exportFile('json')));
$('copyMethods').addEventListener('click',()=>act(async()=>{
  try{await navigator.clipboard.writeText($('methodsParagraph').value);}
  catch(e){$('methodsParagraph').select();if(!document.execCommand('copy'))throw new Error('Select the paragraph and press Command+C to copy.');}
  toast('Paragraph copied. Check it and fill in the [bracketed] items.');
}));
$('reset').addEventListener('click',()=>act(async()=>{if(!confirm('Clear the current transcript and API keys? Save your project first to keep it.'))return;await api('reset',{});$('sourceText').value='';$('projectTitle').value='Untitled interview';currentTask=null;manualLabel='';guidedModels={};await refresh(true);go('source');}));
function copilotRunning(){return !!(state&&state.job.running&&['forward','back','reconcile'].includes(state.job.stage)&&state.project.settings.stages[state.job.stage]?.provider==='copilot');}
function renderCopilotStatus(){
  const c=state.copilot||{},waiting=!!c.connecting,phase=c.connection_phase||'idle',running=copilotRunning();
  const failed=['error','timeout'].includes(phase);
  const signedIn=c.ready||running||(c.open&&!['sign_in','browser'].includes(c.waiting_for)&&phase!=='opening'&&(c.page_host||'').includes('cloud.microsoft'));
  const steps=[
    ['WINDOW',c.open?'Copilot window open':'Opens a separate Chrome window',c.open||running?'done':waiting?'active':''],
    ['SIGN-IN',signedIn?'Signed in to Copilot':c.waiting_for==='sign_in'?'Sign in now in the Copilot window':'Your work or school Microsoft account',signedIn?'done':(c.open&&waiting)?'active':''],
    ['MESSAGE BOX',c.ready||running?'Found · ready':'Checked automatically',c.ready||running?'done':(signedIn&&waiting)?'active':''],
    ['TRANSLATING',running?(state.job.message||'Running'):complete()?'All three stages done':'Starts automatically',running?'active':complete()?'done':'']];
  if(failed){const i=steps.findIndex(x=>x[2]!=='done');if(i>=0)steps[i][2]='blocked';}
  $('copilotMilestones').innerHTML=steps.map(x=>'<li class="'+x[2]+'"><b>'+x[0]+'</b>'+esc(x[1])+'</li>').join('');
  const message=running?'Copilot: '+(c.step_detail||'working')+' ('+(c.step_elapsed||0)+' s). '+(state.job.message||''):(c.message||'Not connected yet.');
  $('copilotConnection').textContent=message+(waiting?' · '+(c.connection_elapsed||0)+' s (stops waiting after '+Math.round((c.connection_timeout||600)/60)+' min)':'');
  $('copilotConnection').dataset.phase=phase;
  $('copilotConnection').classList.toggle('amber',!c.ready&&!running&&!failed);
  $('copilotConnection').classList.toggle('error',failed);
  $('openCopilot').disabled=state.job.running||waiting||copilotStarting;
  $('openCopilot').textContent=waiting?'Waiting for Copilot…':'Connect & translate with Copilot';
  $('checkCopilot').disabled=state.job.running||copilotStarting||waiting;
  $('checkCopilot').classList.toggle('hidden',!failed&&!(state.job.error&&!state.job.running));
  $('closeCopilot').disabled=state.job.running;
  $('forgetCopilot').disabled=state.job.running;
  $('cancelCopilotConnect').classList.toggle('hidden',!waiting&&!pendingCopilotStart);
  $('cancelCopilotConnect').disabled=state.job.running;
  const hint=waiting?(c.waiting_for==='sign_in'?'Switch to the Copilot window (a separate Chrome window) and sign in. This page will continue by itself.':'Checking the Copilot page… Translation starts automatically when it is ready.'):running?'Leave both windows open. You can keep working in other apps; Save progress stays available.':c.ready?'Connected. Nothing has been sent yet.':'';
  $('copilotConnectHint').textContent=hint;$('copilotConnectHint').classList.toggle('hidden',!hint);
}
async function startCopilotTranslation(){
  if(copilotStarting||state.job.running)return;
  if(!state.project.segments.length)throw new Error('Prepare an interview before connecting Copilot.');
  copilotStarting=true;
  try{
    const settings=structuredClone(state.project.settings);
    const changed=stages.some(s=>settings.stages[s].provider!=='copilot');
    if(changed&&state.project.segments.some(s=>s.forward)&&!confirm('Switching to Copilot clears existing drafts. Save a project copy first if needed. Continue?'))return;
    if(changed){stages.forEach(s=>settings.stages[s]={...settings.stages[s],provider:'copilot',chat_tool:'Institutional Copilot',chat_model:'',model:'',workspace_id:'',temperature:null,top_p:null,random_seed:null,effort:''});await api('settings',{settings});await refresh(true);}
    pendingCopilotStart={project_id:state.project.project_id,revision:state.project.revision};
    $('copilotConnection').textContent='Opening Copilot and starting automatic connection checks…';
    $('copilotConnectHint').textContent='Sign in in its separate window. Translation will start after the chat controls are detected.';
    const c=await api('copilot/open',{});state.copilot=c;renderCopilotStatus();
    await refresh();
  }catch(e){pendingCopilotStart=null;state.copilot={...(state.copilot||{}),connecting:false,ready:false,connection_phase:'error',message:e.message};throw e;}
  finally{copilotStarting=false;updateGuidedTool();renderCopilotStatus();}
}
async function advanceCopilotConnection(){
  if(!pendingCopilotStart)return;
  const intent=pendingCopilotStart,c=state.copilot||{};
  const usesCopilot=stages.some(s=>state.project.settings.stages[s].provider==='copilot'&&state.project.segments.some(seg=>!seg[s]));
  if(intent.project_id!==state.project.project_id||intent.revision!==state.project.revision||!usesCopilot){
    pendingCopilotStart=null;await api('copilot/cancel',{});
    showError(intent.project_id!==state.project.project_id||intent.revision!==state.project.revision?'The interview or its settings changed while Copilot was connecting, so nothing was sent. Click Resume with Copilot to translate the current version.':'No remaining stage is set to Copilot, so nothing was sent.');
    return;
  }
  if(['error','timeout','cancelled'].includes(c.connection_phase)){
    pendingCopilotStart=null;if(c.connection_phase!=='cancelled')showError(c.message);return;
  }
  if(!c.ready||state.job.running)return;
  pendingCopilotStart=null;go('workflow');
  await api('run',{stage:'all',expected_project_id:intent.project_id,expected_revision:intent.revision});await refresh();
  if(complete()&&!state.job.running)go('review');
}
$('openCopilot').addEventListener('click',()=>act(startCopilotTranslation));
$('checkCopilot').addEventListener('click',()=>act(startCopilotTranslation));
$('cancelCopilotConnect').addEventListener('click',()=>act(async()=>{pendingCopilotStart=null;await api('copilot/cancel',{});await refresh();toast('Connection cancelled. Your project is still available.');}));
$('closeCopilot').addEventListener('click',()=>act(async()=>{pendingCopilotStart=null;await api('copilot/close',{});await refresh();toast('Copilot window closed. Your sign-in is kept for next time.');}));
$('forgetCopilot').addEventListener('click',()=>act(async()=>{if(!confirm('Close the Copilot window and remove the saved Copilot sign-in from this computer? Your Microsoft 365 chat history is not affected.'))return;pendingCopilotStart=null;await api('copilot/forget',{});await refresh();toast('Saved Copilot sign-in removed from this computer.');}));
$('methodToggle').addEventListener('click',()=>{const expanded=$('processOverview').classList.toggle('expanded');$('methodToggle').setAttribute('aria-expanded',String(expanded));$('methodToggle').textContent=expanded?'Hide details':'How it works';});
$('addTerm').addEventListener('click',()=>{const terms=collectTerms();terms.push({source:'',target:'',alternatives:[],notes:'',required:true});renderTermRows(terms);settingsDirty=true;});
$('settings').addEventListener('change',event=>{const stage=stages.find(s=>event.target.id==='provider-'+s||event.target.id==='sampling-'+s);if(stage)updateSampling(stage);});
$('cleaningBlocks').addEventListener('change',event=>{const id=event.target.dataset.includeBlock;if(id){if(event.target.checked)includedBlocks.add(id);else includedBlocks.delete(id);renderCleaning();}});
$('approveMinors').addEventListener('click',()=>{
  const rows=state.project.segments.filter(s=>s.reconcile&&!s.review&&issueTier(s)==='minor');if(!rows.length)return;
  batchSelection={ids:rows.map(s=>s.id),revision:state.project.revision};
  $('batchSummary').textContent='Accept the unchanged proposed translations for '+rows.length+' minor-only sections?';
  $('batchItems').innerHTML=rows.map(s=>'<article><strong>'+esc(s.id)+' · '+esc(s.speaker)+'</strong><p>'+esc(s.reconcile.text)+'</p></article>').join('');
  $('batchReviewer').value=$('quickReviewer').value;$('batchError').classList.add('hidden');$('batchDialog').showModal();
});
$('closeBatch').addEventListener('click',()=>$('batchDialog').close());
$('batchForm').addEventListener('submit',async event=>{event.preventDefault();try{await api('review-batch',{...batchSelection,reviewer:$('batchReviewer').value.trim()});$('quickReviewer').value=$('batchReviewer').value.trim();$('batchDialog').close();await refresh(true);toast('Minor-only batch accepted. Major and critical sections remain pending.');}catch(e){$('batchError').textContent=e.message;$('batchError').classList.remove('hidden');}});
$('reviewFilter').addEventListener('change',renderReview);
$('reviewNext').addEventListener('click',openNextReview);
$('closeDialog').addEventListener('click',()=>$('reviewDialog').close());
$('closeEdit').addEventListener('click',()=>$('editDialog').close());
$('reviewForm').addEventListener('submit',async event=>{
  event.preventDefault();try{$('quickReviewer').value=$('reviewerName').value;await api('review',{id:reviewId,text:$('finalText').value,reviewer:$('reviewerName').value,note:$('reviewNote').value,uncertainty:$('retainUncertainty').checked});$('reviewDialog').close();await refresh(true);if(event.submitter&&event.submitter.id==='saveReviewNext')openNextReview();else toast('Human review saved.');}catch(e){$('dialogError').textContent=e.message;$('dialogError').classList.remove('hidden');}
});
$('editForm').addEventListener('submit',async event=>{
  event.preventDefault();try{await api('edit',{id:editing.id,stage:editing.stage,text:$('editText').value,note:$('editNote').value});$('editDialog').close();await refresh(true);if(editing.stage==='source')$('sourceText').value=state.project.segments.map(s=>(s.speaker?s.speaker+': ':'')+s.source).join('\n\n');toast(editing.human?'Human translation saved. Click Resume to continue with Copilot.':'Correction saved. Dependent stages require a fresh run.');}catch(e){$('editError').textContent=e.message;$('editError').classList.remove('hidden');}
});
window.addEventListener('beforeunload',event=>{if(state&&state.project.segments.length){event.preventDefault();event.returnValue='Export your project before closing the application.';}});
act(async()=>{await refresh(true);go('source');});
setInterval(async()=>{
  if(!state||pollInFlight||!(state.job.running||state.copilot?.connecting||pendingCopilotStart))return;
  pollInFlight=true;try{await refresh();}catch(e){showError(e.message);}finally{pollInFlight=false;}
},1000);
window.addEventListener('focus',()=>{if(state&&(state.copilot?.connecting||pendingCopilotStart)&&!pollInFlight)refresh().catch(e=>showError(e.message));});
