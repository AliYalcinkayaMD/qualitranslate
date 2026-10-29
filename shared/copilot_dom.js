/* Visible Copilot UI adapter (QualiTranslate 1.4+; shared by the Python and R packages).
   Reads only the visible chat page structure. No service API, cookies, storage,
   network traffic, or stored credentials are read.
   Selectors were taken from the signed-in Microsoft 365 Copilot Chat page observed
   on 27 Oct 2026 (copilot.cloud.microsoft/chat), with generic fallbacks. */
(()=>{
  const ZW=/[​-‏⁠﻿]/g;
  const clean=s=>String(s||'').replace(ZW,'');
  // Reply and composer text keep every character: ZWNJ/ZWJ and direction marks are
  // meaningful in Persian, Arabic, Hebrew and emoji. clean() is for labels and notices only.
  const exact=s=>String(s||'');
  const visible=e=>{try{return !!(e&&e.getClientRects().length)&&getComputedStyle(e).visibility!=='hidden';}catch{return false;}};
  const roots=[];
  (function discover(root){roots.push(root);for(const el of root.querySelectorAll('*'))if(el.shadowRoot)discover(el.shadowRoot);})(document);
  const all=selector=>roots.flatMap(r=>[...r.querySelectorAll(selector)]);
  const shown=selector=>all(selector).filter(visible);
  const name=e=>clean(e.getAttribute('aria-label')||e.getAttribute('title')||e.innerText||'').replace(/\s+/g,' ').trim();
  const MESSAGE='[data-testid="m365-chat-llm-web-ui-chat-message"],[data-testid="chatQuestion"],[data-testid="copilot-message-div"],[data-message-author-role],cib-message-group,cib-message,.fai-UserMessage,.fai-CopilotMessage';
  const inMessage=e=>!!e.closest(MESSAGE);
  const inCode=e=>!!e.closest('.scriptor-component-code-block,pre,code,[aria-label="Code editor"]');

  // Composer: the known Copilot editor first, then accessible generic fallbacks.
  let editors=shown('#m365-chat-editor-target-element');
  if(!editors.length){
    editors=shown('[role="textbox"][contenteditable="true"],[contenteditable="true"],[contenteditable="plaintext-only"],textarea')
      .filter(e=>!e.disabled&&!e.readOnly&&e.getAttribute('aria-readonly')!=='true'&&!inMessage(e)&&!inCode(e)
        &&!/search|søg|rechercher|suchen|buscar|ara/i.test((e.getAttribute('placeholder')||'')+' '+(e.getAttribute('aria-label')||'')));
    const preferred=editors.filter(e=>/copilot|message|prompt|ask|spørg|besked|mesaj|nachricht/i.test((e.getAttribute('placeholder')||'')+' '+(e.getAttribute('aria-label')||'')+' '+e.id));
    if(preferred.length)editors=preferred;
  }
  const editor=editors.length===1?editors[0]:null;
  const composerText=()=>editor?exact(editor.value??editor.innerText??''):'';

  const controls=shown('button,[role="button"]').filter(e=>!inMessage(e));
  const sendRe=/^(?:send(?: message| prompt)?|submit|send besked|gönder|envoyer|enviar|senden|보내기)$/i;
  const stopRe=/^(?:stop(?: (?:generating|responding|response|generation))?|stop generering|stands generering|durdur|arrêter|detener|anhalten|중지)$/i;
  const newRe=/^(?:new (?:chat|conversation)|start (?:a )?new chat|ny (?:chat|samtale)|yeni (?:sohbet|konuşma)|nouvelle (?:discussion|conversation)|nueva (?:conversación|charla)|neuer chat|neue unterhaltung|nova conversa)$/i;
  let sendControls=controls.filter(e=>e.getAttribute('type')==='submit'&&sendRe.test(name(e)));
  if(!sendControls.length)sendControls=controls.filter(e=>sendRe.test(name(e)));
  const sendButton=sendControls.length===1?sendControls[0]:null;
  const stopControls=controls.filter(e=>stopRe.test(name(e)));
  const newControls=controls.filter(e=>newRe.test(name(e))).concat(shown('a[href]').filter(e=>{
    if(!newRe.test(name(e)))return false;try{const u=new URL(e.href,location.href);return u.origin===location.origin&&/^\/chat\/?$/.test(u.pathname);}catch{return false;}}));
  const temporary=controls.filter(e=>/^temporary chat$/i.test(name(e)));
  const temporaryButton=temporary.length===1?temporary[0]:null;
  const modelButton=controls.find(e=>e.id==='gptModeSwitcher'||/model selector/i.test(e.getAttribute('aria-label')||''))||null;

  // Conversation turns and assistant replies.
  const turnNodes=all('[data-testid="m365-chat-llm-web-ui-chat-message"]');
  const userNodes=all('[data-testid="chatQuestion"],.fai-UserMessage,[data-message-author-role="user"],[data-author="user"]');
  const replyNodes=all('[data-testid="copilot-message-reply-div"],[data-testid="chat-response-message"],[data-message-author-role="assistant"],[data-author="assistant"],cib-message[source="bot"]')
    .filter(e=>!e.parentElement||!e.parentElement.closest('[data-testid="copilot-message-reply-div"],[data-testid="chat-response-message"]'));
  const lastReply=replyNodes[replyNodes.length-1]||null;
  const lastTurn=lastReply&&(lastReply.closest('[data-testid="m365-chat-llm-web-ui-chat-message"]')||lastReply.parentElement);
  const replyDone=!!(lastTurn&&[...lastTurn.querySelectorAll('button,[role="button"]')].some(b=>/^copy response$/i.test(name(b))));
  const loading=all('[data-testid="loading-message"]').length;

  const headingText=shown('h1,h2,[role="heading"]').map(e=>clean(e.innerText).trim()).filter(Boolean);
  const text=clean((document.querySelector('main')||document.body).innerText||'');
  const auth=!!(editor&&/^unauth/i.test(editor.id))||controls.some(e=>/^(sign in|log in|log på|oturum aç|anmelden)$/i.test(name(e)));
  const warnings=clean(shown('[role="alert"],[role="alertdialog"],[role="dialog"]').map(e=>e.innerText||'').join('\n'))+(editor?'':'\n'+text.slice(0,4000));
  // A short non-JSON reply can carry a service notice; a translation reply (which may quote
  // a participant saying "limit") is never scanned for notices.
  const lastText=lastReply?clean(lastReply.innerText):'';
  const replyWarning=lastText.length<600&&!/"segments"|qt_receipt/.test(lastText)?lastText:'';
  const challenge=/verify (?:that )?you(?: are|'re) human|unusual traffic|automated (?:traffic|queries)|captcha|bekræft.*menneske|insan olduğunuzu/i.test(warnings);
  const limit=/too many requests|you(?:'ve| have) reached (?:your|the) (?:daily |message )?limit|usage limit|for mange anmodninger|message is too long|exceeds the (?:maximum|character) limit/i.test(warnings+'\n'+replyWarning);
  const token=globalThis.__qtReceipt||'';
  const metadata={
    path:location.pathname.slice(0,80),page_load_id:String(performance.timeOrigin),page_age_ms:Math.round(performance.now()),ready_state:document.readyState,
    has_editor:!!editor,editor_count:editors.length,composer_empty:editor?!clean(composerText()).trim():false,composer_length:composerText().length,
    has_send:!!sendButton,send_count:sendControls.length,send_enabled:!!sendButton&&!sendButton.disabled&&sendButton.getAttribute('aria-disabled')!=='true',
    stopping:stopControls.length>0,has_new_chat:newControls.length>0,
    has_temporary:!!temporaryButton,temporary_on:!!temporaryButton&&temporaryButton.getAttribute('aria-pressed')==='true',
    temporary_heading:headingText.some(h=>/^temporary chat$/i.test(h)),
    model_label:modelButton?name(modelButton).replace(/,?\s*model selector$/i,'').slice(0,60):'',
    turn_count:Math.max(turnNodes.length,userNodes.length,replyNodes.length),user_turns:userNodes.length,reply_count:replyNodes.length,
    reply_done:replyDone,loading:loading,heading:headingText.slice(0,2).join(' | ').slice(0,120),
    authentication_required:auth,challenge:challenge,limit:limit,previous_receipt_present:!!(token&&text.includes(token)),
    title:clean(document.title).slice(0,80)
  };
  const replyTexts=()=>replyNodes.map(e=>exact(e.innerText||'')).filter(Boolean).map(t=>t.slice(0,1500000));
  globalThis.__qtUI={editor,sendButton,stopButton:stopControls[0]||null,temporaryButton,replyTexts,composerText,
    lastReplyText:()=>lastReply?exact(lastReply.innerText||'').slice(0,1500000):'',
    temporaryNotice:()=>{const h=shown('h1,h2,[role="heading"]').find(x=>/^temporary chat$/i.test(clean(x.innerText).trim()));
      // Climb from the heading to the smallest block that also contains Copilot's explanatory notice.
      let p=h;for(let i=0;i<5&&p&&p.parentElement;i++){p=p.parentElement;const t=clean(p.innerText).replace(/\s+/g,' ').trim();if(/sav|memor|retention|admin/i.test(t)&&t.length<600)return t.slice(0,400);}
      return '';}};
  return metadata;
})()
