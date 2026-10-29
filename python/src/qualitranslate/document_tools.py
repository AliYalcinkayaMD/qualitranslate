"""Local document reading and conservative interview-boundary suggestions. No model calls."""
import base64
import io
import re
import zipfile
from xml.etree import ElementTree as ET

SCAN_VERSION = '1.0.0'
MAX_TEXT = 1_500_000
META = re.compile(r'^(?:interview\s*(?:date|id|number|location|duration)|date|project|study\s*(?:id|title)|file\s*name|transcriber|location|country|site|recording\s*(?:id|date)|duration|page|participant\s*(?:id|code|details)|demographic\s*(?:data|details))\s*:', re.I)
SECTION = re.compile(r'^(?:appendix|references|demographics|participant characteristics|coding\s*(?:table|notes|framework)|codebook|researcher notes|interview guide|instructions for|administrative|consent form|end of (?:the )?(?:interview|transcript))\b', re.I)
SPEAKER = re.compile(r'^\s*([^:\n]{1,60}):\s*\S')
KNOWN_SPEAKER = re.compile(r'^(?:interviewer|participant|respondent|moderator|patient|speaker|parent|caregiver|interviewee|p\d+|r\d+|i\d*|f\d+)(?:[ _-]*[\w-]{1,20})?$', re.I)

class DocumentError(ValueError):
    pass


def _text(value):
    if not isinstance(value,str) or not value.strip() or len(value)>MAX_TEXT or '\x00' in value:
        raise DocumentError('Document text is empty, invalid, or too large.')
    return value.replace('\r\n','\n').replace('\r','\n').lstrip('\ufeff')


def read_document(filename, encoded):
    if not isinstance(filename,str) or len(filename)>240:
        raise DocumentError('Invalid filename.')
    try: raw=base64.b64decode(encoded,validate=True)
    except Exception as e: raise DocumentError('Invalid upload encoding.') from e
    if len(raw)>8_000_000: raise DocumentError('File exceeds the 8 MB limit.')
    if filename.lower().endswith(('.txt','.md')):
        try: text=raw.decode('utf-8-sig')
        except UnicodeDecodeError as e: raise DocumentError('Text files must use UTF-8 encoding.') from e
        return {'text':_text(text),'blocks':[]}
    if not filename.lower().endswith('.docx'):
        raise DocumentError('Supported transcript files: .txt, .md, .docx')
    try:
        with zipfile.ZipFile(io.BytesIO(raw)) as z:
            if len(z.infolist())>2000: raise DocumentError('The document has too many archive entries.')
            entry=z.getinfo('word/document.xml')
            if entry.file_size>5_000_000: raise DocumentError('Document text is too large.')
            xml=z.read(entry)
        if b'<!DOCTYPE' in xml.upper() or b'<!ENTITY' in xml.upper():
            raise DocumentError('XML entity declarations are not supported.')
        root=ET.fromstring(xml); ns='{http://schemas.openxmlformats.org/wordprocessingml/2006/main}'
        body=root.find(ns+'body')
        if body is None: raise DocumentError('The Word document has no main body.')
        def para(p):
            return ''.join((n.text or '') if n.tag==ns+'t' else '\t' if n.tag==ns+'tab' else '\n' if n.tag in {ns+'br',ns+'cr'} else '' for n in p.iter())
        blocks=[]
        for element in body:
            if element.tag==ns+'p':
                content=para(element).strip()
                if content: blocks.append({'text':content,'kind':'paragraph'})
            elif element.tag==ns+'tbl':
                rows=[];dialogue=True
                for row in element.findall(ns+'tr'):
                    cells=[' '.join(para(p) for p in cell.findall(ns+'p')).strip() for cell in row.findall(ns+'tc')]
                    if len(cells)==2 and KNOWN_SPEAKER.fullmatch(cells[0].rstrip(':')):
                        rows.append(cells[0].rstrip(':')+': '+cells[1])
                    else:
                        dialogue=False;rows.append('\t'.join(cells))
                content='\n'.join(rows).strip()
                if content: blocks.append({'text':content,'kind':'dialogue_table' if dialogue else 'table'})
        return {'text':_text('\n\n'.join(b['text'] for b in blocks)),'blocks':blocks}
    except DocumentError: raise
    except Exception as e: raise DocumentError('Cannot read this DOCX. Save a checked copy as UTF-8 TXT.') from e


def scan_document(text, structured_blocks=None):
    text=_text(text)
    structured=isinstance(structured_blocks,list) and len(structured_blocks)<=10000 and all(isinstance(b,dict) and isinstance(b.get('text'),str) and b.get('kind') in {'paragraph','table','dialogue_table'} for b in structured_blocks)
    if structured and '\n\n'.join(b['text'] for b in structured_blocks)!=text: structured=False
    raw=[]
    for block in structured_blocks if structured else [{'text':text,'kind':'paragraph'}]:
        if block['kind']=='table': raw.append(dict(block));continue
        collected=[]
        def flush():
            if collected:
                raw.append({'text':'\n'.join(collected).strip(),'kind':'paragraph'});collected.clear()
        for line in block['text'].split('\n'):
            if not line.strip(): flush()
            elif META.match(line) or SECTION.match(line) or re.fullmatch(r'(?:page\s*)?\d+\s*(?:of|/)\s*\d+',line,re.I):
                flush();raw.append({'text':line.strip(),'kind':'paragraph'})
            elif not SPEAKER.match(line) and ('\t' in line or line.count('|')>=2):
                flush()
                if raw and raw[-1]['kind']=='table': raw[-1]['text']+='\n'+line
                else: raw.append({'text':line,'kind':'table'})
            elif SPEAKER.match(line): flush();collected.append(line)
            else: collected.append(line)
        flush()
    if not raw or len(raw)>10000: raise DocumentError('Document must contain 1–10,000 blocks.')
    spoken=[i for i,b in enumerate(raw) if SPEAKER.match(b['text']) and not META.match(b['text']) and not SECTION.match(b['text'])]
    result=[]
    trailing_section = None
    for i,b in enumerate(raw):
        content=b['text'];reason='';exclude=False
        if b['kind']=='table' or ('\t' in content and not SPEAKER.match(content)) or (content.count('|')>=4 and re.search(r'\|\s*[-:]{2,}',content)):
            reason='Possible table or administrative data. Check whether this belongs to the interview.';exclude=True
        elif META.match(content): reason='Possible document metadata rather than spoken interview content.';exclude=True
        elif SECTION.match(content): reason='Possible heading, appendix, coding notes, or end marker.';exclude=True
        elif re.fullmatch(r'(?:page\s*)?\d+\s*(?:of|/)\s*\d+',content,re.I): reason='Possible page numbering.';exclude=True
        elif spoken and i<spoken[0] and not SPEAKER.match(content):
            reason='Text before the first speaker turn. Confirm whether this is interview content.'
        elif trailing_section is not None and spoken and i>spoken[-1] and not SPEAKER.match(content):
            reason='Text after a closing or appendix heading. Confirm whether it is part of the interview.'
        if SECTION.match(content) and spoken and i>spoken[-1]: trailing_section=i
        result.append({'id':'B%04d'%(i+1),'text':content,'kind':b['kind'],'reason':reason,'suggested_exclude':exclude,'needs_check':bool(reason)})
    return {'scan_version':SCAN_VERSION,'blocks':result,'suspected':sum(b['needs_check'] for b in result),'suggested_exclusions':sum(b['suggested_exclude'] for b in result)}
