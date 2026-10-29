"""Review priorities and exact, team-supplied terminology checks."""
import re

SEVERITY_RANK={'none':0,'minor':1,'major':2,'critical':3}


def severity(segment):
    issues=(segment.get('reconcile') or {}).get('issues',[])
    return max((i['severity'] for i in issues),key=lambda x:SEVERITY_RANK[x],default='none')


def term_present(text,term):
    return bool(re.search(r'(?<!\w)'+re.escape(term)+r'(?!\w)',text,re.I))


def terminology_issues(source,target,terms):
    issues=[]
    for t in terms:
        if term_present(source,t['source']) and not any(term_present(target,x) for x in [t['target']]+t['alternatives']):
            issues.append({'category':'terminology','severity':'major' if t['required'] else 'minor','detail':'Terminology check: source contains "'+t['source']+'", but the preferred target "'+t['target']+'"'+(' or an approved alternative' if t['alternatives'] else '')+' was not found. Check context before changing the participant’s words.','needs_human':True,'origin':'local_glossary_check'})
    return issues
