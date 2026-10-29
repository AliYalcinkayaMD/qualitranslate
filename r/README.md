# qualitranslate (R)

QualiTranslate is a local workspace for translating qualitative interview transcripts
for research. Each batch of transcript sections goes through:

1. **Forward translation** into the target language.
2. **Blind back-translation**: the request holds only the forward translation, never the
   original, the study context, the glossary or translator notes.
3. **Reconciliation**: the original, forward and back-translations are compared,
   discrepancies are flagged by category and severity, and a draft is proposed.
4. **Named human review** of every draft, before anything counts as reviewed.

The results export as a six-file research bundle (reviewed translations as CSV, a parallel
review table, the project file, the exact prompts and replies, a hash-chained audit record,
and a methods record with a draft methods paragraph). Projects are interchangeable with the
Python version of QualiTranslate.

## Install and start

```r
install.packages("qualitranslate")
qualitranslate::run_app()
```

Before the CRAN release, install from a downloaded source file instead:
`install.packages("qualitranslate_1.5.0.tar.gz", repos = NULL, type = "source")`.

`run_app()` starts the app on `http://127.0.0.1:8765` (or the next free port), opens it in
your web browser and keeps running until you press Escape (RStudio) or Control+C. Your
project stays in memory only: export it from the app before you stop.
`show_prompts()` prints the translation instructions the app uses.

## Ways to run each stage

- **Manual**: copy a prompt packet into any chat tool (for example ChatGPT, Claude,
  Microsoft Copilot or Mistral's Le Chat, including your institution's chat service) and
  paste the reply back.
- **API**: the Mistral or Anthropic (Claude) API with your own key. Keys stay in memory
  and are never exported.


## Data protection

Transcript text leaves your computer only when you run a stage with a provider you chose,
and then only to that provider: nothing in manual mode (you paste it into the chat tool
yourself), and the selected API in API mode. Back-translation requests never contain
the original. The app warns about common identifiers (e-mail addresses, phone-like and
CPR-like numbers, dates, web addresses) but does not remove them and cannot detect names
or places, so pseudonymise transcripts before translating them.

The app serves only the local address `127.0.0.1`, checks the Host and Origin of every
request and requires a per-session token.
