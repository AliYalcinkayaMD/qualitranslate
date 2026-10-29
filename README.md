# QualiTranslate

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23784148.svg)](https://doi.org/10.5281/zenodo.23784148)

QualiTranslate supports translation of qualitative interview transcripts with large language models, keeping the steps that cross-language qualitative research needs:

**forward translation → blind back-translation → reconciliation → recorded human review**

It is published as two packages with the same features, prompts, interface and project file format:

| | Install | Start |
|---|---|---|
| **Python** (PyPI) | `python3 -m pip install qualitranslate` | `qualitranslate` |
| **R** (CRAN) | `install.packages("qualitranslate")` | `qualitranslate::run_app()` |

Both open a local workspace in your browser. A project saved in one can be continued in the other.

## Features

- Reads `.docx`, `.txt` and `.md` transcripts, splits them into speaker turns or paragraphs, and helps exclude headers, notes and tables before anything is sent.
- Warns about possible identifiers (e-mail addresses, phone-like and CPR-like numbers, web addresses, full dates) without changing the text.
- Tools per stage: the **Claude API** or the **Mistral API** (automatic, with your own key), or **any AI chat** such as ChatGPT, Claude, Microsoft Copilot or Le Chat by copy and paste.
- Per-stage model, temperature or top-p, reasoning effort and seed where the provider supports them, including a different model for blind back-translation.
- The blind back-translation request never contains the original text, study context or glossary.
- Reconciliation flags discrepancies by category (negation, omission, addition, temporality, intensity, uncertainty, agency, terminology, cultural meaning, idioms and metaphors, source ambiguity) and severity, with an optional team lexicon checked locally.
- Human review with a named reviewer for every section; quick approval and batch approval of minor-only sections are recorded as such.
- One download with six files: translation table, side-by-side report, project file, every prompt and reply, audit trail, and a methods record that begins with a draft methods paragraph.

## Repository layout

```
shared/     prompts.json and the web interface (single source)
python/     the PyPI package (standard library only)
r/          the CRAN package
tools/      sync_shared.py (copies shared/ into both packages), api_conformance.py (checks both servers behave the same)
examples/   fictional transcripts for trying the tool
```

Edit files in `shared/`, then run `python3 tools/sync_shared.py`.

## Data protection and limits

Text is sent only to the service the user chooses. Use a service approved for the data in your study, and pseudonymise transcripts first when your approval requires it. API keys stay in memory and are never exported. Back-translation is a diagnostic aid; separate requests to one model are not independent translators. The methods record states what was done without claiming validation.

## Citation

Yalcinkaya A. QualiTranslate [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.23784148

This DOI covers all versions and always points to the latest one. Each release also has its own DOI on Zenodo; cite the version you used, and report the tools, models and review steps (the methods record in each download lists them). See also `CITATION.cff`.

## Licence

MIT
