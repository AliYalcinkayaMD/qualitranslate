# QualiTranslate (Python)

QualiTranslate helps research teams translate qualitative interview transcripts with large language models while keeping the steps that make translation in cross-language research defensible:

**forward translation → blind back-translation → reconciliation → recorded human review**

It runs on your own computer as a small local web page. Nothing is saved to disk unless you download it, and nothing is sent anywhere until you choose a translation tool.

The same tool is available for R as the `qualitranslate` R package. Both versions share the prompts, the interface and the project file format, so a project started in one can be continued in the other.

## Install and start

You need Python 3.9 or newer. On a Mac, use the installer from [python.org](https://www.python.org/downloads/macos/) if Python is not installed yet.

```bash
python3 -m pip install qualitranslate
qualitranslate
```

Your browser opens the workspace at `http://127.0.0.1:8765` (or the next free address shown in the Terminal). Keep the Terminal window open while you work. Press Control+C there to stop.

If the `qualitranslate` command is not found, start it with `python3 -m qualitranslate`.

QualiTranslate uses only the Python standard library.

## What it does

1. **Add a transcript.** Paste text or open a `.docx`, `.txt` or `.md` file. The app splits it into sections by speaker turn or paragraph and points out headers, notes and tables, so you can confirm what enters translation. A quick local check warns about possible identifiers (e-mail addresses, phone-like and CPR-like numbers, web addresses, full dates). It never changes your text, and it cannot find names or places.
2. **Choose a tool.** Each stage can use a different tool:
   - **Claude API** or **Mistral API**: automatic, with your own API key.
   - **Any AI chat** (for example ChatGPT, Claude, Microsoft Copilot or Mistral's Le Chat), by copying a prepared prompt packet and pasting the reply back.
3. **Translate.** The app runs forward translation, blind back-translation and reconciliation as separate requests. The back-translation request contains only the forward translation: never the original, the study context or the glossary.
4. **Review.** Reconciliation flags possible problems by category (for example negation, omission, temporality, idioms and metaphors) and severity. A named reviewer records a decision for each section.
5. **Download.** One ZIP holds the translation table (CSV), a side-by-side report (HTML), the project file, every prompt and reply, the audit trail and a methods record that starts with a **draft methods paragraph** for your manuscript.

Try **Try the demo** first. It runs all three stages on fictional data without calling any AI service.

## Model settings

Under **Advanced settings** each stage has its own provider and model, plus:

- **Temperature or top-p**, sent only when you set them. Some current Claude models accept only their default temperature; if a request is rejected, set it back to *Provider default*.
- **Reasoning effort**: for Claude models that support it (`low` to `max`) and Mistral models with adjustable reasoning (`none` or `high`). It mostly matters for reconciliation, where finding subtle discrepancies benefits from more reasoning.
- **Random seed** (Mistral only). A seed does not guarantee identical translations.
- A **different model for blind back-translation** can catch errors that the first model would repeat. In the guided setup choose it under *Blind back-translation model*.

In copy-and-paste mode these settings are not sent anywhere; set them in the chat itself if it offers them, and record what you used.

Changing a later stage's settings keeps the earlier stages: for example, a new back-translation model clears only the back-translations and reconciliations.

## Data protection

- Text is sent only to the service you choose, in batches. Use a tool and account that your institution has approved for the data in your study, and pseudonymise transcripts first when your approval requires it.
- API keys are kept only in memory while the app runs. They are never written to disk or included in exports.
- Projects stay in memory until you download them.

## What it does not claim

Back-translation is a diagnostic aid. Separate requests to the same model are not independent human translators, and a new or temporary chat does not prove that a service has no other context. The methods record states what was actually done, including the tools, models and settings used, and does not claim validation or equivalence.

## Citation

Yalcinkaya A. QualiTranslate [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.23784148

## Licence

MIT. See `LICENSE`.
