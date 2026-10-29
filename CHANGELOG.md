# Changelog

## 1.5.0

First release as packages: `qualitranslate` on PyPI (Python) and CRAN (R).

- **The automatic Copilot browser mode of 1.4 is not included.** Microsoft's terms for its online services (Acceptable Use Policy, June 2026) forbid scraping or other data extraction from those services, and that mode read Copilot's replies from its web page. Copilot can still be used by copy and paste, like any other chat. Projects made with 1.4 still open; their Copilot stages become copy-and-paste stages, and their run records are kept.
- ChatGPT, Claude chat, Microsoft Copilot and Mistral's Le Chat are listed for copy and paste.
- Python package with a `qualitranslate` command (`python -m qualitranslate` also works). No third-party dependencies.
- R package with `run_app()`, built on the same prompts, interface and project format.
- Per-stage temperature or top-p for the Claude API (sent only when set), reasoning effort for Claude (`output_config.effort`) and Mistral (`reasoning_effort`), and a seed for Mistral.
- Optional different model for blind back-translation in the guided setup.
- Changing a later stage's settings keeps earlier stages (for example a new back-translation model keeps the forward translations).
- Draft methods paragraph, structured after the reporting items of Yunus et al. (2022), shown under Download and placed at the top of `methods_record.txt`.
- Local warning for possible identifiers (e-mail, phone-like and CPR-like numbers, web addresses, full dates) before sending. Text is never changed.
- Prompts 1.2.0: keep timestamps and transcription markers, keep fillers and hesitations, and a new reconciliation category `idiom_metaphor`.
- The tool-choice step reminds users to follow their institution's rules for the data they translate, whichever tool they choose; interface wording is no longer specific to one university.

## 1.4.0

- Copilot browser mode rebuilt and tested live with Microsoft 365 Copilot Chat: new chat and temporary chat per batch, verified sending, receipt-checked replies, refusal handling by batch splitting and single-section retry without context, human entry for declined sections.
