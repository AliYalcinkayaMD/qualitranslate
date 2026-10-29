# Changelog

## 1.5.1

First release on PyPI (Python) and CRAN (R). Documentation corrections; the app works as in 1.5.0.

## 1.5.0

First public release, as a Python package and an R package.

- ChatGPT, Claude chat, Microsoft Copilot and Mistral's Le Chat are listed for copy and paste.
- Python package with a `qualitranslate` command (`python -m qualitranslate` also works). No third-party dependencies.
- R package with `run_app()`, built on the same prompts, interface and project format.
- Per-stage temperature or top-p for the Claude API (sent only when set), reasoning effort for Claude (`output_config.effort`) and Mistral (`reasoning_effort`), and a seed for Mistral.
- Optional different model for blind back-translation in the guided setup.
- Changing a later stage's settings keeps earlier stages (for example a new back-translation model keeps the forward translations).
- Draft methods paragraph, structured after the reporting items of Yunus et al. (2022), shown under Download and placed at the top of `methods_record.txt`.
- Local warning for possible identifiers (e-mail, phone-like and CPR-like numbers, web addresses, full dates) before sending. Text is never changed.
- Prompts 1.2.0: keep timestamps and transcription markers, keep fillers and hesitations, and a new reconciliation category `idiom_metaphor`.
- The tool-choice step reminds users to follow their institution's rules for the data they translate, whichever tool they choose.
