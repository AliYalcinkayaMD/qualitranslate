# Changelog

## 1.5.0

First release as packages: `qualitranslate` on PyPI (Python) and CRAN (R).

- Python package with a `qualitranslate` command (`python -m qualitranslate` also works). No third-party dependencies.
- R package with `run_app()`, built on the same prompts, interface and project format.
- Per-stage temperature or top-p for the Claude API (sent only when set), reasoning effort for Claude (`output_config.effort`) and Mistral (`reasoning_effort`), and a seed for Mistral.
- Optional different model for blind back-translation in the guided setup.
- Changing a later stage's settings keeps earlier stages (for example a new back-translation model keeps the forward translations).
- Draft methods paragraph, structured after the reporting items of Yunus et al. (2022), shown under Download and placed at the top of `methods_record.txt`.
- Local warning for possible identifiers (e-mail, phone-like and CPR-like numbers, web addresses, full dates) before sending. Text is never changed.
- Prompts 1.2.0: keep timestamps and transcription markers, keep fillers and hesitations, and a new reconciliation category `idiom_metaphor`.
- The Copilot window lock now uses POSIX record locks.
- The Copilot page diagnostic moved into the package: `python -m qualitranslate.diagnostic`.

## 1.4.0

- Copilot browser mode rebuilt and tested live with Microsoft 365 Copilot Chat: new chat and temporary chat per batch, verified sending, receipt-checked replies, refusal handling by batch splitting and single-section retry without context, human entry for declined sections.
