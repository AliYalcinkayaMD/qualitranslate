# qualitranslate 1.5.0

* First R release of QualiTranslate, matching the Python version 1.5.0: the same
  browser interface, JSON API, project format (projects move freely between the two),
  six-file export, prompts and methods record.
* Forward translation, blind back-translation and reconciliation by copy and paste into
  any chat tool (for example ChatGPT, Claude, Microsoft Copilot or Mistral's Le Chat), or
  through the Mistral or Anthropic (Claude) API.
* Per-stage model settings: temperature or top-p, a Mistral seed, and reasoning effort
  (Claude `output_config.effort`, Mistral `reasoning_effort`). Changing only a later
  stage's settings keeps the earlier stages' results.
* The tool-choice step reminds users to follow their institution's rules for the data they
  translate, whichever tool they choose.
* Local identifier check (warning only) and a draft methods paragraph derived from the
  results currently in the project.
* A section that a chat tool declines to translate can be given a human translation,
  which is recorded as human work.
