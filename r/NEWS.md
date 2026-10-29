# qualitranslate 1.5.0

* First R release of QualiTranslate, matching the Python version 1.5.0: the same
  browser interface, JSON API, project format (projects move freely between the two),
  six-file export, prompts and methods record.
* Forward translation, blind back-translation and reconciliation by copy and paste,
  through the Mistral or Anthropic (Claude) API, or through Microsoft 365 Copilot Chat
  in a dedicated browser window.
* Per-stage model settings: temperature or top-p, a Mistral seed, and reasoning effort
  (Claude `output_config.effort`, Mistral `reasoning_effort`). Changing only a later
  stage's settings keeps the earlier stages' results.
* The Copilot panel reminds users to check that their institution allows their data in
  Copilot before using it.
* Local identifier check (warning only) and a draft methods paragraph derived from the
  results currently in the project.
* Copilot mode isolates a declined section by splitting the batch and retrying a single
  section once without neighbouring context; text is never rephrased. Sections Copilot
  declines can be given a human translation that is recorded as human work.
