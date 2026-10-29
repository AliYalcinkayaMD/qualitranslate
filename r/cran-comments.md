## Submission

This is a new submission.

## Test environments

* Local: Linux (x86_64), R 4.3.3, `R CMD check --as-cran`.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewers

* `run_app()` starts a local web server on 127.0.0.1 and blocks until interrupted, like
  `shiny::runApp()`; its example is wrapped in `if (interactive())`.
* Nothing is written outside `tempdir()`, during checks or in use: the project stays in
  memory until the user exports it from the interface.
* Network access happens only when the user runs a translation stage with an API provider
  ('Mistral' or 'Anthropic'); the tests use local fixtures and a local test server only.
