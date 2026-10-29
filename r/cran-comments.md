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
* Nothing is written outside `tempdir()` during checks. The optional Copilot browser
  mode keeps a dedicated browser profile in `tools::R_user_dir("qualitranslate", "data")`,
  created only when the user clicks Connect in that mode, and removed by the app's
  "Forget saved Copilot sign-in" action.
* Network access happens only when the user runs a translation stage with an API provider
  or connects the Copilot mode; the tests use local fixtures only. Tests that drive a real
  headless Chromium are skipped on CRAN and unless `QT_TEST_CHROME` names a browser.
