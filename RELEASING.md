# Releasing QualiTranslate

One GitHub release publishes the Python package to PyPI automatically and archives the release on Zenodo with a DOI. The R package is submitted to CRAN separately, by hand.

The one-time setup (steps 1–4) takes about an hour. Later releases take a few minutes plus CRAN's review.

## 1. Put the repository on GitHub

1. Sign in at [github.com](https://github.com) and create a new **public** repository named `qualitranslate`. Do not add a README, licence or .gitignore (they are already in this folder).
2. Upload this folder. The easiest way is [GitHub Desktop](https://desktop.github.com): *File → Add local repository* → choose this folder → *create a repository* if asked → *Publish repository* (uncheck "Keep this code private").
   With a terminal instead:
   ```bash
   cd qualitranslate
   git init -b main
   git add .
   git commit -m "QualiTranslate 1.5.0"
   git remote add origin https://github.com/<your-user>/qualitranslate.git
   git push -u origin main
   ```
3. The repository address (github.com/AliYalcinkayaMD/qualitranslate) is already filled in `python/pyproject.toml`, `r/DESCRIPTION` and `CITATION.cff`. If you have an ORCID, add `orcid: "https://orcid.org/0000-..."` under your name in `CITATION.cff` and `"orcid": "0000-..."` in `.zenodo.json`. Add co-authors to both files and to `r/DESCRIPTION` in the same way.
4. Open the *Actions* tab. The *Python package* and *R package* checks should turn green.

## 2. Connect Zenodo (before the first release)

1. Sign in at [zenodo.org](https://zenodo.org) with your GitHub account.
2. Open *your name → GitHub*, find `qualitranslate` and switch it **On**.

Zenodo then archives every GitHub release and gives it a DOI. It reads the title, authors and licence from `.zenodo.json`.

## 3. Set up PyPI publishing (no password or token stored in GitHub)

1. Create an account at [pypi.org](https://pypi.org) and switch on two-factor authentication.
2. Go to *Your account → Publishing → Add a new pending publisher* (GitHub tab) and enter:
   - PyPI project name: `qualitranslate`
   - Owner: your GitHub user name (or organisation)
   - Repository name: `qualitranslate`
   - Workflow name: `publish-pypi.yml`
   - Environment name: `pypi`
3. On GitHub: repository *Settings → Environments → New environment*, name it `pypi`. Optionally add yourself as a required reviewer, so each upload waits for your click.

## 4. Make the release

1. On GitHub: *Releases → Draft a new release*.
2. *Choose a tag*: type `v1.5.0` and create it. Title: `QualiTranslate 1.5.0`. Paste the 1.5.0 section of `CHANGELOG.md` as the description.
3. Click **Publish release**.
4. In *Actions*, the *Publish to PyPI* run uploads the package (approve it if you added a reviewer). After it finishes, `python3 -m pip install qualitranslate` works for everyone.
5. A few minutes later Zenodo shows the archived release with its DOI. Add the DOI to `CITATION.cff` (`doi: 10.5281/zenodo.…`) and, if you like, the DOI badge from Zenodo to `README.md`. Commit and push.

## 5. Submit the R package to CRAN

CRAN checks every package by hand. Do this after the GitHub release, using the same version number.

1. Check locally in R (from the repository folder):
   ```r
   install.packages(c("devtools", "rcmdcheck"))
   rcmdcheck::rcmdcheck("r", args = "--as-cran", error_on = "warning")
   ```
   It must end with `0 errors | 0 warnings`. A NOTE saying "New submission" is normal.
2. Check on CRAN's other platforms (results arrive by e-mail within an hour):
   ```r
   devtools::check_win_devel("r")
   devtools::check_mac_release("r")
   ```
3. Build the file to upload: `devtools::build("r")` creates `qualitranslate_1.5.0.tar.gz`.
4. Submit at [cran.r-project.org/submit.html](https://cran.r-project.org/submit.html): upload the `.tar.gz`, paste the contents of `r/cran-comments.md` as the comment, and confirm the link CRAN sends by e-mail.
5. CRAN usually replies within days. First submissions often get small requests (for example wording in the Description, `\value` sections in help pages, or examples). Make the changes, increase nothing but what they ask, and resubmit with a short reply. If the version changes (for example to 1.5.1), change it everywhere listed below and make a matching GitHub release.

## Later releases

1. Change the version number in: `python/pyproject.toml`, `python/src/qualitranslate/core.py` (`APP_VERSION`), `r/DESCRIPTION`, `CITATION.cff`, `shared/web/index.html` (footer), and add a section to `CHANGELOG.md` and `r/NEWS.md`.
2. If you changed anything in `shared/`, run `python3 tools/sync_shared.py`.
3. Run the tests (`cd python/tests && PYTHONPATH=../src python3 -m unittest discover -s .`, and the R check above), push, wait for green checks, then make the GitHub release and submit to CRAN as above.
