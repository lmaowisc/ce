# Statistical Methods for Composite Endpoints

## Win Ratio and Beyond

This folder contains the book website sources. Edit the five chapter `.qmd` files; see [EDITING.md](EDITING.md). The original course notes and slides remain in the parent folder.

From the repository root, build with:

```powershell
python -m pip install -r website/requirements.txt
python website/build.py
python -m http.server 8768 --bind 127.0.0.1
```

Quarto must also be installed. Preview http://127.0.0.1:8768/docs/index.html. Commit the changed sources and generated `docs` files to publish through GitHub Pages.

`build.py` renders chapter Markdown and equations through Quarto Pandoc, generates the searchable bibliography from `website/references.bib`, and extracts the first worked R chunk from each original companion note. It does not execute R analyses. `style.css` is deployed as `docs/book.css` to preserve the original slide stylesheet. `app.js` provides the interactive comparison, navigation, reference search, and code copying.

Chapter narratives are an initial sample/overview and will be developed with the author. The illustrative comparison uses fictional complete histories: treatment hospitalization at 1.2 years and survival through year 4; control hospitalization at 2.5 years and death at 3.4 years.

Use `python website/build.py` for book updates. Do not render the entire legacy root Quarto project over these pages. Original slide presentations and source analyses are preserved.
