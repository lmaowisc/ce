# Editing the chapter text

Open the appropriate file in RStudio, VS Code, or any text editor:

| Chapter | File |
| --- | --- |
| 1. Introduction | `intro.qmd` |
| 2. Hypothesis Testing | `testing.qmd` |
| 3. Nonparametric Estimation | `estimation.qmd` |
| 4. Semiparametric Regression | `regression.qmd` |
| 5. Discussions | `discussions.qmd` |

These files are inside **website/**. The original companion notes with similar names in the parent folder remain available as source material.

## What to edit

The short block between `---` lines at the top contains the chapter title, introductory description, topic list, and draft status. The chapter text follows it.

Write normal paragraphs separated by a blank line. Use `##` for sections, `###` for subsections, `**bold**` for emphasis, and `$...$` or `$$...$$` for equations. Links and figures use ordinary Markdown. Keep existing `{#section-1}` identifiers when renaming a section so its links continue to work; new sections can simply use `## Your Heading`.

Optional highlighted passages use `::: callout` before the passage and `:::` after it. You can also write plain paragraphs without a box.

Section numbers, chapter navigation, slide links, code download links, and the contents list are generated automatically. You do not need to edit HTML, Python, or CSS to change a chapter's narrative.

## Refresh the website

After saving the `.qmd` file, run this in a terminal from the **website** folder:

```powershell
python build.py
```

Then refresh the open website. From the parent course folder, the equivalent command is `python website/build.py`.

The website uses Quarto’s Pandoc to render the Markdown and equations into the shared design. It does not execute R chunks. The original R scripts remain downloadable, and the source analyses remain in the parent folder. We can add executable chapter examples when developing the book content.

## Current scope

Keep the slide sources (`chap1.qmd`–`chap5.qmd` in the parent folder) and existing slide presentations unchanged. The five editable files here contain only the current chapter narratives; they are not a replacement for the original chapter summaries and analyses. Use both those summaries and the slides when developing the chapters.

Settle the layout first. Then develop Chapters 1 and 2 together with Lu Mao before expanding the remaining chapters.

The generated pages are written to `../docs/`, the GitHub Pages publishing folder. Update `website/references.bib` for the book bibliography. The root bibliography and companion notes remain original course source material.

Use this build command for the book site; a full render of the legacy root Quarto project would overwrite the book pages. Slide updates will be handled separately.
