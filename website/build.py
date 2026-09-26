"""Build the book website while preserving existing course slide assets.

Run from any directory: python website/build.py
Chapter narratives are editable Quarto files in this folder.
"""
from pathlib import Path
import html
import hashlib
import re
import subprocess
import shutil
import yaml
from bs4 import BeautifulSoup

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
OUTPUT = ROOT / 'docs'
OUTPUT.mkdir(exist_ok=True)
shutil.copyfile(HERE / 'style.css', OUTPUT / 'book.css')
shutil.copyfile(HERE / 'app.js', OUTPUT / 'app.js')
shutil.copyfile(HERE / 'references.bib', OUTPUT / 'references.bib')
STYLE_VERSION = hashlib.sha256((HERE / 'style.css').read_bytes()).hexdigest()[:12]
TITLE = 'Statistical Methods for Composite Endpoints'
CHAPTER_ORDER = ['intro', 'testing', 'estimation', 'regression', 'discussions']
chapter_metadata = {}
chapters = []
for slug in CHAPTER_ORDER:
    source = (HERE / f'{slug}.qmd').read_text(encoding='utf-8')
    metadata = yaml.safe_load(source.split('---', 2)[1])
    chapter_metadata[slug] = metadata
    chapters.append((metadata['title'], slug, metadata['description'], metadata['topics'], []))


def external(url, label):
    return f'<a href="{html.escape(url, quote=True)}" target="_blank" rel="noopener noreferrer">{label}</a>'

def nav(active):
    def link(key, href, label, num=''):
        current = ' aria-current="page"' if key == active else ''
        cls = (' nav-home' if key == 'index' else '') + (' active' if key == active else '')
        return f'<a class="nav-link{cls}" href="{href}"{current}>{f"<span class=num>{num}</span>" if num else ""}<span class="nav-text">{label}</span></a>'
    links = link('index', 'index.html', 'About This Book')
    links += '<p class="nav-label">CONTENTS</p>'
    for i, (title, slug, *_rest) in enumerate(chapters, 1):
        links += link(slug, f'{slug}.html', title, f'{i:02}')
    links += '<div class="nav-bottom">' + link('resources', 'resources.html', 'Slides & R Code') + link('references', 'references.html', 'References') + '</div>'
    return f'<nav class="sidebar" id="book-navigation" aria-label="Book contents">{links}</nav>'

def page(slug, title, content):
    text = f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="{html.escape(title)} — Win Ratio and Beyond, by Lu Mao.">
<title>{html.escape(title)} — Win Ratio and Beyond</title><link rel="stylesheet" href="book.css?v={STYLE_VERSION}"><link rel="icon" href="favicon.svg" type="image/svg+xml"><script src="app.js" defer></script></head>
<body><a class="skip" href="#main">Skip to content</a>
<header class="masthead"><a class="brand" href="index.html"><span class="monogram" aria-hidden="true">CE</span><span>COMPOSITE<br>ENDPOINTS</span></a><div class="masthead-right"><a href="resources.html">Course Materials</a><a href="references.html">References</a><button class="menu-toggle" aria-expanded="false" aria-controls="book-navigation">Contents</button></div></header>
<div class="layout">{nav(slug)}<main class="main" id="main">{content}<footer class="footer"><span class="footer-title">Statistical Methods for Composite Endpoints<span>Win Ratio and Beyond</span></span><span class="footer-author">Lu Mao</span></footer></main></div></body></html>'''
    (OUTPUT / f'{slug}.html').write_text(text, encoding='utf-8')

def illustration():
    return '''<div class="figure-panel"><p class="eyebrow">A Pairwise Comparison</p><h2>What counts as a win?</h2><p>Compare survival first, then time to first hospitalization. Move the time horizon to see the comparison change.</p>
<div class="patient-chart"><div class="patient-labels" aria-hidden="true"><span>Treatment</span><span>Control</span></div><div class="patient-plot"><div class="plot-canvas">
<svg class="timeline" viewBox="0 0 248 120" preserveAspectRatio="none" role="img" aria-labelledby="timeline-title timeline-desc"><title id="timeline-title">Two illustrative patient histories</title><desc id="timeline-desc">Top row, treatment: hospitalized at year 1.2, alive through year 4. Bottom row, control: hospitalized at year 2.5, dies at year 3.4. Complete outcome histories are assumed.</desc>
<g class="grid"><path d="M0 0V120M62 0V120M124 0V120M186 0V120M248 0V120"/></g>
<path class="track" d="M0 30H248"/><path class="track control" d="M0 90H210.8"/>

<rect id="future" x="124" y="0" width="124" height="120" fill="#f5efef" opacity=".78"/>
<line class="tau-line" id="tau-line" x1="124" x2="124" y1="0" y2="120"/>
</svg><span class="event-mark control-event" data-time="2.5" style="left:62.5%;top:90px"><svg class="event-icon" viewBox="0 0 12 12" aria-hidden="true"><circle cx="6" cy="6" r="4"/></svg></span><span class="event-mark treatment-event" data-time="1.2" style="left:30%;top:30px"><svg class="event-icon" viewBox="0 0 12 12" aria-hidden="true"><circle cx="6" cy="6" r="4"/></svg></span><span class="event-mark control-event" data-time="3.4" style="left:85%;top:90px"><svg class="event-icon" viewBox="0 0 12 12" aria-hidden="true"><path d="M2 2L10 10M10 2L2 10"/></svg></span></div><div class="time-axis" aria-hidden="true"><span>0</span><span>1</span><span>2</span><span>3</span><span>4</span></div><div class="axis-title">Years</div></div></div>
<div class="legend"><span><svg class="event-icon" viewBox="0 0 12 12" aria-hidden="true"><circle cx="6" cy="6" r="4"/></svg>Hospitalization</span><span><svg class="event-icon" viewBox="0 0 12 12" aria-hidden="true"><path d="M2 2L10 10M10 2L2 10"/></svg>Death</span><span><svg class="event-icon" viewBox="0 0 12 12" aria-hidden="true"><path d="M6 0V12" stroke-dasharray="2 2"/></svg>Time horizon</span></div>
<div class="time-control"><label for="horizon">Comparison by time τ <output id="horizon-value" for="horizon">2.0 years</output></label><div class="range-align"><input id="horizon" type="range" min="0" max="4" step="0.1" value="2" aria-describedby="pair-result"></div><div class="pair-result" id="pair-result" aria-live="polite"><strong class="result-winner">Control wins · Treatment loses</strong><span class="result-component">Deciding component: hospitalization</span><span class="result-reason">Both patients are alive; control has the later first hospitalization.</span></div></div><p class="figure-note">Illustrative pair, not trial data. Complete histories assumed.<br>Adapted from the time-horizon question in Chapter 1.</p></div>'''

rows = ''
for i, (title, slug, summary, tags, _) in enumerate(chapters, 1):
    script_link = f'<a href="code/{slug}.R" download>R Script ↓</a>' if i <= 4 else ''
    rows += f'''<div class="chapter-row"><span class="chapter-number">{i:02}</span><div><h3><a href="{slug}.html">{title}</a></h3><p>{summary}</p><div class="chapter-tags">{tags}</div></div><div class="chapter-tools"><a href="{slug}.html">Read →</a>{external(f'chap{i}.html', 'Slides ↗')}{script_link}</div></div>'''
page('index', TITLE, f'''<div class="hero"><div><p class="eyebrow">Theory, Methods & Applications</p><h1>Statistical Methods for Composite Endpoints</h1><p class="subtitle">Win Ratio and Beyond</p><p class="intro">How do we combine survival and nonfatal events into a meaningful measure of treatment effect? Explore the statistical ideas, estimands, and methods behind composite endpoints.</p><div class="author">{external('https://lmaowisc.github.io/', 'Lu Mao')}, PhD<small>University of Wisconsin–Madison</small></div><nav class="book-tools" aria-label="Book resources"><a href="#chapters">Chapters <span aria-hidden="true">↓</span></a><a href="resources.html">Slides &amp; R Code <span aria-hidden="true">→</span></a><a href="references.html">References <span aria-hidden="true">→</span></a></nav><section class="course-note"><h2>From the Course to the Book</h2><p>These materials grew out of a short course taught at the 2024 Annual Meeting of the Society for Clinical Trials (SCT). The five chapters form the starting structure for this book, connecting statistical theory with applications in R.</p></section></div>{illustration()}</div>
<section id="chapters"><div class="section-heading"><h2>The Chapters</h2></div>{rows}</section>''')

(OUTPUT / 'code').mkdir(exist_ok=True)
for i, (title, slug, summary, tags, headings) in enumerate(chapters, 1):
    source = (ROOT / f'{slug}.qmd').read_text(encoding='utf-8')
    chunks = re.findall(r'```\{r[^\n]*\}\s*\n(.*?)\n```', source, re.S)
    # The first chunk is the original worked analysis; later prose includes separate examples.
    if i <= 4 and chunks:
        (OUTPUT / 'code' / f'{slug}.R').write_text(f'# Extracted from {slug}.qmd, first R chunk.\n# Original course code; not re-executed for this website refresh.\n\n' + chunks[0] + '\n', encoding='utf-8')
    rendered = subprocess.run(
        ['quarto', 'pandoc', str(HERE / f'{slug}.qmd'), '--from=markdown',
         '--to=html5', '--section-divs', '--mathml', '--wrap=none'],
        capture_output=True, text=True, encoding='utf-8', check=True,
    ).stdout
    chapter = BeautifulSoup(rendered, 'html.parser')
    for formula in chapter.select('math[display="block"]'):
        if formula.parent.name == 'p':
            formula.parent['class'] = ['equation']
        else:
            wrapper = chapter.new_tag('div', attrs={'class': 'equation'})
            formula.wrap(wrapper)
    toc = ''
    for j, section in enumerate(chapter.select('section.level2'), 1):
        heading = section.find('h2')
        label = heading.get_text()
        section_id = section['id']
        if section_id == 'reading':
            for listing in section.select('ul'):
                listing['class'] = ['reading-list']
            continue
        number = chapter.new_tag('span', attrs={'class': 'section-no'})
        number.string = f'{i}.{j}'
        heading.insert(0, number)
        toc += f'<a href="#{html.escape(section_id)}">{i}.{j} {html.escape(label)}</a>'
    for figure in chapter.select('img[src]'):
        if figure['src'].startswith('../images/'):
            figure['src'] = figure['src'][3:]
    body = str(chapter)
    if i <= 4:
        code_link = f'<a href="code/{slug}.R" download>Download R Code ↓</a>'
    else:
        code_link = '<a href="resources.html#software">Software Resources →</a>'
    prev = ('index', 'About This Book') if i == 1 else (chapters[i-2][1], chapters[i-2][0])
    nxt = ('references', 'References') if i == 5 else (chapters[i][1], chapters[i][0])
    body += f'<nav class="page-turn" aria-label="Chapter navigation"><a href="{prev[0]}.html"><small>PREVIOUS</small>← {prev[1]}</a><a href="{nxt[0]}.html"><small>NEXT</small>{nxt[1]} →</a></nav>'
    status = html.escape(chapter_metadata[slug]['status'])
    page(slug, title, f'<div class="reader-layout"><article class="article"><p class="eyebrow">Chapter {i:02}</p><h1>{title}</h1><p class="deck">{summary}</p><div class="chapter-actions">{external(f"chap{i}.html", "Open Chapter Slides ↗")}{code_link}</div><p class="chapter-status">{status}</p>{body}</article><aside class="toc" aria-label="On this page"><strong>ON THIS PAGE</strong>{toc}<a href="#reading">Selected Reading</a></aside></div>')

resource_rows = ''
for i, (title, slug, *_rest) in enumerate(chapters, 1):
    code_link = f'<a href="code/{slug}.R" download>R script ↓</a>' if i <= 4 else '—'
    resource_rows += f'<tr><td>{i:02} &nbsp; {title}</td><td>{external(f"chap{i}.html", "Slides ↗")}</td><td>{code_link}</td></tr>'
software = [('Wcompo', 'Weighted total-event analyses', 'https://cran.r-project.org/package=Wcompo'), ('WR', 'Win ratio tests, sample size, and PW regression', 'https://cran.r-project.org/package=WR'), ('rmt', 'Restricted mean time analyses', 'https://cran.r-project.org/package=rmt'), ('WA', 'While-alive estimands', 'https://cran.r-project.org/package=WA'), ('WRNet', 'Regularized win ratio regression', 'https://lmaowisc.github.io/wrnet/'), ('WinKM', 'Win–loss measures from published survival summaries', 'https://lmaowisc.github.io/winkm/')]
software_rows = ''.join(f'<tr><td>{external(url, name + " ↗")}</td><td>{desc}</td></tr>' for name, desc, url in software)
page('resources', 'Slides & R Code', f'''<p class="eyebrow">Companion Materials</p><h1 class="resource-title">Slides & R Code</h1><p class="deck">The original course materials, organized around the five chapters.</p><section class="resource-group"><h2>Chapter Materials</h2><p>Slides open in a separate tab. R scripts reproduce the original code blocks from the companion notes; they have not been re-executed for this website refresh.</p><table class="resource-table"><thead><tr><th>Chapter</th><th>Presentation</th><th>Analysis</th></tr></thead><tbody>{resource_rows}</tbody></table></section><section class="resource-group" id="software"><h2>Software Used in the Course</h2><table class="resource-table"><thead><tr><th>Package / toolkit</th><th>Role in the course</th></tr></thead><tbody>{software_rows}</tbody></table><div class="code-block"><div class="code-head"><span>R · Install the core packages</span><button class="copy-button" aria-label="Copy installation code">Copy</button></div><pre><code>install.packages(c("Wcompo", "WR", "rmt", "WA"))</code></pre></div></section><section class="resource-group"><h2>Bibliography</h2><p>Browse the <a href="references.html">searchable reference collection</a> or <a href="references.bib" download>download the original BibTeX bibliography ↓</a>.</p></section>''')

bibliography = subprocess.run(
    ['quarto', 'pandoc', str(ROOT / 'references.qmd'), '--citeproc',
     '--bibliography', str(HERE / 'references.bib'), '--csl', str(ROOT / 'apa.csl'), '-t', 'html'],
    capture_output=True, text=True, encoding='utf-8', check=True,
).stdout
soup = BeautifulSoup(bibliography, 'html.parser')
refs = soup.select_one('#refs')
if not refs:
    raise RuntimeError('Bibliography rendering did not produce references.')
for a in refs.select('a[href]'):
    if a['href'].startswith(('https://', 'http://')):
        a['target'] = '_blank'
        a['rel'] = 'noopener noreferrer'
count = len(refs.select('.csl-entry'))
page('references', 'References', f'''<p class="eyebrow">The Literature</p><h1 class="resource-title">References</h1><p class="deck">The course bibliography: foundations, methods, and applications.</p><p class="chapter-status">Generated from the current course bibliography. Original publication details retained.</p><label for="reference-filter">Find a reference</label><input class="reference-search" type="search" id="reference-filter" placeholder="Search by author, title, year, or journal…"><div class="count" id="reference-count" aria-live="polite">{count} references</div><p class="no-results" hidden>No matching references. Try a different author or keyword.</p>{refs}''')
(OUTPUT / 'favicon.svg').write_text('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40"><rect width="40" height="40" rx="3" fill="#51334d"/><text x="5" y="28" font-family="Georgia,serif" font-size="25" fill="#fcfaf7">CE</text></svg>', encoding='utf-8')
print(f'Built 8 website pages, 4 original R scripts, and {count} bibliography entries.')
