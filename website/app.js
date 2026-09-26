const menu = document.querySelector('.menu-toggle');
menu?.addEventListener('click', () => {
  const expanded = menu.getAttribute('aria-expanded') !== 'true';
  menu.setAttribute('aria-expanded', String(expanded));
  document.querySelector('.sidebar').classList.toggle('open', expanded);
  menu.textContent = expanded ? 'Close' : 'Contents';
});
const slider = document.querySelector('#horizon');
if (slider) {
  const update = () => {
    const t = Number(slider.value), x = t * 62;
    document.querySelector('#horizon-value').textContent = `${t.toFixed(1)} years`;
    slider.setAttribute('aria-valuetext', `${t.toFixed(1)} years`);
    const line = document.querySelector('#tau-line');
    line.setAttribute('x1', x); line.setAttribute('x2', x);
    const shade = document.querySelector('#future');
    shade.setAttribute('x', x); shade.setAttribute('width', 248 - x);
    document.querySelectorAll('.event-mark').forEach(marker => {
      marker.classList.toggle('future-event', Number(marker.dataset.time) > t);
    });
    const result = t < 1.2
      ? ['Tie', 'No deciding component', 'Both patients are alive and hospitalization-free.']
      : t < 3.4
        ? ['Control wins · Treatment loses', 'Deciding component: hospitalization', 'Both patients are alive; control has the later first hospitalization.']
        : ['Treatment wins · Control loses', 'Deciding component: survival', 'Treatment survives longer. Survival takes priority over hospitalization.'];
    document.querySelector('.result-winner').textContent = result[0];
    document.querySelector('.result-component').textContent = result[1];
    document.querySelector('.result-reason').textContent = result[2];
  };
  slider.addEventListener('input', update); update();
}
const filter = document.querySelector('#reference-filter');
if (filter) {
  const entries = Array.from(document.querySelectorAll('.csl-entry'));
  const normalize = text => text.normalize('NFKD').replace(/[\u0300-\u036f]/g, '').toLowerCase().replace(/\s+/g, ' ').trim();
  filter.addEventListener('input', () => {
    const terms = normalize(filter.value).split(' ').filter(Boolean);
    let count = 0;
    for (const entry of entries) {
      const text = normalize(entry.textContent);
      const match = terms.every(term => text.includes(term));
      entry.hidden = !match; if (match) count++;
    }
    document.querySelector('#reference-count').textContent = `${count} of ${entries.length} references`;
    document.querySelector('.no-results').hidden = count > 0;
  });
}
document.querySelectorAll('.copy-button').forEach(button => {
  button.addEventListener('click', async () => {
    try {
      const panel = button.closest('.code-block, .analysis-block');
      await navigator.clipboard.writeText(panel.querySelector('pre code, pre').textContent);
      button.textContent = 'Copied';
      setTimeout(() => { button.textContent = 'Copy'; }, 2000);
    } catch { button.textContent = 'Select code to copy'; }
  });
});
