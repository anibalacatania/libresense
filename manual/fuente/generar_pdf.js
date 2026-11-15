// Genera manual/LibreSense-manual.pdf a partir de p1.html … p5.html (necesita playwright y la fuente Inter).
// Uso: node manual/fuente/generar_pdf.js
const { chromium } = require('playwright');
(async () => {
  const b = await chromium.launch(); const p = await b.newPage();
  const fs = require('fs'), path = require('path');
  // une las partes p1…p5 en manual.html
  fs.writeFileSync(path.join(__dirname, 'manual.html'), [1, 2, 3, 4, 5].map(i => fs.readFileSync(path.join(__dirname, `p${i}.html`), 'utf8')).join(''));
  await p.goto('file://' + path.join(__dirname, 'manual.html'), { waitUntil: 'networkidle' });
  await p.pdf({ path: require('path').join(__dirname, '..', 'LibreSense-manual.pdf'), format: 'A4', printBackground: true, preferCSSPageSize: true,
    displayHeaderFooter: true, headerTemplate: '<div></div>',
    footerTemplate: '<div style="width:100%;font-size:7.5pt;color:#9ca3af;font-family:Inter,sans-serif;padding:0 18mm;display:flex;justify-content:space-between"><span>LibreSense · Manual de uso</span><span class="pageNumber"></span></div>' });
  await b.close();
})();
