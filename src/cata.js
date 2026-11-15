// CATA (Check-All-That-Apply) con aceptabilidad y penalidades.
// Referencias: Meyners & Castura (cap. 11 de Varela & Ares, "Novel Techniques in Sensory Characterization
// and Consumer Profiling"); Meyners, Castura & Carr (2013); Ares et al. (2014) penalidades con producto ideal.
import { lgamma, jacobi, pF } from './analisis.js';

export const GUSTO = 'Aceptabilidad';
export const HEDONICA = [
  'Me disgusta muchísimo', 'Me disgusta mucho', 'Me disgusta moderadamente', 'Me disgusta levemente',
  'Ni me gusta ni me disgusta',
  'Me gusta levemente', 'Me gusta moderadamente', 'Me gusta mucho', 'Me gusta muchísimo',
];

// P(χ² > x) con gl grados de libertad (gamma incompleta regularizada)
export function pChi2(x, gl) {
  if (!(x > 0) || gl <= 0) return 1;
  const a = gl / 2, z = x / 2;
  if (z < a + 1) {           // serie
    let ap = a, sum = 1 / a, del = sum;
    for (let n = 0; n < 500; n++) { ap++; del *= z / ap; sum += del; if (Math.abs(del) < Math.abs(sum) * 1e-14) break; }
    return Math.max(0, 1 - sum * Math.exp(-z + a * Math.log(z) - lgamma(a)));
  }
  let b = z + 1 - a, c = 1 / 1e-300, d = 1 / b, h = d;   // fracción continua
  for (let i = 1; i < 500; i++) {
    const an = -i * (i - a); b += 2;
    d = an * d + b; if (Math.abs(d) < 1e-300) d = 1e-300;
    c = b + an / c; if (Math.abs(c) < 1e-300) c = 1e-300;
    d = 1 / d; const del = d * c; h *= del; if (Math.abs(del - 1) < 1e-14) break;
  }
  return Math.min(1, Math.exp(-z + a * Math.log(z) - lgamma(a)) * h);
}

const media = a => (a.length ? a.reduce((s, x) => s + x, 0) / a.length : null);
const varianza = a => { const m = media(a); return a.length > 1 ? a.reduce((s, x) => s + (x - m) ** 2, 0) / (a.length - 1) : null; };

// t de Welch para dos grupos → p bilateral
export function welch(a, b) {
  if (a.length < 2 || b.length < 2) return { t: null, p: null };
  const va = varianza(a) / a.length, vb = varianza(b) / b.length, se = Math.sqrt(va + vb);
  const dif = media(a) - media(b);
  if (!(se > 0)) return { t: dif ? Infinity : 0, p: dif ? 0 : 1 };
  const t = dif / se, gl = (va + vb) ** 2 / (va * va / (a.length - 1) + vb * vb / (b.length - 1));
  return { t, p: pF(t * t, 1, gl) };
}

// Q de Cochran por término: bloques = consumidor × repetición con todas las muestras evaluadas
export function cochran(filas, terminos, vinos) {
  const bloques = {};
  for (const f of filas) { const k = `${f.Valuador}|${f.Rep}`; (bloques[k] = bloques[k] || {})[f.NombreProducto] = f; }
  const completos = Object.values(bloques).filter(b => vinos.every(v => b[v]));
  const k = vinos.length, out = {};
  for (const t of terminos) {
    if (k < 2 || completos.length < 2) { out[t] = { Q: null, p: null }; continue; }
    const C = vinos.map(v => completos.reduce((s, b) => s + (Number(b[v][t]) > 0 ? 1 : 0), 0));
    const R = completos.map(b => vinos.reduce((s, v) => s + (Number(b[v][t]) > 0 ? 1 : 0), 0));
    const N = C.reduce((s, x) => s + x, 0);
    const den = k * N - R.reduce((s, x) => s + x * x, 0);
    if (den <= 0) { out[t] = { Q: 0, p: 1 }; continue; }
    const Q = (k - 1) * (k * C.reduce((s, x) => s + x * x, 0) - N * N) / den;
    out[t] = { Q, p: pChi2(Q, k - 1) };
  }
  return { porTermino: out, bloques: completos.length };
}

// Análisis de correspondencias (como FactoMineR::CA) sobre la tabla vinos × términos (frecuencias);
// sup = filas suplementarias (ej. el ideal) proyectadas sobre el mapa.
export function correspondencias(vinos, terminos, tabla, sup = []) {
  const ts = terminos.filter(t => vinos.reduce((s, v) => s + tabla[v][t], 0) > 0);
  const vs = vinos.filter(v => ts.reduce((s, t) => s + tabla[v][t], 0) > 0);
  if (vs.length < 3 || ts.length < 2) return null;
  const n = vs.reduce((s, v) => s + ts.reduce((u, t) => u + tabla[v][t], 0), 0);
  const r = vs.map(v => ts.reduce((s, t) => s + tabla[v][t], 0) / n);
  const c = ts.map(t => vs.reduce((s, v) => s + tabla[v][t], 0) / n);
  const S = vs.map((v, i) => ts.map((t, j) => (tabla[v][t] / n - r[i] * c[j]) / Math.sqrt(r[i] * c[j])));
  const SSt = vs.map((_, i) => vs.map((_, k) => S[i].reduce((s, x, j) => s + x * S[k][j], 0)));
  const { valores, vectores } = jacobi(SSt);
  const total = valores.reduce((s, x) => s + Math.max(0, x), 0);
  if (!(valores[1] > 1e-12)) return null;
  const sv = [Math.sqrt(valores[0]), Math.sqrt(valores[1])];
  const filasC = vs.map((v, i) => ({ v, x: vectores[0][i] / Math.sqrt(r[i]) * sv[0], y: vectores[1][i] / Math.sqrt(r[i]) * sv[1] }));
  // columnas: estándar = Sᵀu / (σ √c); principales = estándar × σ
  const std = ts.map((t, j) => [0, 1].map(d => vs.reduce((s, _, i) => s + S[i][j] * vectores[d][i], 0) / (sv[d] * Math.sqrt(c[j]))));
  const colsC = ts.map((t, j) => ({ a: t, x: std[j][0] * sv[0], y: std[j][1] * sv[1] }));
  const supC = sup.map(({ v, perfil }) => {
    const tot = ts.reduce((s, t) => s + (perfil[t] || 0), 0); if (!tot) return null;
    return { v, x: ts.reduce((s, t, j) => s + (perfil[t] || 0) / tot * std[j][0], 0), y: ts.reduce((s, t, j) => s + (perfil[t] || 0) / tot * std[j][1], 0) };
  }).filter(Boolean);
  const chi2 = n * total, gl = (vs.length - 1) * (ts.length - 1);
  return { filas: filasC, cols: colsC, sup: supC, pct: [100 * valores[0] / total, 100 * valores[1] / total], chi2, gl, p: pChi2(chi2, gl), fuera: terminos.filter(t => !ts.includes(t)) };
}

// Todo el análisis CATA.
// filas: [{ NombreProducto, Valuador, Rep, Aceptabilidad?, <término>: 0/1 }]; ideales: { Valuador: { término: 0/1 } }
export function analizarCata(filas, terminos, { gusto = true, ideales = null } = {}) {
  const vinos = [...new Set(filas.map(f => f.NombreProducto).filter(Boolean))].sort((a, b) => String(a).localeCompare(String(b), 'es', { numeric: true }));
  const n = {}, tabla = {};
  vinos.forEach(v => { n[v] = 0; tabla[v] = {}; terminos.forEach(t => { tabla[v][t] = 0; }); });
  filas.forEach(f => { if (!(f.NombreProducto in n)) return; n[f.NombreProducto]++; terminos.forEach(t => { if (Number(f[t]) > 0) tabla[f.NombreProducto][t]++; }); });
  const N = filas.length;
  const total = {}; terminos.forEach(t => { total[t] = vinos.reduce((s, v) => s + tabla[v][t], 0); });

  // términos citados por el ideal
  let ideal = null;
  if (ideales && Object.keys(ideales).length) {
    const cons = Object.keys(ideales);
    ideal = { n: cons.length, frec: {} };
    terminos.forEach(t => { ideal.frec[t] = cons.filter(c => Number(ideales[c][t]) > 0).length; });
  }

  const q = cochran(filas, terminos, vinos);
  const ca = correspondencias(vinos, terminos, tabla, ideal ? [{ v: 'Ideal', perfil: ideal.frec }] : []);

  // aceptabilidad: medias, desvío y ANOVA vino + consumidor
  let agrado = null;
  if (gusto) {
    const ok = filas.filter(f => Number(f[GUSTO]) >= 1);
    const por = {}; vinos.forEach(v => { por[v] = ok.filter(f => f.NombreProducto === v).map(f => Number(f[GUSTO])); });
    const mc = {}, cc = {}; ok.forEach(f => { mc[f.Valuador] = (mc[f.Valuador] || 0) + Number(f[GUSTO]); cc[f.Valuador] = (cc[f.Valuador] || 0) + 1; });
    const ys = {}; ok.forEach(f => { (ys[f.NombreProducto] = ys[f.NombreProducto] || []).push(Number(f[GUSTO]) - mc[f.Valuador] / cc[f.Valuador]); });
    const todos = Object.values(ys).flat(), gm = media(todos) || 0;
    let ssB = 0, ssW = 0; Object.values(ys).forEach(a => { const m = media(a); ssB += a.length * (m - gm) ** 2; a.forEach(x => { ssW += (x - m) ** 2; }); });
    const k = Object.keys(ys).length, d1 = k - 1, d2 = ok.length - k - (Object.keys(mc).length - 1);
    const F = d1 > 0 && d2 > 0 && ssW > 0 ? (ssB / d1) / (ssW / d2) : null;
    agrado = {
      vinos: vinos.map(v => ({ v, n: por[v].length, media: media(por[v]), sd: varianza(por[v]) == null ? null : Math.sqrt(varianza(por[v])),
        gustan: por[v].filter(x => x >= 6).length, disgustan: por[v].filter(x => x <= 4).length })),
      F, p: F == null ? null : pF(F, d1, d2), d1, d2,
    };

    // impacto de cada término en el agrado (con vs. sin el término), todas las evaluaciones juntas
    var impacto = terminos.map(t => {
      const con = ok.filter(f => Number(f[t]) > 0).map(f => Number(f[GUSTO])), sin = ok.filter(f => !(Number(f[t]) > 0)).map(f => Number(f[GUSTO]));
      const w = welch(con, sin);
      return { t, pct: ok.length ? 100 * con.length / ok.length : 0, con: media(con), sin: media(sin),
        efecto: con.length && sin.length ? media(con) - media(sin) : null, p: w.p };
    });

    // penalidades con el producto ideal (Ares et al., 2014)
    var penal = null;
    if (ideales && Object.keys(ideales).length) {
      const ev = ok.filter(f => ideales[f.Valuador]);
      penal = terminos.map(t => {
        const g = { '11': [], '10': [], '00': [], '01': [] };
        ev.forEach(f => { g[`${Number(ideales[f.Valuador][t]) > 0 ? 1 : 0}${Number(f[t]) > 0 ? 1 : 0}`].push(Number(f[GUSTO])); });
        const falta = welch(g['11'], g['10']), sobra = welch(g['00'], g['01']);
        return {
          t, nEv: ev.length,
          falta: { pct: ev.length ? 100 * g['10'].length / ev.length : 0, caida: g['11'].length && g['10'].length ? media(g['11']) - media(g['10']) : null, p: falta.p },
          sobra: { pct: ev.length ? 100 * g['01'].length / ev.length : 0, caida: g['00'].length && g['01'].length ? media(g['00']) - media(g['01']) : null, p: sobra.p },
        };
      });
    }
  }
  return { vinos, n, N, tabla, total, ideal, q, ca, agrado, impacto: gusto ? impacto : null, penal: gusto ? penal : null };
}
