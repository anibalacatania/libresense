// Sorting (agrupamiento libre): cada panelista arma grupos de muestras parecidas y, opcionalmente, describe cada grupo.
// Referencias: Chollet, Valentin y Abdi (2014), "Free sorting task", cap. de Varela y Ares (eds.), Novel Techniques in
// Sensory Characterization and Consumer Profiling; Cadoret, Lê y Pagès (2009) FAST; Abdi et al. (2007) DISTATIS.
import { jacobi } from './analisis.js';

// respuestas → hojas (una por panelista × sesión) con el grupo y las palabras de cada producto
// nombre(r) = producto de la respuesta (con el vino real, que el panelista no ve)
export function hojasSorting(respuestas, nombre) {
  const hojas = {};
  for (const r of respuestas) {
    const id = `${r.Valuador}_${r.Sesion}`;
    const h = (hojas[id] = hojas[id] || { id, Valuador: r.Valuador, Sesion: r.Sesion, grupos: {}, pal: {} });
    const p = nombre(r);
    h.grupos[p] = Number((r.valores || {}).Grupo);
    const w = String((r.def || {}).Palabras || '').split(';').map(x => x.trim().toLowerCase()).filter(Boolean);
    h.pal[p] = w;
  }
  return Object.values(hojas);
}

// co-ocurrencias: cuántos panelistas pusieron cada par de productos en el mismo grupo (y cuántos evaluaron el par)
export function coocurrencias(productos, hojas) {
  const n = productos.length;
  const C = Array.from({ length: n }, () => Array(n).fill(0)), N = Array.from({ length: n }, () => Array(n).fill(0));
  for (const h of hojas) for (let i = 0; i < n; i++) for (let j = 0; j < n; j++) {
    const gi = h.grupos[productos[i]], gj = h.grupos[productos[j]];
    if (gi == null || gj == null || Number.isNaN(gi) || Number.isNaN(gj)) continue;
    N[i][j]++; if (gi === gj) C[i][j]++;
  }
  return { C, N };
}

// MDS clásico (Torgerson, como cmdscale) sobre la disimilitud 1 − proporción de veces juntos
export function mdsSorting(productos, { C, N }) {
  const n = productos.length;
  if (n < 3) return null;
  const D2 = productos.map((_, i) => productos.map((_, j) => { const d = i === j ? 0 : 1 - (N[i][j] ? C[i][j] / N[i][j] : 0); return d * d; }));
  const fm = D2.map(r => r.reduce((s, x) => s + x, 0) / n), gm = fm.reduce((s, x) => s + x, 0) / n;
  const B = D2.map((r, i) => r.map((x, j) => -0.5 * (x - fm[i] - fm[j] + gm)));
  const { valores, vectores } = jacobi(B);
  const pos = valores.filter(x => x > 1e-10), tot = pos.reduce((s, x) => s + x, 0);
  if (pos.length < 2) return null;
  return {
    filas: productos.map((v, i) => ({ v, x: vectores[0][i] * Math.sqrt(valores[0]), y: vectores[1][i] * Math.sqrt(valores[1]) })),
    pct: [100 * valores[0] / tot, 100 * valores[1] / tot],
  };
}

// palabras: veces que cada palabra se usó para cada producto
export function palabrasSorting(productos, hojas) {
  const t = {};
  for (const h of hojas) for (const p of productos) for (const w of h.pal[p] || []) { (t[w] = t[w] || {}); t[w][p] = (t[w][p] || 0) + 1; }
  return t;
}

// palabras ubicadas en el promedio de los productos donde se usaron (ponderado por las veces)
export function palabrasEnMapa(mapa, tabla, minimo = 2) {
  const pos = Object.fromEntries(mapa.filas.map(f => [f.v, f]));
  return Object.entries(tabla).map(([a, por]) => {
    const tot = Object.values(por).reduce((s, x) => s + x, 0); if (tot < minimo) return null;
    let x = 0, y = 0; Object.entries(por).forEach(([p, c]) => { if (pos[p]) { x += pos[p].x * c; y += pos[p].y * c; } });
    return { a, x: x / tot, y: y / tot, n: tot };
  }).filter(Boolean);
}

// clasificación jerárquica (enlace promedio) de los productos sobre la disimilitud; devuelve k grupos
export function grupos(productos, { C, N }, k) {
  let cl = productos.map((p, i) => [i]);
  const d = (a, b) => { let s = 0; a.forEach(i => b.forEach(j => { s += 1 - (N[i][j] ? C[i][j] / N[i][j] : 0); })); return s / (a.length * b.length); };
  while (cl.length > Math.max(1, k)) {
    let mi = 0, mj = 1, md = Infinity;
    for (let i = 0; i < cl.length; i++) for (let j = i + 1; j < cl.length; j++) { const x = d(cl[i], cl[j]); if (x < md) { md = x; mi = i; mj = j; } }
    cl[mi] = [...cl[mi], ...cl[mj]]; cl.splice(mj, 1);
  }
  return cl.map(g => g.map(i => productos[i]));
}
