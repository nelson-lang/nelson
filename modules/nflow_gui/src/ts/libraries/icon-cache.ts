//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Shared, process-wide cache for block-icon SVG files.
//
// Every unique icon URL is fetched at most once per session, parsed once, and
// its colors extracted once. A block used to trigger up to four fetches of the
// same file (palette button, canvas `<image>`, color probes), which is slow on
// the WebView2 host. `prewarmIcons()` fetches the whole set in parallel so the
// parsed SVG is available synchronously via `getIconSync()`.
export interface IconColors {
  fill?: string;
  stroke?: string;
}
//=============================================================================
export interface CachedIcon {
  url: string;
  ok: boolean;
  /** Raw SVG source text, or null when the fetch/parse failed. */
  svgText: string | null;
  /** Parsed, detached <svg> element (import/clone before inserting). */
  svgEl: SVGSVGElement | null;
  /** Fill/stroke sampled from the icon, or null when none/unavailable. */
  colors: IconColors | null;
}
//=============================================================================
// The bundled app (app.js) and the externally-imported library-loader.js are
// SEPARATE module instances, so this module is evaluated twice. The sprite
// would warm one cache while the palette and canvas read the other, so the state
// lives in a single object on globalThis.
interface IconCacheStore {
  cache: Map<string, CachedIcon>;
  inflight: Map<string, Promise<CachedIcon>>;
  spriteLoaded: Promise<void> | null;
}
//=============================================================================
function getStore(): IconCacheStore {
  const g = (typeof globalThis !== 'undefined' ? globalThis : {}) as {
    __nf_icon_cache_store?: IconCacheStore;
  };
  if (!g.__nf_icon_cache_store) {
    g.__nf_icon_cache_store = { cache: new Map(), inflight: new Map(), spriteLoaded: null };
  }
  return g.__nf_icon_cache_store;
}
//=============================================================================
const store = getStore();
const cache = store.cache;
const inflight = store.inflight;
function extractColors(svg: Element): IconColors | null {
  let fill: string | null = null;
  let stroke: string | null = null;

  try {
    const f = svg.getAttribute('fill');
    const s = svg.getAttribute('stroke');
    if (f && f !== 'none') fill = f;
    if (s && s !== 'none') stroke = s;
  } catch {
    // ignore
  }

  if (!fill) {
    const el = svg.querySelector(
      'path[fill], rect[fill], circle[fill], ellipse[fill], polygon[fill], polyline[fill]'
    );
    if (el) {
      const f = el.getAttribute('fill');
      if (f && f !== 'none') fill = f;
    }
  }
  if (!stroke) {
    const el = svg.querySelector(
      'path[stroke], rect[stroke], circle[stroke], ellipse[stroke], polygon[stroke], polyline[stroke]'
    );
    if (el) {
      const s = el.getAttribute('stroke');
      if (s && s !== 'none') stroke = s;
    }
  }

  if ((!fill || !stroke) && svg.querySelectorAll) {
    const els = Array.from(svg.querySelectorAll('path,rect,circle,ellipse,polygon,polyline'));
    for (const e of els) {
      if (!fill) {
        const f = e.getAttribute('fill') || '';
        if (f && f !== 'none') {
          fill = f;
        } else {
          const style = e.getAttribute('style') || '';
          const m = style.match(/fill:\s*([^;]+)/);
          if (m && m[1] && m[1].trim() !== 'none') fill = m[1].trim();
        }
      }
      if (!stroke) {
        const s = e.getAttribute('stroke') || '';
        if (s && s !== 'none') {
          stroke = s;
        } else {
          const style = e.getAttribute('style') || '';
          const m2 = style.match(/stroke:\s*([^;]+)/);
          if (m2 && m2[1] && m2[1].trim() !== 'none') stroke = m2[1].trim();
        }
      }
      if (fill && stroke) break;
    }
  }

  if (!fill && !stroke) return null;
  return { fill: fill ?? undefined, stroke: stroke ?? undefined };
}
//=============================================================================
function makeFailed(url: string): CachedIcon {
  return { url, ok: false, svgText: null, svgEl: null, colors: null };
}
//=============================================================================
async function fetchAndParse(url: string): Promise<CachedIcon> {
  let resp: Response | null = null;
  try {
    resp = await fetch(url);
  } catch {
    return makeFailed(url);
  }
  if (!resp || !resp.ok) return makeFailed(url);

  let text = '';
  try {
    text = await resp.text();
  } catch {
    return makeFailed(url);
  }
  if (!text) return makeFailed(url);

  const parser = typeof DOMParser !== 'undefined' ? new DOMParser() : null;
  if (!parser) {
    return { url, ok: true, svgText: text, svgEl: null, colors: null };
  }

  try {
    const doc = parser.parseFromString(text, 'image/svg+xml');
    const svg = doc.querySelector('svg');
    if (!svg) return { url, ok: false, svgText: text, svgEl: null, colors: null };
    return {
      url,
      ok: true,
      svgText: text,
      svgEl: svg as unknown as SVGSVGElement,
      colors: extractColors(svg),
    };
  } catch {
    return { url, ok: false, svgText: text, svgEl: null, colors: null };
  }
}
/** Resolve an icon; concurrent callers for the same URL share one request. */
export function getIcon(url: string): Promise<CachedIcon> {
  const cached = cache.get(url);
  if (cached) return Promise.resolve(cached);
  const pending = inflight.get(url);
  if (pending) return pending;

  // A builtin icon (.../libraries/<lib>/exports/<name>.svg) requested before
  // anything started the sprite starts it here, deriving the sprite URL from the
  // icon URL, so the first request does not fetch the icon individually.
  if (!store.spriteLoaded) {
    const m = url.match(/^(.*\/libraries\/)[^?]*\/exports\/[^/?]+\.svg(\?.*)?$/i);
    if (m) {
      try {
        loadBuiltinSprite(`${m[1]}_builtin-icons.sprite.svg`);
      } catch {
        // ignore; fall through to a direct fetch
      }
    }
  }

  // While the sprite is loading, wait for it before an individual fetch: the
  // icon is almost certainly in it. Once it has settled, or for external
  // library icons, this adds no delay.
  if (store.spriteLoaded) {
    const p = (async () => {
      try {
        await store.spriteLoaded;
      } catch {
        // ignore sprite failure; fall through to a direct fetch
      }
      const afterSprite = cache.get(url);
      if (afterSprite) return afterSprite;
      const stillPending = inflight.get(url);
      if (stillPending) return stillPending;
      const q = fetchAndParse(url).then((result) => {
        cache.set(url, result);
        inflight.delete(url);
        return result;
      });
      inflight.set(url, q);
      return q;
    })();
    return p;
  }

  const p = fetchAndParse(url).then((result) => {
    cache.set(url, result);
    inflight.delete(url);
    return result;
  });
  inflight.set(url, p);
  return p;
}
//=============================================================================
/** Returns the cached icon if it is already resolved. */
export function getIconSync(url: string): CachedIcon | undefined {
  return cache.get(url);
}
/**
 * True once the builtin sprite load has started (loading or done). Consumers
 * wait for the sprite instead of fetching per file; the canvas uses it to avoid
 * a throwaway async <image> at startup.
 */
export function builtinSpritePending(): boolean {
  return store.spriteLoaded != null;
}
/**
 * Returns a copy of the cached icon's <svg>, or null. Callers must
 * `document.importNode(...)` the children (cross-document adoption) before
 * inserting them into the live document.
 */
export function cloneIconSvg(url: string): SVGSVGElement | null {
  const cached = cache.get(url);
  if (cached && cached.svgEl) return cached.svgEl.cloneNode(true) as SVGSVGElement;
  return null;
}
//=============================================================================
/** Fetches a set of icon URLs in parallel; resolves when all settle. */
export function prewarmIcons(urls: Iterable<string>): Promise<void> {
  const uniq = new Set<string>();
  for (const u of urls) if (u) uniq.add(u);
  if (uniq.size === 0) return Promise.resolve();
  return Promise.all(Array.from(uniq, (u) => getIcon(u).catch(() => makeFailed(u)))).then(
    () => undefined
  );
}
/**
 * Canonicalize a builtin icon URL so the palette (icons at the library root) and
 * the canvas (`exports/...`) share one cache entry. A root-level
 * `.../libraries/<dir>/<name>.svg` becomes `.../libraries/<dir>/exports/<name>.svg`;
 * other URLs (already in `exports/`, deeper, data: URIs, not `.svg`) are returned
 * unchanged. Callers keep the original URL as a fallback for external libraries
 * laid out differently.
 */
export function toExportsUrl(url: string): string {
  if (!url || /^data:/i.test(url)) return url;
  const m = url.match(/^(.*\/libraries\/[^/]+\/)(?!exports\/)([^/]+\.svg)(\?.*)?$/i);
  if (!m) return url;
  return `${m[1]}exports/${m[2]}${m[3] || ''}`;
}
/**
 * Load the builtin icon sprite (one SVG of `<symbol>`s built from the
 * `exports/*.svg` icons) in one request and fill the URL-keyed cache, so every
 * builtin icon resolves synchronously via getIconSync(). The WebView2 host needs
 * it: 183 individual round-trips are slow enough that a diagram paints before its
 * icons arrive. External libraries are not in the sprite. Loads at most once; on
 * failure the per-icon getIcon path still works.
 */
export function loadBuiltinSprite(spriteUrl: string): Promise<void> {
  if (store.spriteLoaded) return store.spriteLoaded;
  store.spriteLoaded = (async () => {
    let text = '';
    try {
      const resp = await fetch(spriteUrl);
      if (!resp || !resp.ok) return;
      text = await resp.text();
    } catch {
      return;
    }
    if (!text) return;
    const parser = typeof DOMParser !== 'undefined' ? new DOMParser() : null;
    if (!parser) return;
    let doc: Document;
    try {
      doc = parser.parseFromString(text, 'image/svg+xml');
    } catch {
      return;
    }
    const spriteBase = new URL('./', spriteUrl).href; // .../libraries/
    const SVG_NS = 'http://www.w3.org/2000/svg';
    for (const sym of Array.from(doc.querySelectorAll('symbol'))) {
      const lib = sym.getAttribute('data-lib');
      const name = sym.getAttribute('data-name');
      if (!lib || !name) continue;
      // Consumers read childNodes and viewBox, as for a fetched icon.
      const svg = doc.createElementNS(SVG_NS, 'svg') as unknown as SVGSVGElement;
      const vb = sym.getAttribute('viewBox');
      if (vb) svg.setAttribute('viewBox', vb);
      const f = sym.getAttribute('fill');
      if (f) svg.setAttribute('fill', f);
      const s = sym.getAttribute('stroke');
      if (s) svg.setAttribute('stroke', s);
      for (const child of Array.from(sym.childNodes)) svg.appendChild(child.cloneNode(true));
      let url: string;
      try {
        url = new URL(`${lib}/exports/${name}`, spriteBase).href;
      } catch {
        continue;
      }
      // Do not clobber a real per-file fetch that already resolved.
      if (!cache.has(url)) {
        cache.set(url, { url, ok: true, svgText: null, svgEl: svg, colors: extractColors(svg) });
      }
    }
  })();
  return store.spriteLoaded;
}
//=============================================================================
/** Test-only. */
export function _resetIconCache(): void {
  cache.clear();
  inflight.clear();
  store.spriteLoaded = null;
}
//=============================================================================
