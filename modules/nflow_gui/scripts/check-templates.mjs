import { fileURLToPath } from 'url';
import { createRequire } from 'module';
import { JSDOM } from 'jsdom';
import * as loader from '../dist/app/library-loader.js';

const require = createRequire(import.meta.url);
const BUILTIN_DIRS = ['source','math','continuous','discrete','nonlinear','sink','utility','userdefined'];

// bootstrap a minimal DOM like jest's jsdom environment
const dom = new JSDOM('<!doctype html><html><body></body></html>');
globalThis.window = dom.window;
globalThis.document = dom.window.document;
globalThis.CustomEvent = dom.window.CustomEvent;
globalThis.HTMLElement = dom.window.HTMLElement;
globalThis.Element = dom.window.Element;
globalThis.requestAnimationFrame = (cb) => setTimeout(cb, 0);
globalThis.cancelAnimationFrame = (id) => clearTimeout(id);

const discoveredManifests = loader.getDiscoveredManifests();
for (const dir of BUILTIN_DIRS) {
  const manifestPath = fileURLToPath(new URL(`../dist/libraries/${dir}/library.json`, import.meta.url));
  // eslint-disable-next-line import/no-dynamic-require, global-require
  const manifest = require(manifestPath);
  manifest._dir = dir;
  discoveredManifests.set(`builtin.${dir}`, manifest);
}

(async () => {
  const results = await Promise.all(BUILTIN_DIRS.map((d) => loader.loadLibrary(`builtin.${d}`)));
  console.log('loadLibrary results:', results.map(r => !!r));
  const loaded = loader.getLoadedLibraries();
  console.log('loadedLibraries keys:', Array.from(loaded.keys()));
  const templates = loader.buildAllBlockTemplates({});
  console.log('templates count:', Object.keys(templates).length);
  console.log('sample templates:', Object.keys(templates).slice(0,50));
  console.log('has gain:', Object.prototype.hasOwnProperty.call(templates, 'gain'));
  // refresh blockTemplates in the templates module (like the tests do)
  const tpl = await import('../dist/render/block/templates.js');
  tpl.refreshBlockTemplates();

  // Now import createRenderer and exercise createBlock similarly to the failing test
  const mod = await import('../dist/render/create-renderer.js');
  const { createRenderer } = mod;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  Object.defineProperty(svg, 'clientWidth', { get: () => 600, configurable: true });
  Object.defineProperty(svg, 'clientHeight', { get: () => 400, configurable: true });
  svg.createSVGPoint = function () { return { x: 0, y: 0, matrixTransform: (m) => ({ x: 0, y: 0 }) }; };
  svg.getScreenCTM = () => ({ inverse: () => ({}) });

  const blockLayer = document.createElementNS(ns, 'g');
  const wireLayer = document.createElementNS(ns, 'g');
  const overlayLayer = document.createElementNS(ns, 'g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    routingDirty: false,
    fastRouting: false,
    autoRoute: false,
    routeEpoch: 0,
    selectedConnection: null,
  };

  const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state, onSelectBlock: () => { }, onSelectConnection: () => { }, onOpenSubsystem: () => { }, onConnectionError: () => { } });
  const a = renderer.createBlock('constant', 10, 10, { id: 'A', params: { value: 1 } });
  const b = renderer.createBlock('gain', 150, 10, { id: 'B', params: { gain: 2 } });
  console.log('a exists?', !!a, 'b exists?', !!b);
})();
