// ============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================
//
// Dev tool: (re)apply the acausal schematic layout (src/ts/libraries/acausal/
// schematic-layout.ts) to the shipped acausal example models. It resolves each
// block's physical-pin geometry (side + domain, plus per-instance PlanarBody
// frames) from the block libraries, runs the SAME layout function the GUI's
// "Auto-arrange (schematic)" command uses, and rewrites x/y/rotation in place.
// Layout is purely visual (acausal wiring is by pin name) so simulation is
// unaffected. Usage:  node scripts/apply-acausal-schematic-layout.mjs [--check]
// ============================================================================
import fs from 'fs';
import path from 'path';
import url from 'url';
import esbuild from 'esbuild';
import prettier from 'prettier';

const HERE = path.dirname(url.fileURLToPath(import.meta.url));
const GUI = path.resolve(HERE, '..');
const BLOCKS = path.resolve(GUI, '../nflow_blocks');
const LIBDIR = path.join(BLOCKS, 'libraries');
const EXDIR = path.join(BLOCKS, 'examples/acausal');
const check = process.argv.includes('--check');

// Transpile the TS layout module and import it (no bundler/runtime deps needed).
const built = await esbuild.build({
  entryPoints: [path.join(GUI, 'src/ts/libraries/acausal/schematic-layout.ts')],
  bundle: true,
  format: 'esm',
  platform: 'node',
  write: false,
});
const mod = await import('data:text/javascript,' + encodeURIComponent(built.outputFiles[0].text));
const { computeSchematicLayout } = mod;

// Library geometry: type -> { domain, acausal, pins[{name,side,domain}], outN, inN }.
const GEO = {};
for (const lib of fs.readdirSync(LIBDIR)) {
  const p = path.join(LIBDIR, lib, 'library.json');
  if (!fs.existsSync(p)) continue;
  const d = JSON.parse(fs.readFileSync(p, 'utf8'));
  for (const b of d.blocks || []) {
    GEO[b.type] = {
      width: (b.size && b.size[0]) || b.width || 100,
      height: (b.size && b.size[1]) || b.height || 80,
      domain: b.domain,
      acausal: !!b.acausal,
      pins: (b.physicalPins || []).map((p) => ({
        name: p.name,
        side: p.side,
        domain: p.domain || b.domain,
      })),
      outN: (b.outputs || []).length,
      inN: (b.inputs || []).length,
    };
  }
}

const sideOfFrame = (dx, dy) =>
  Math.abs(dx) >= Math.abs(dy) ? (dx < 0 ? 'left' : 'right') : dy > 0 ? 'top' : 'bottom';

function toSchematicBlock(b) {
  const g = GEO[b.type] || {
    width: 100,
    height: 80,
    domain: null,
    acausal: false,
    pins: [],
    outN: 0,
    inN: 0,
  };
  const pins = g.pins.map((p) => ({ ...p }));
  const frames = b.params && b.params.frames;
  if (frames && typeof frames === 'object') {
    for (const [name, off] of Object.entries(frames)) {
      if (pins.some((p) => p.name === name)) continue;
      const dx = Array.isArray(off) ? Number(off[0]) || 0 : 0;
      const dy = Array.isArray(off) ? Number(off[1]) || 0 : 0;
      pins.push({ name, side: sideOfFrame(dx, dy), domain: g.domain });
    }
  }
  return {
    id: b.id,
    type: b.type,
    acausal: g.acausal || pins.length > 0,
    width: g.width,
    height: g.height,
    pins,
    hasSignalOut: g.outN > 0,
    hasSignalIn: g.inN > 0,
  };
}

let changed = 0;
let mismatch = 0;
for (const file of fs.readdirSync(EXDIR).filter((f) => f.endsWith('.nflow'))) {
  const full = path.join(EXDIR, file);
  const model = JSON.parse(fs.readFileSync(full, 'utf8'));
  const sModel = {
    blocks: model.blocks.map(toSchematicBlock),
    physicalWires: model.physicalWires || [],
    connections: model.connections || [],
  };
  const pos = computeSchematicLayout(sModel);
  let touched = false;
  for (const b of model.blocks) {
    const p = pos.get(b.id);
    if (!p) continue;
    if (b.x !== p.x || b.y !== p.y || (b.rotation || 0) !== p.rotation) touched = true;
    b.x = p.x;
    b.y = p.y;
    b.rotation = p.rotation;
    delete b._dy;
    delete b._bary;
  }
  // Order each physical net's pins so the loader's spanning chain (pins[i-1]->
  // pins[i]) draws a clean rail. The physical-wire renderer only brackets pins
  // that share an EDGE side (e.g. every bottom pin of a return rail); mixing a
  // differently-sided pin (a ground tap points up) into the middle of the chain
  // breaks that and drops the segment to the general router, which detours. So
  // group by side, order each group along its rail (bottom/top by x, left/right
  // by y), and append the odd-one-out taps last. The net is the same set of pins
  // (union-find re-lifts it on save), so simulation is unaffected.
  const anchor = new Map();
  for (const b of model.blocks) {
    const p = pos.get(b.id);
    if (!p) continue;
    const g = GEO[b.type] || { width: 100, height: 80 };
    const rot = ((p.rotation % 360) + 360) % 360;
    anchor.set(b.id, { cx: p.x + g.width / 2, cy: p.y + g.height / 2, rot });
  }
  const pinInfo = (key) => {
    const id = key.slice(0, key.lastIndexOf('.'));
    const name = key.slice(key.lastIndexOf('.') + 1);
    const a = anchor.get(id) || { cx: 0, cy: 0, rot: 0 };
    const blk = model.blocks.find((b) => b.id === id);
    const sb = blk ? toSchematicBlock(blk) : null;
    const pin = sb && sb.pins.find((p) => p.name === name);
    // Rotate the base side by the block rotation to get the on-screen edge.
    const order = ['left', 'top', 'right', 'bottom'];
    let side = pin ? pin.side : 'left';
    if (pin) side = order[(order.indexOf(pin.side) + rotIndex(a.rot)) % 4];
    return { ...a, side };
  };
  function rotIndex(rot) {
    return Math.round((rot || 0) / 90) % 4;
  }
  const SIDE_PRIO = { bottom: 0, top: 1, left: 2, right: 3 };
  for (const net of model.physicalWires || []) {
    if (!Array.isArray(net.pins) || net.pins.length < 3) continue; // 2-pin = one wire
    // Which side dominates this net (the rail direction)? Ties resolve by a fixed
    // side priority so the ordering is deterministic (idempotent re-runs).
    const infos = net.pins.map((k) => ({ k, ...pinInfo(k) }));
    const bySide = {};
    for (const i of infos) bySide[i.side] = (bySide[i.side] || 0) + 1;
    const rail = Object.keys(bySide).sort(
      (a, b) => bySide[b] - bySide[a] || SIDE_PRIO[a] - SIDE_PRIO[b]
    )[0];
    const horiz = rail === 'top' || rail === 'bottom';
    net.pins = infos
      .sort((a, b) => {
        const am = a.side === rail ? 0 : 1;
        const bm = b.side === rail ? 0 : 1;
        // Taps (odd side, e.g. a ground pin pointing up) come first, chained to
        // the leftmost rail pin they sit under; then the rail pins in order.
        if (am !== bm) return bm - am;
        const primary = horiz ? a.cx - b.cx || a.cy - b.cy : a.cy - b.cy || a.cx - b.cx;
        return primary || (a.k < b.k ? -1 : a.k > b.k ? 1 : 0);
      })
      .map((i) => i.k);
  }
  // Match the repo's .nflow formatting (prettier, json parser) so format:check
  // stays green and the tool is idempotent.
  const out = await prettier.format(JSON.stringify(model), { parser: 'json' });
  if (check) {
    if (out !== fs.readFileSync(full, 'utf8')) {
      console.log('DIFF', file);
      mismatch += 1;
    }
  } else if (touched || out !== fs.readFileSync(full, 'utf8')) {
    fs.writeFileSync(full, out);
    console.log('wrote', file);
    changed += 1;
  }
}
if (check) {
  console.log(mismatch ? `${mismatch} file(s) out of date` : 'all example layouts up to date');
  process.exit(mismatch ? 1 : 0);
} else {
  console.log(`updated ${changed} file(s)`);
}
