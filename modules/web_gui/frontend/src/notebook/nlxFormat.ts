//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// .nlx is the native Notebook document: a zip (OPC-style) container.
//
//   manifest.json   - the format marker { format, version }
//   document.json   - the SOURCE: the ordered blocks, each code cell holding
//                     only its code (no results). This part stays small and
//                     diffs cleanly, and the headless reader parses it directly.
//   output.json     - the saved OUTPUT: per code cell, its last text output,
//                     error and figure references, keyed by block id. It is
//                     regenerable by re-running the document, and kept apart so
//                     it never pollutes the source.
//   media/figN.ext  - the figures a code cell produced, extracted so the JSON
//                     parts stay small and images round-trip losslessly.
import { zipSync, unzipSync, strToU8, strFromU8 } from 'fflate';
//=============================================================================
export const NLX_FORMAT = 'nelson-notebook';
export const NLX_VERSION = 2;
// Code-cell props that are run results, not source: they live in output.json
// (or are re-established at runtime), never in the document.json source.
const RUNTIME_PROP_KEYS = [
  'output',
  'figures',
  'figureRefs',
  'figureHandles',
  'runRevision',
  'errorMsg',
  'stale',
];
//=============================================================================
interface FigureRef {
  path: string;
  mime: string;
  base64: boolean;
}
//=============================================================================
function base64ToBytes(b64: string): Uint8Array {
  const bin = atob(b64);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}
//=============================================================================
function bytesToBase64(bytes: Uint8Array): string {
  let bin = '';
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin);
}
//=============================================================================
// A stable key tying a block's saved output to it: its own id, or its position.
function blockKey(b: any, index: number): string {
  return typeof b?.id === 'string' && b.id ? b.id : `#${index}`;
}
//=============================================================================
function extractFigures(
  figuresJson: string,
  media: Record<string, Uint8Array>,
  nextSeq: () => number
): FigureRef[] {
  let figures: string[] = [];
  try {
    figures = JSON.parse(figuresJson || '[]');
  } catch {
    figures = [];
  }
  const refs: FigureRef[] = [];
  for (const dataUrl of figures) {
    const m = /^data:([^;,]+)(;base64)?,([\s\S]*)$/.exec(dataUrl);
    if (!m) continue;
    const mime = m[1];
    const isB64 = Boolean(m[2]);
    const ext = mime.includes('svg') ? 'svg' : mime.split('/')[1] || 'bin';
    const path = `media/fig${nextSeq()}.${ext}`;
    media[path] = isB64 ? base64ToBytes(m[3]) : strToU8(decodeURIComponent(m[3]));
    refs.push({ path, mime, base64: isB64 });
  }
  return refs;
}
//=============================================================================
function refsToDataUrls(refs: FigureRef[], files: Record<string, Uint8Array>): string[] {
  const urls: string[] = [];
  for (const ref of refs ?? []) {
    const data = files[ref.path];
    if (!data) continue;
    urls.push(
      ref.mime.includes('svg')
        ? `data:${ref.mime},${encodeURIComponent(strFromU8(data))}`
        : `data:${ref.mime};base64,${bytesToBase64(data)}`
    );
  }
  return urls;
}
//=============================================================================
export function serializeNlx(blocks: any[]): Uint8Array {
  const media: Record<string, Uint8Array> = {};
  let seq = 0;
  const nextSeq = () => seq++;
  const sourceBlocks: any[] = [];
  const outputs: Record<string, unknown> = {};

  blocks.forEach((b, index) => {
    if (b.type !== 'codecell') {
      sourceBlocks.push(b);
      return;
    }
    const sourceProps: Record<string, unknown> = { ...b.props };
    for (const k of RUNTIME_PROP_KEYS) delete sourceProps[k];
    sourceBlocks.push({ ...b, props: sourceProps });

    const refs = extractFigures(b.props?.figures, media, nextSeq);
    const out: Record<string, unknown> = {};
    if (b.props?.output) out.output = b.props.output;
    if (b.props?.errorMsg) out.errorMsg = b.props.errorMsg;
    if (refs.length) out.figureRefs = refs;
    if (Object.keys(out).length) outputs[blockKey(b, index)] = out;
  });

  const files: Record<string, Uint8Array> = {
    'manifest.json': strToU8(JSON.stringify({ format: NLX_FORMAT, version: NLX_VERSION })),
    'document.json': strToU8(JSON.stringify({ blocks: sourceBlocks }, null, 1)),
    'output.json': strToU8(JSON.stringify({ outputs }, null, 1)),
    ...media,
  };
  return zipSync(files, { level: 6 });
}
//=============================================================================
// Accepts a zip (source/output split, or the earlier single-document zip) or a
// plain-JSON .nlx (the earliest v1).
export function parseNlx(bytes: Uint8Array): any[] | null {
  try {
    const files = unzipSync(bytes);
    const docFile = files['document.json'];
    if (docFile) {
      const doc = JSON.parse(strFromU8(docFile));
      const blocks: any[] = Array.isArray(doc.blocks) ? doc.blocks : [];
      const outFile = files['output.json'];
      if (outFile) {
        // Source/output split: merge each cell's saved output back in.
        let outputs: Record<string, any> = {};
        try {
          outputs = JSON.parse(strFromU8(outFile))?.outputs ?? {};
        } catch {
          outputs = {};
        }
        return blocks.map((b, index) => mergeOutput(b, outputs[blockKey(b, index)], files));
      }
      // Legacy single-document zip: output lives in the block props.
      return blocks.map((b) => restoreFigures(b, files));
    }
  } catch {
    /* not a zip, fall through to JSON */
  }
  try {
    const doc = JSON.parse(strFromU8(bytes));
    if (doc && Array.isArray(doc.blocks)) return doc.blocks;
    if (Array.isArray(doc)) return doc;
  } catch {
    /* not valid */
  }
  return null;
}
//=============================================================================
function mergeOutput(b: any, out: any, files: Record<string, Uint8Array>): any {
  if (b.type !== 'codecell') return b;
  const urls = refsToDataUrls(out?.figureRefs, files);
  return {
    ...b,
    props: {
      ...b.props,
      output: out?.output ?? '',
      errorMsg: out?.errorMsg ?? '',
      figures: JSON.stringify(urls),
    },
  };
}
//=============================================================================
// Legacy single-document zip: the block already carries its output; just turn
// its figure refs back into data URLs.
function restoreFigures(b: any, files: Record<string, Uint8Array>): any {
  if (b.type !== 'codecell' || !b.props?.figureRefs) return b;
  let refs: FigureRef[] = [];
  try {
    refs = JSON.parse(b.props.figureRefs);
  } catch {
    refs = [];
  }
  const props = { ...b.props, figures: JSON.stringify(refsToDataUrls(refs, files)) };
  delete props.figureRefs;
  return { ...b, props };
}
//=============================================================================
