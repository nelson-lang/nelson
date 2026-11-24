//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Export helpers: plain-text `.m`, HTML, Markdown, Word (.docx) and Jupyter.
import { zipSync, strToU8 } from 'fflate';
//=============================================================================
export function inlineText(block: any): string {
  const c = block?.content;
  if (!Array.isArray(c)) return '';
  return c.map((n: any) => (typeof n?.text === 'string' ? n.text : '')).join('');
}
//=============================================================================
function figuresOf(b: any): string[] {
  try {
    return JSON.parse(b.props?.figures || '[]');
  } catch {
    return [];
  }
}
//=============================================================================
function esc(s: string): string {
  return String(s).replace(
    /[<>&"]/g,
    (c) => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;', '"': '&quot;' }[c] ?? c)
  );
}
//=============================================================================
function controlLine(b: any): string {
  const p = b.props;
  if (b.type === 'checkbox') return `**${p.label}** (\`${p.varName}\` = ${p.checked ? 1 : 0})`;
  if (b.type === 'statebutton') return `**${p.label}** (\`${p.varName}\` = ${p.on ? 1 : 0})`;
  if (b.type === 'rangeslider') return `**${p.label}** (\`${p.varName}\` = [${p.low} ${p.high}])`;
  if (b.type === 'runbutton') return '';
  return `**${p.label}** (\`${p.varName}\` = ${p.value ?? ''})`;
}
//=============================================================================
const HTML_CSS = `
body{font-family:'Segoe UI',system-ui,sans-serif;max-width:820px;margin:32px auto;padding:0 20px;color:#1f1f1f;line-height:1.55}
h1{font-size:28px}h2{font-size:20px;border-top:1px solid #e0e0e0;padding-top:8px}h3{font-size:16px}
pre{background:#f6f8fa;border:1px solid #e6e6e6;border-radius:4px;padding:10px 12px;overflow:auto;font-family:Consolas,monospace;font-size:13px}
pre.out{background:#fbfcfd}
img{max-width:100%;border:1px solid #e6e6e6;border-radius:4px;margin:6px 0}
.ctl{background:#eaf3fb;border-left:3px solid #0072bd;padding:6px 10px;border-radius:4px;font-size:14px}
.eq{text-align:center;font-style:italic;font-family:Cambria,Georgia,serif;margin:10px 0}
`;
/** Standalone HTML document (also used for Word and Print/PDF export). */
export function blocksToHtml(blocks: any[], title = 'Notebook'): string {
  const parts: string[] = [];
  let list: 'ul' | 'ol' | null = null;
  const closeList = () => {
    if (list) {
      parts.push(`</${list}>`);
      list = null;
    }
  };
  for (const b of blocks) {
    if (b.type === 'bulletListItem' || b.type === 'numberedListItem') {
      const want = b.type === 'bulletListItem' ? 'ul' : 'ol';
      if (list !== want) {
        closeList();
        parts.push(`<${want}>`);
        list = want;
      }
      parts.push(`<li>${esc(inlineText(b))}</li>`);
      continue;
    }
    closeList();
    switch (b.type) {
      case 'heading': {
        const lvl = Math.min(3, Math.max(1, b.props?.level ?? 1));
        parts.push(`<h${lvl}>${esc(inlineText(b))}</h${lvl}>`);
        break;
      }
      case 'paragraph':
        parts.push(`<p>${esc(inlineText(b))}</p>`);
        break;
      case 'equation':
        parts.push(`<div class="eq">${esc(b.props?.latex ?? '')}</div>`);
        break;
      case 'codecell':
      case 'livetask': {
        const code = b.type === 'codecell' ? b.props.code : liveTaskCode(b);
        parts.push(`<pre class="code">${esc(code ?? '')}</pre>`);
        if (b.props.output) parts.push(`<pre class="out">${esc(b.props.output)}</pre>`);
        for (const src of figuresOf(b)) parts.push(`<img src="${src}" alt="figure"/>`);
        break;
      }
      case 'slider':
      case 'numberfield':
      case 'dropdown':
      case 'checkbox':
      case 'statebutton':
      case 'rangeslider':
      case 'textfield':
      case 'colorpicker':
      case 'datepicker':
      case 'filebrowser':
      case 'varselect': {
        const line = controlLine(b);
        if (line) parts.push(`<p class="ctl">${esc(line).replace(/&amp;lt;/g, '<')}</p>`);
        break;
      }
      default:
        if (inlineText(b)) parts.push(`<p>${esc(inlineText(b))}</p>`);
    }
  }
  closeList();
  return `<!doctype html><html><head><meta charset="utf-8"><title>${esc(
    title
  )}</title><style>${HTML_CSS}</style></head><body>${parts.join('\n')}</body></html>`;
}
//=============================================================================
export function blocksToMarkdown(blocks: any[]): string {
  const out: string[] = [];
  for (const b of blocks) {
    switch (b.type) {
      case 'heading':
        out.push('#'.repeat(b.props?.level ?? 1) + ' ' + inlineText(b));
        break;
      case 'paragraph':
        out.push(inlineText(b));
        break;
      case 'bulletListItem':
        out.push('- ' + inlineText(b));
        break;
      case 'numberedListItem':
        out.push('1. ' + inlineText(b));
        break;
      case 'equation':
        out.push('$$' + (b.props?.latex ?? '') + '$$');
        break;
      case 'codecell':
      case 'livetask': {
        const code = b.type === 'codecell' ? b.props.code : liveTaskCode(b);
        out.push('```matlab\n' + (code ?? '') + '\n```');
        if (b.props.output) out.push('```\n' + b.props.output + '\n```');
        for (const src of figuresOf(b)) out.push(`![figure](${src})`);
        break;
      }
      default: {
        const line = controlLine(b);
        if (line) out.push('> ' + line);
        else if (inlineText(b)) out.push(inlineText(b));
      }
    }
  }
  return out.join('\n\n') + '\n';
}
//=============================================================================
function liveTaskCode(b: any): string {
  const op = b.props?.op ?? 'plot';
  const v = b.props?.inputVar || 'x';
  const out = b.props?.outVar || 'result';
  return op === 'plot' || op === 'bar' || op === 'hist' ? `${op}(${v})` : `${out} = ${op}(${v})`;
}
//=============================================================================
function splitKeep(text: string): string[] {
  const lines = String(text ?? '').split('\n');
  return lines.map((l, i) => (i < lines.length - 1 ? l + '\n' : l));
}
//=============================================================================
function joinLines(lines: string[]): string[] {
  return splitKeep(lines.join('\n\n'));
}
//=============================================================================
// data:<mime>[;base64],<payload> -> an nbformat output.data entry.
function figureToOutput(dataUrl: string): any | null {
  const m = /^data:([^;,]+)(;base64)?,(.*)$/s.exec(dataUrl);
  if (!m) return null;
  const mime = m[1];
  const isB64 = Boolean(m[2]);
  const payload = isB64 ? m[3] : decodeURIComponent(m[3]);
  return {
    output_type: 'display_data',
    metadata: {},
    data: { [mime]: mime.startsWith('image/svg') ? splitKeep(payload) : payload },
  };
}
//=============================================================================
/** Convert the BlockNote document to a minimal, valid Jupyter notebook (nbformat 4.5). */
export function blocksToIpynb(blocks: any[]): string {
  const cells: any[] = [];
  let cellId = 0;
  const nextId = () => `cell-${cellId++}`;
  let md: string[] = [];
  const flushMd = () => {
    if (md.length) {
      cells.push({ id: nextId(), cell_type: 'markdown', metadata: {}, source: joinLines(md) });
      md = [];
    }
  };

  for (const b of blocks) {
    switch (b.type) {
      case 'codecell': {
        flushMd();
        const outputs: any[] = [];
        if (b.props.output)
          outputs.push({ output_type: 'stream', name: 'stdout', text: splitKeep(b.props.output) });
        let figures: string[] = [];
        try {
          figures = JSON.parse(b.props.figures || '[]');
        } catch {
          figures = [];
        }
        for (const src of figures) {
          const out = figureToOutput(src);
          if (out) outputs.push(out);
        }
        cells.push({
          id: nextId(),
          cell_type: 'code',
          metadata: {},
          execution_count: null,
          outputs,
          source: splitKeep(b.props.code ?? ''),
        });
        break;
      }
      case 'heading':
        md.push('#'.repeat(b.props?.level ?? 1) + ' ' + inlineText(b));
        break;
      case 'paragraph':
        md.push(inlineText(b));
        break;
      case 'bulletListItem':
        md.push('- ' + inlineText(b));
        break;
      case 'numberedListItem':
        md.push('1. ' + inlineText(b));
        break;
      case 'equation':
        md.push('$$' + (b.props?.latex ?? '') + '$$');
        break;
      case 'slider':
      case 'numberfield':
      case 'rangeslider':
      case 'dropdown':
        md.push(`> **${b.props.label}** (\`${b.props.varName}\` = ${b.props.value ?? ''})`);
        break;
      case 'checkbox':
        md.push(`> **${b.props.label}** (\`${b.props.varName}\` = ${b.props.checked ? 1 : 0})`);
        break;
      case 'statebutton':
        md.push(`> **${b.props.label}** (\`${b.props.varName}\` = ${b.props.on ? 1 : 0})`);
        break;
      default:
        md.push(inlineText(b));
    }
  }
  flushMd();

  return JSON.stringify(
    {
      cells,
      metadata: {
        kernelspec: { name: 'nelson', display_name: 'Nelson', language: 'nelson' },
        language_info: { name: 'nelson', file_extension: '.m' },
      },
      nbformat: 4,
      nbformat_minor: 5,
    },
    null,
    1
  );
}
//=============================================================================
// Plain-text .m Live Code format. The file is a valid Nelson script (code runs,
// everything else is a comment) and round-trips back into the document. Each
// block starts with a `%% [kind]` marker.
function escXml(s: string): string {
  return String(s).replace(/[<>&]/g, (c) => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;' }[c] ?? c));
}
//=============================================================================
function stripOutputs(props: Record<string, unknown>): Record<string, unknown> {
  const p = { ...props };
  delete p.output;
  delete p.figures;
  delete p.errorMsg;
  delete p.figureRefs;
  return p;
}
//=============================================================================
export function blocksToM(blocks: any[]): string {
  const lines: string[] = [];
  for (const b of blocks) {
    switch (b.type) {
      case 'heading':
        lines.push(`%% [heading:${b.props?.level ?? 1}] ${inlineText(b)}`);
        break;
      case 'paragraph':
      case 'bulletListItem':
      case 'numberedListItem':
        lines.push('%% [text]');
        for (const ln of inlineText(b).split('\n')) lines.push(`% ${ln}`);
        break;
      case 'equation':
        lines.push(`%% [equation] ${b.props?.latex ?? ''}`);
        break;
      case 'codecell':
        lines.push('%% [code]');
        for (const ln of String(b.props.code ?? '').split('\n')) lines.push(ln);
        break;
      default:
        // controls, live tasks, etc.: encode losslessly as a block marker.
        lines.push(
          `%% [block] ${JSON.stringify({ type: b.type, props: stripOutputs(b.props ?? {}) })}`
        );
    }
    lines.push('');
  }
  return lines.join('\n');
}
//=============================================================================
export function mToBlocks(text: string): any[] {
  const src = text.replace(/\r\n/g, '\n').split('\n');
  const blocks: any[] = [];
  let i = 0;
  const marker = /^%%\s*\[(\w+)(?::(\d+))?\]\s?(.*)$/;
  while (i < src.length) {
    const m = marker.exec(src[i]);
    if (!m) {
      i++;
      continue;
    }
    const kind = m[1];
    const arg = m[3];
    i++;
    if (kind === 'heading') {
      blocks.push({ type: 'heading', props: { level: Number(m[2] || 1) }, content: arg });
    } else if (kind === 'equation') {
      blocks.push({ type: 'equation', props: { latex: arg } });
    } else if (kind === 'block') {
      try {
        blocks.push(JSON.parse(arg));
      } catch {
        /* skip malformed */
      }
    } else if (kind === 'text') {
      const parts: string[] = [];
      while (i < src.length && !marker.test(src[i])) {
        const ln = src[i];
        if (ln.startsWith('% ')) parts.push(ln.slice(2));
        else if (ln === '%') parts.push('');
        else if (ln.trim() !== '') parts.push(ln);
        i++;
      }
      blocks.push({ type: 'paragraph', content: parts.join('\n').replace(/\n+$/, '') });
    } else if (kind === 'code') {
      const parts: string[] = [];
      while (i < src.length && !marker.test(src[i])) {
        parts.push(src[i]);
        i++;
      }
      blocks.push({ type: 'codecell', props: { code: parts.join('\n').replace(/\n+$/, '') } });
    }
  }
  return blocks;
}
//=============================================================================
/** True when a `.m` is a Notebook saved by Nelson (has `%% [kind]` markers). */
export function isMarkedLiveM(text: string): boolean {
  return /^\s*%%\s*\[\w+/m.test(text);
}
/**
 * Convert a plain Nelson `.m` script into Notebook blocks ("open as Live
 * Script"): `%% Title` becomes a section heading, `%` comment lines become
 * formatted text, and runs of code become code cells.
 */
export function mScriptToBlocks(text: string): any[] {
  const src = text.replace(/\r\n/g, '\n').split('\n');
  const blocks: any[] = [];
  let code: string[] = [];
  let txt: string[] = [];
  const flushCode = () => {
    const c = code.join('\n').replace(/^\n+|\n+$/g, '');
    if (c.trim()) blocks.push({ type: 'codecell', props: { code: c } });
    code = [];
  };
  const flushText = () => {
    const t = txt.join('\n').replace(/^\n+|\n+$/g, '');
    if (t.trim()) blocks.push({ type: 'paragraph', content: t });
    txt = [];
  };
  for (const line of src) {
    if (/^\s*%%/.test(line)) {
      flushText();
      flushCode();
      const title = line.replace(/^\s*%%\s?/, '').trim();
      if (title) blocks.push({ type: 'heading', props: { level: 2 }, content: title });
    } else if (/^\s*%/.test(line)) {
      flushCode();
      txt.push(line.replace(/^\s*%\s?/, ''));
    } else if (line.trim() === '') {
      if (code.length) code.push('');
      else flushText();
    } else {
      flushText();
      code.push(line);
    }
  }
  flushText();
  flushCode();
  return blocks;
}
//=============================================================================
// Native Word (.docx): a valid OOXML package built with fflate. Text, headings
// and code render as paragraphs; inline PNG figures are embedded as drawings.
function docxParagraph(
  text: string,
  opts: { bold?: boolean; size?: number; mono?: boolean } = {}
): string {
  const rpr =
    (opts.bold ? '<w:b/>' : '') +
    (opts.size ? `<w:sz w:val="${opts.size}"/>` : '') +
    (opts.mono ? '<w:rFonts w:ascii="Consolas" w:hAnsi="Consolas"/>' : '');
  const rprXml = rpr ? `<w:rPr>${rpr}</w:rPr>` : '';
  return `<w:p><w:r>${rprXml}<w:t xml:space="preserve">${escXml(text)}</w:t></w:r></w:p>`;
}
//=============================================================================
function b64ToBytes(b64: string): Uint8Array {
  const bin = atob(b64);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}
//=============================================================================
// PNG IHDR holds width/height as big-endian uint32 at bytes 16 and 20.
function pngSize(bytes: Uint8Array): { w: number; h: number } {
  if (bytes.length < 24) return { w: 480, h: 320 };
  const u32 = (o: number) =>
    (bytes[o] << 24) | (bytes[o + 1] << 16) | (bytes[o + 2] << 8) | bytes[o + 3];
  return { w: u32(16) >>> 0, h: u32(20) >>> 0 };
}
//=============================================================================
const EMU_PER_PX = 9525;
const MAX_WIDTH_EMU = 5486400; // ~6 inches
function docxDrawing(rId: string, picId: number, wEmu: number, hEmu: number): string {
  return (
    '<w:p><w:r><w:drawing>' +
    '<wp:inline distT="0" distB="0" distL="0" distR="0" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing">' +
    `<wp:extent cx="${wEmu}" cy="${hEmu}"/><wp:docPr id="${picId}" name="Figure ${picId}"/>` +
    '<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">' +
    '<pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">' +
    `<pic:nvPicPr><pic:cNvPr id="${picId}" name="fig${picId}.png"/><pic:cNvPicPr/></pic:nvPicPr>` +
    `<pic:blipFill><a:blip r:embed="${rId}" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>` +
    `<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="${wEmu}" cy="${hEmu}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>` +
    '</pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p>'
  );
}
//=============================================================================
export function blocksToDocx(blocks: any[]): Uint8Array {
  const body: string[] = [];
  const media: Record<string, Uint8Array> = {};
  const rels: string[] = [
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>',
  ];
  const docRels: string[] = [];
  let mediaSeq = 0;

  const addFigures = (b: any) => {
    for (const src of figuresOf(b)) {
      const m = /^data:image\/png;base64,(.*)$/.exec(src);
      if (!m) continue; // only PNG figures embed in docx
      const bytes = b64ToBytes(m[1]);
      const n = ++mediaSeq;
      const name = `media/fig${n}.png`;
      media[`word/${name}`] = bytes;
      const rId = `rIdImg${n}`;
      docRels.push(
        `<Relationship Id="${rId}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="${name}"/>`
      );
      const { w, h } = pngSize(bytes);
      let wEmu = w * EMU_PER_PX;
      let hEmu = h * EMU_PER_PX;
      if (wEmu > MAX_WIDTH_EMU) {
        hEmu = Math.round((hEmu * MAX_WIDTH_EMU) / wEmu);
        wEmu = MAX_WIDTH_EMU;
      }
      body.push(docxDrawing(rId, n, wEmu, hEmu));
    }
  };

  for (const b of blocks) {
    switch (b.type) {
      case 'heading':
        body.push(
          docxParagraph(inlineText(b), { bold: true, size: 34 - (b.props?.level ?? 1) * 4 })
        );
        break;
      case 'paragraph':
        body.push(docxParagraph(inlineText(b)));
        break;
      case 'equation':
        body.push(docxParagraph(b.props?.latex ?? '', { mono: true }));
        break;
      case 'codecell':
      case 'livetask':
        for (const ln of String(b.props.code ?? '').split('\n'))
          if (b.type === 'codecell') body.push(docxParagraph(ln || ' ', { mono: true }));
        if (b.props.output)
          for (const ln of String(b.props.output).split('\n'))
            body.push(docxParagraph(ln || ' ', { mono: true }));
        addFigures(b);
        break;
      default: {
        const t = inlineText(b);
        if (t) body.push(docxParagraph(t));
      }
    }
  }

  const documentXml =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">' +
    `<w:body>${body.join('')}<w:sectPr/></w:body></w:document>`;
  const contentTypes =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
    '<Default Extension="xml" ContentType="application/xml"/>' +
    '<Default Extension="png" ContentType="image/png"/>' +
    '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>' +
    '</Types>';
  const rootRels =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">${rels.join(
      ''
    )}</Relationships>`;
  const documentRels =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">${docRels.join(
      ''
    )}</Relationships>`;

  const files: Record<string, Uint8Array> = {
    '[Content_Types].xml': strToU8(contentTypes),
    '_rels/.rels': strToU8(rootRels),
    'word/document.xml': strToU8(documentXml),
    'word/_rels/document.xml.rels': strToU8(documentRels),
    ...media,
  };
  return zipSync(files);
}
//=============================================================================
// Jupyter import: a nbformat notebook back into Notebook blocks.
function joinSrc(s: unknown): string {
  return Array.isArray(s) ? (s as string[]).join('') : String(s ?? '');
}
//=============================================================================
export function ipynbToBlocks(text: string): any[] | null {
  let nb: any;
  try {
    nb = JSON.parse(text);
  } catch {
    return null;
  }
  if (!nb || !Array.isArray(nb.cells)) return null;
  const blocks: any[] = [];
  for (const cell of nb.cells) {
    const src = joinSrc(cell.source);
    if (cell.cell_type === 'code') {
      let output = '';
      const figs: string[] = [];
      for (const o of cell.outputs ?? []) {
        if (o.output_type === 'stream') output += joinSrc(o.text);
        else if (o.output_type === 'error') output += (o.evalue ? String(o.evalue) : '') + '\n';
        else if (
          (o.output_type === 'display_data' || o.output_type === 'execute_result') &&
          o.data
        ) {
          if (o.data['image/png'])
            figs.push('data:image/png;base64,' + joinSrc(o.data['image/png']).replace(/\s+/g, ''));
          else if (o.data['image/svg+xml'])
            figs.push('data:image/svg+xml,' + encodeURIComponent(joinSrc(o.data['image/svg+xml'])));
          else if (o.data['text/plain']) output += joinSrc(o.data['text/plain']);
        }
      }
      blocks.push({
        type: 'codecell',
        props: {
          code: src.replace(/\n$/, ''),
          output: output.replace(/\s+$/, ''),
          figures: JSON.stringify(figs),
        },
      });
    } else {
      for (const line of src.split('\n')) {
        const h = /^(#{1,6})\s+(.*)$/.exec(line);
        if (h)
          blocks.push({
            type: 'heading',
            props: { level: Math.min(3, h[1].length) },
            content: h[2],
          });
        else if (line.trim()) blocks.push({ type: 'paragraph', content: line });
      }
    }
  }
  return blocks;
}
//=============================================================================
