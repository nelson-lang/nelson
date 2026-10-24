//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DrawPrimitive, TreeNode, UIControl } from '../rpc/types';
import { buttonLabelAnchor } from './buttonLabelPlacement';
import {
  autoTicks,
  dialFraction,
  dialPoint,
  withinLimits,
  FRAME_COLOR,
  GAUGE_NEEDLE,
  KNOB_POINTER,
  DISCRETE_POINTER,
  TICK_COLOR,
  TRACK_COLOR,
  SWITCH_TRACK,
} from './uiScaleGeometry';
import {
  decodeMarkerBatch,
  decodePointPairs,
  decodeRectBatch,
  markerGeometry,
} from './figureMarkers';
//=============================================================================
// Serialize a figure's display list (the same 2D primitives FigureCanvas
// replays, produced by the Qt-faithful RenderWeb projection) plus its
// uicontrols to a standalone SVG. This gives a vector export equivalent to the
// Qt desktop's "export SVG", entirely client-side (no server round-trip, no
// Qt). uicontrols, which are interactive HTML overlays at run time, are drawn
// as static SVG so the exported picture matches what is on screen.
const escapeXml = (s: string): string =>
  s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const textAnchor = (align: 'left' | 'center' | 'right'): string =>
  align === 'center' ? 'middle' : align === 'right' ? 'end' : 'start';
function primitiveToSvg(p: DrawPrimitive): string {
  switch (p.kind) {
    case 'rect':
      return `<rect x="${p.x}" y="${p.y}" width="${p.w}" height="${p.h}" fill="${p.fill}"/>`;
    case 'line': {
      const dash = p.dash && p.dash.length ? ` stroke-dasharray="${p.dash.join(',')}"` : '';
      return (
        `<line x1="${p.x1}" y1="${p.y1}" x2="${p.x2}" y2="${p.y2}" ` +
        `stroke="${p.stroke}" stroke-width="${p.width}"${dash}/>`
      );
    }
    case 'segments': {
      const points = decodePointPairs(p.pointsB64, p.count);
      const dash = p.dash && p.dash.length ? ` stroke-dasharray="${p.dash.join(',')}"` : '';
      let out = '';
      for (let i = 0; i + 1 < points.length; i += 2) {
        const a = points[i];
        const b = points[i + 1];
        out +=
          `<line x1="${a[0]}" y1="${a[1]}" x2="${b[0]}" y2="${b[1]}" ` +
          `stroke="${p.stroke}" stroke-width="${p.width}"${dash}/>`;
      }
      return out;
    }
    case 'polyline': {
      const pts = decodePointPairs(p.pointsB64, p.count)
        .map(([x, y]) => `${x},${y}`)
        .join(' ');
      const dash = p.dash && p.dash.length ? ` stroke-dasharray="${p.dash.join(',')}"` : '';
      return (
        `<polyline points="${pts}" fill="none" stroke="${p.stroke}" ` +
        `stroke-width="${p.width}"${dash}/>`
      );
    }
    case 'polygon': {
      const pts = decodePointPairs(p.pointsB64, p.count)
        .map(([x, y]) => `${x},${y}`)
        .join(' ');
      const stroke = p.stroke ? ` stroke="${p.stroke}" stroke-width="${p.width ?? 1}"` : '';
      return `<polygon points="${pts}" fill="${p.fill}"${stroke}/>`;
    }
    case 'image':
      return (
        `<image x="${p.x}" y="${p.y}" width="${p.w}" height="${p.h}" ` +
        `preserveAspectRatio="none" href="${p.image}"/>`
      );
    case 'text': {
      const attrs =
        `fill="${p.color}" font-size="${p.size}" font-family="${p.font ?? 'sans-serif'}"` +
        `${p.bold ? ' font-weight="bold"' : ''}${p.italic ? ' font-style="italic"' : ''} ` +
        `text-anchor="${textAnchor(p.align)}"`;
      if (p.rotation) {
        return (
          `<text x="0" y="0" ${attrs} ` +
          `transform="translate(${p.x},${p.y}) rotate(${-p.rotation})">${escapeXml(p.text)}</text>`
        );
      }
      return `<text x="${p.x}" y="${p.y}" ${attrs}>${escapeXml(p.text)}</text>`;
    }
    case 'points': {
      // Marker batch -> one SVG shape per point (see figureMarkers for the packing
      // and geometry). SVG export favours fidelity over payload size.
      const points = decodeMarkerBatch(p.data, p.count, p.hasFace, p.hasEdge);
      if (!points) {
        return '';
      }
      const ew = p.edgeWidth || 1;
      let out = '';
      for (const pt of points) {
        if (pt.r <= 0) continue;
        const fillAttr = pt.fill ? `fill="${pt.fill}"` : 'fill="none"';
        const strokeAttr = pt.edge ? ` stroke="${pt.edge}" stroke-width="${ew}"` : '';
        const geom = markerGeometry(p.shape, pt.x, pt.y, pt.r);
        if (geom.kind === 'disc') {
          out += `<circle cx="${pt.x}" cy="${pt.y}" r="${pt.r}" ${fillAttr}${strokeAttr}/>`;
        } else if (geom.kind === 'polygon') {
          const pts = geom.points.map(([x, y]) => `${x},${y}`).join(' ');
          out += `<polygon points="${pts}" ${fillAttr}${strokeAttr}/>`;
        } else {
          const color = pt.edge ?? pt.fill ?? '#000';
          for (const s of geom.segments) {
            out +=
              `<line x1="${s[0]}" y1="${s[1]}" x2="${s[2]}" y2="${s[3]}" ` +
              `stroke="${color}" stroke-width="${ew}"/>`;
          }
        }
      }
      return out;
    }
    case 'rects': {
      // Rectangle batch (bar/histogram) -> one <rect> per bar, shared fill/outline.
      const rects = decodeRectBatch(p.data, p.count);
      if (!rects) {
        return '';
      }
      const fillAttr = p.fill ? `fill="${p.fill}"` : 'fill="none"';
      const strokeAttr = p.stroke ? ` stroke="${p.stroke}" stroke-width="${p.width || 1}"` : '';
      let out = '';
      for (const [x, y, w, h] of rects) {
        out += `<rect x="${x}" y="${y}" width="${w}" height="${h}" ${fillAttr}${strokeAttr}/>`;
      }
      return out;
    }
    case 'clip':
    case 'clipSave':
    case 'clipRestore':
      // SVG export is unclipped for now (would need <clipPath> grouping); the
      // on-screen canvas honours the clip. Skip the markers here.
      return '';
  }
}
//=============================================================================
// A static SVG rendering of a uicontrol, mirroring how FigureControls draws it.
// Exported so the live widgets (FigureControls) can reuse the exact same face as
// the SVG export; callers pass a control with x=0/y=0 to draw in local coords.
export function controlToSvg(c: UIControl): string {
  const { x, y, w, h } = c;
  const cx = x + w / 2;
  const cy = y + h / 2;
  const fs = c.fontSize || 12;
  const fg = c.foreground || '#000000';
  const label = (tx: number, anchor: string, txt: string, color = fg): string =>
    txt
      ? `<text x="${tx}" y="${cy}" font-size="${fs}" font-family="sans-serif" ` +
        `text-anchor="${anchor}" dominant-baseline="central" fill="${color}">${escapeXml(
          txt
        )}</text>`
      : '';

  switch (c.style) {
    case 'pushbutton':
    case 'togglebutton': {
      const pressed = c.style === 'togglebutton' && c.value >= c.max;
      const lines = c.text ? c.text.split('\n') : [];
      const lineHeight = fs * 1.2;
      const at = buttonLabelAnchor(c, lines.length, lineHeight);
      const text = lines
        .map(
          (line, k) =>
            `<text x="${at.x}" y="${at.y + k * lineHeight}" font-size="${fs}" ` +
            `font-family="sans-serif" text-anchor="${at.anchor}" dominant-baseline="central" ` +
            `fill="${fg}">${escapeXml(line)}</text>`
        )
        .join('');
      return (
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="3" ` +
        `fill="${pressed ? '#cfcfcf' : '#e8e8e8'}" stroke="#8f8f8f"/>` +
        text
      );
    }
    case 'text':
      return label(x + 2, 'start', c.text);
    case 'spinner': {
      // A numeric field with its value and an up/down stepper column, like the
      // Qt widget: value right-aligned against the steppers.
      const stepperW = Math.min(18, w * 0.3);
      const sx = x + w - stepperW;
      const midY = y + h / 2;
      const acx = sx + stepperW / 2;
      return (
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" fill="#ffffff" stroke="#8f8f8f"/>` +
        `<text x="${
          sx - 6
        }" y="${cy}" font-size="${fs}" font-family="sans-serif" text-anchor="end" ` +
        `dominant-baseline="central" fill="${fg}">${escapeXml(c.text)}</text>` +
        `<line x1="${sx}" y1="${y + 2}" x2="${sx}" y2="${y + h - 2}" stroke="rgb(200,200,200)"/>` +
        `<line x1="${sx}" y1="${midY}" x2="${x + w}" y2="${midY}" stroke="rgb(200,200,200)"/>` +
        `<polygon points="${acx - 4},${y + h * 0.33} ${acx + 4},${y + h * 0.33} ${acx},${
          y + h * 0.19
        }" fill="rgb(110,110,110)"/>` +
        `<polygon points="${acx - 4},${y + h * 0.67} ${acx + 4},${y + h * 0.67} ${acx},${
          y + h * 0.81
        }" fill="rgb(110,110,110)"/>`
      );
    }
    case 'edit':
    case 'date': {
      const box = `<rect x="${x}" y="${y}" width="${w}" height="${h}" fill="#ffffff" stroke="#8f8f8f"/>`;
      // A multi-line uitextarea Value lays out top-down; a single line is centred.
      if (c.text && c.text.includes('\n')) {
        const lineHeight = fs * 1.35;
        const lines = c.text
          .split('\n')
          .map(
            (ln, k) =>
              `<text x="${x + 4}" y="${y + 4 + fs + k * lineHeight}" font-size="${fs}" ` +
              `font-family="sans-serif" fill="${fg}">${escapeXml(ln)}</text>`
          )
          .join('');
        return box + lines;
      }
      return box + label(x + 4, 'start', c.text);
    }
    case 'checkbox': {
      const bs = Math.min(h - 2, 14);
      const bx = x + 1;
      const by = cy - bs / 2;
      const check =
        c.value >= c.max
          ? `<path d="M${bx + 2},${by + bs * 0.5} L${bx + bs * 0.4},${by + bs * 0.82} ` +
            `L${bx + bs - 2},${by + bs * 0.18}" fill="none" stroke="#333333" stroke-width="2"/>`
          : '';
      return (
        `<rect x="${bx}" y="${by}" width="${bs}" height="${bs}" fill="#ffffff" stroke="#666666"/>` +
        check +
        label(bx + bs + 4, 'start', c.text)
      );
    }
    case 'radiobutton': {
      const r = Math.min(h - 2, 14) / 2;
      const rx = x + 1 + r;
      const dot =
        c.value >= c.max ? `<circle cx="${rx}" cy="${cy}" r="${r * 0.5}" fill="#333333"/>` : '';
      return (
        `<circle cx="${rx}" cy="${cy}" r="${r}" fill="#ffffff" stroke="#666666"/>` +
        dot +
        label(rx + r + 4, 'start', c.text)
      );
    }
    case 'slider': {
      const major = c.data?.major ?? [];
      const minor = c.data?.minor ?? [];
      const labels = c.data?.labels ?? [];
      const vertical = c.data?.vertical ?? false;
      const range = c.data?.range;
      const frac = (v: number): number => dialFraction(v, c.min, c.max);
      const out: string[] = [];
      const line = (
        x1: number,
        y1: number,
        x2: number,
        y2: number,
        s: string,
        wd: number
      ): string =>
        `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${s}" stroke-width="${wd}"/>`;
      const thumb = (tx: number, ty: number): string =>
        `<rect x="${tx - 5}" y="${ty - 4}" width="10" height="8" fill="rgb(240,240,240)" ` +
        `stroke="rgb(100,100,100)"/>`;
      if (vertical) {
        const vx = x + 2;
        const posY = (v: number): number => y + (1 - frac(v)) * h;
        out.push(line(vx, y, vx, y + h, TRACK_COLOR, 3));
        if (range) out.push(line(vx, posY(range[0]), vx, posY(range[1]), TRACK_COLOR, 3));
        for (const t of minor) {
          if (withinLimits(t, c.min, c.max))
            out.push(line(vx + 4, posY(t), vx + 7, posY(t), TRACK_COLOR, 1));
        }
        major.forEach((t, k) => {
          if (!withinLimits(t, c.min, c.max)) return;
          const yy = posY(t);
          out.push(line(vx + 4, yy, vx + 10, yy, TRACK_COLOR, 1));
          if (k < labels.length) {
            out.push(
              `<text x="${vx + 12 + fs}" y="${yy + fs * 0.35}" font-size="${fs}" ` +
                `font-family="sans-serif" fill="${fg}">${escapeXml(labels[k])}</text>`
            );
          }
        });
        if (range) {
          out.push(thumb(vx, posY(range[0])));
          out.push(thumb(vx, posY(range[1])));
        } else {
          out.push(thumb(vx, posY(c.value)));
        }
        return out.join('');
      }
      const trackY = y - 10;
      const posX = (v: number): number => x + frac(v) * w;
      out.push(line(x, trackY, x + w, trackY, TRACK_COLOR, 3));
      if (range) {
        out.push(
          line(
            posX(Math.min(range[0], range[1])),
            trackY,
            posX(Math.max(range[0], range[1])),
            trackY,
            TRACK_COLOR,
            3
          )
        );
      }
      for (const t of minor) {
        if (!withinLimits(t, c.min, c.max)) continue;
        const xx = posX(t);
        out.push(line(xx, trackY + 4, xx, trackY + 7, TRACK_COLOR, 1));
      }
      major.forEach((t, k) => {
        if (!withinLimits(t, c.min, c.max)) return;
        const xx = posX(t);
        out.push(line(xx, trackY + 4, xx, trackY + 10, TRACK_COLOR, 1));
        if (k < labels.length) {
          out.push(
            `<text x="${xx}" y="${trackY + 11 + fs * 0.9}" font-size="${fs}" ` +
              `font-family="sans-serif" text-anchor="middle" fill="${fg}">${escapeXml(
                labels[k]
              )}</text>`
          );
        }
      });
      if (range) {
        out.push(thumb(posX(range[0]), trackY));
        out.push(thumb(posX(range[1]), trackY));
      } else {
        out.push(thumb(posX(c.value), trackY));
      }
      return out.join('');
    }
    case 'popupmenu':
    case 'listbox':
      return (
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" fill="#ffffff" stroke="#8f8f8f"/>` +
        (c.text || c.items?.[0]
          ? `<text x="${x + 4}" y="${y + Math.min(h / 2, fs + 2)}" font-size="${fs}" ` +
            `font-family="sans-serif" fill="${fg}">${escapeXml(c.text || c.items![0])}</text>`
          : '')
      );
    case 'knob': {
      const major = c.data?.major ?? [];
      const minor = c.data?.minor ?? [];
      const labels = c.data?.labels ?? [];
      const frac = (v: number): number => dialFraction(v, c.min, c.max);
      const out: string[] = [];
      const line = (
        x1: number,
        y1: number,
        x2: number,
        y2: number,
        s: string,
        wd: number
      ): string =>
        `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${s}" stroke-width="${wd}"/>`;
      const centered = (lx: number, ly: number, txt: string): string =>
        `<text x="${lx}" y="${ly + fs * 0.35}" font-size="${fs}" font-family="sans-serif" ` +
        `text-anchor="middle" fill="${fg}">${escapeXml(txt)}</text>`;
      if (c.data?.discrete) {
        // Qt expands the paint area around the authored knob face by 60 pixels.
        const outerR = Math.min(w, h) / 2 + 60 - 62;
        if (outerR <= 0) return '';
        const labelR = outerR + 35;
        const items = c.items ?? [];
        const count = Math.max(1, items.length);
        const itemIndex = Math.min(count - 1, Math.max(0, Math.round(c.value) - 1));
        out.push(
          `<circle cx="${cx}" cy="${cy}" r="${outerR}" fill="rgb(202,202,202)" stroke="rgb(80,80,80)"/>`
        );
        const activeAngle = 180 - (count > 1 ? itemIndex / (count - 1) : 0) * 180;
        const [asx, asy] = dialPoint(cx, cy, outerR, activeAngle);
        const [aex, aey] = dialPoint(cx, cy, outerR, activeAngle + 180);
        const [nxp, nyp] = dialPoint(0, 0, 10, activeAngle + 90);
        const band = `${asx + nxp},${asy + nyp} ${asx - nxp},${asy - nyp} ${aex - nxp},${
          aey - nyp
        } ${aex + nxp},${aey + nyp}`;
        out.push(`<polygon points="${band}" fill="rgb(238,238,238)" stroke="rgb(110,110,110)"/>`);
        for (let k = 0; k < count; k++) {
          const ang = 180 - (count > 1 ? k / (count - 1) : 0) * 180;
          const [dx, dy] = dialPoint(cx, cy, outerR + 13, ang);
          out.push(`<circle cx="${dx}" cy="${dy}" r="3.5" fill="${fg}"/>`);
          if (k < items.length) {
            const [lx, ly] = dialPoint(cx, cy, labelR + 2, ang);
            out.push(centered(lx, ly, items[k]));
          }
        }
        const [sx, sy] = dialPoint(cx, cy, outerR * 0.32, activeAngle);
        const [tx, ty] = dialPoint(cx, cy, outerR - 9, activeAngle);
        out.push(line(sx, sy, tx, ty, DISCRETE_POINTER, 3));
        return out.join('');
      }
      // The authored Position describes the face; ticks paint into a 38px margin.
      const outerR = Math.min(w, h) / 2 + 38 - 24;
      const faceR = outerR - 20;
      if (faceR <= 0) return '';
      const minors = minor.length ? minor : autoTicks(c.min, c.max, 40);
      for (const t of minors) {
        if (!withinLimits(t, c.min, c.max)) continue;
        const ang = 225 - frac(t) * 270;
        const [ox, oy] = dialPoint(cx, cy, outerR, ang);
        const [ix, iy] = dialPoint(cx, cy, outerR - 7, ang);
        out.push(line(ox, oy, ix, iy, fg, 1));
      }
      major.forEach((t, k) => {
        if (!withinLimits(t, c.min, c.max)) return;
        const ang = 225 - frac(t) * 270;
        const [ox, oy] = dialPoint(cx, cy, outerR, ang);
        const [ix, iy] = dialPoint(cx, cy, outerR - 12, ang);
        out.push(line(ox, oy, ix, iy, fg, 2));
        if (k < labels.length) {
          const [lx, ly] = dialPoint(cx, cy, outerR + 9, ang);
          out.push(centered(lx, ly, labels[k]));
        }
      });
      out.push(
        `<circle cx="${cx}" cy="${cy}" r="${faceR}" fill="rgb(202,202,202)" stroke="rgb(90,90,90)"/>`
      );
      const innerR = faceR * 0.65;
      out.push(
        `<circle cx="${cx}" cy="${cy}" r="${innerR}" fill="rgb(238,238,238)" stroke="rgb(90,90,90)"/>`
      );
      const ang = 225 - frac(c.value) * 270;
      const [tx, ty] = dialPoint(cx, cy, faceR + 5, ang);
      const [lx2, ly2] = dialPoint(cx, cy, innerR - 2, ang - 8);
      const [rx2, ry2] = dialPoint(cx, cy, innerR - 2, ang + 8);
      out.push(
        `<polygon points="${tx},${ty} ${lx2},${ly2} ${rx2},${ry2}" fill="${KNOB_POINTER}"/>`
      );
      return out.join('');
    }
    case 'gauge': {
      const shape = c.data?.shape ?? 'circular';
      const major = c.data?.major ?? [];
      const minor = c.data?.minor ?? [];
      const labels = c.data?.labels ?? [];
      const vertical = c.data?.vertical ?? false;
      const bg = c.background || '#ffffff';
      const frac = (v: number): number => dialFraction(v, c.min, c.max);
      const out: string[] = [];
      const line = (
        x1: number,
        y1: number,
        x2: number,
        y2: number,
        s: string,
        wd: number
      ): string =>
        `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${s}" stroke-width="${wd}"/>`;
      if (shape === 'linear') {
        out.push(
          `<rect x="${x + 1}" y="${y + 1}" width="${w - 2}" height="${h - 2}" fill="${bg}" ` +
            `stroke="${FRAME_COLOR}"/>`
        );
        const minors = minor.length ? minor : autoTicks(c.min, c.max, 25);
        if (vertical) {
          const vx = x + w / 2;
          const top = y + 8;
          const bottom = y + h - 8;
          for (const t of minors) {
            if (!withinLimits(t, c.min, c.max)) continue;
            const yy = bottom - (bottom - top) * frac(t);
            out.push(line(vx - 4, yy, vx - 1, yy, FRAME_COLOR, 1));
          }
          major.forEach((t, k) => {
            if (!withinLimits(t, c.min, c.max)) return;
            const yy = bottom - (bottom - top) * frac(t);
            out.push(line(vx - 8, yy, vx - 1, yy, FRAME_COLOR, 1));
            if (k < labels.length) {
              out.push(
                `<text x="${vx + 2 + fs}" y="${yy + fs * 0.35}" font-size="${fs}" ` +
                  `font-family="sans-serif" fill="${FRAME_COLOR}">${escapeXml(labels[k])}</text>`
              );
            }
          });
          const yv = bottom - (bottom - top) * frac(c.value);
          out.push(line(vx - 12, yv, vx + 12, yv, GAUGE_NEEDLE, 2));
        } else {
          const left = x + 8;
          const right = x + w - 8;
          const baseline = y + 11;
          for (const t of minors) {
            if (!withinLimits(t, c.min, c.max)) continue;
            const xx = left + (right - left) * frac(t);
            out.push(line(xx, baseline, xx, baseline + 4, FRAME_COLOR, 1));
          }
          major.forEach((t, k) => {
            if (!withinLimits(t, c.min, c.max)) return;
            const xx = left + (right - left) * frac(t);
            out.push(line(xx, baseline, xx, baseline + 8, FRAME_COLOR, 1));
            if (k < labels.length) {
              out.push(
                `<text x="${xx}" y="${baseline + 8 + fs * 0.9}" font-size="${fs}" ` +
                  `font-family="sans-serif" text-anchor="middle" fill="${FRAME_COLOR}">${escapeXml(
                    labels[k]
                  )}</text>`
              );
            }
          });
          const xv = left + (right - left) * frac(c.value);
          out.push(line(xv, baseline + 5, xv, y + h - 5, GAUGE_NEEDLE, 2));
        }
        return out.join('');
      }
      let startAngle = 225;
      let sweep = -270;
      let cxg = x + w / 2;
      let cyg = y + h / 2 + 2;
      let radius = Math.min(w, h) / 2 - 8;
      let minorDiv = 25;
      if (shape === 'semicircular') {
        startAngle = 180;
        sweep = -180;
        cyg = y + h - 7;
        radius = Math.min(w / 2 - 8, h - 12);
      } else if (shape === 'ninetydegree') {
        startAngle = 180;
        sweep = -90;
        cxg = x + w - 8;
        cyg = y + h - 8;
        radius = Math.min(w - 14, h - 14);
        minorDiv = 20;
      }
      if (radius <= 0) return '';
      if (shape === 'circular') {
        out.push(
          `<circle cx="${cxg}" cy="${cyg}" r="${radius}" fill="${bg}" stroke="${FRAME_COLOR}"/>`
        );
      } else {
        const wedge = [`${cxg},${cyg}`];
        const arc: string[] = [];
        for (let i = 0; i <= 40; i++) {
          const [px, py] = dialPoint(cxg, cyg, radius, startAngle + (sweep * i) / 40);
          wedge.push(`${px},${py}`);
          arc.push(`${px},${py}`);
        }
        out.push(`<polygon points="${wedge.join(' ')}" fill="${bg}" stroke="none"/>`);
        out.push(
          `<polyline points="${arc.join(
            ' '
          )}" fill="none" stroke="${FRAME_COLOR}" stroke-width="1"/>`
        );
        const [e0x, e0y] = dialPoint(cxg, cyg, radius, startAngle);
        const [e1x, e1y] = dialPoint(cxg, cyg, radius, startAngle + sweep);
        out.push(line(cxg, cyg, e0x, e0y, FRAME_COLOR, 1));
        out.push(line(cxg, cyg, e1x, e1y, FRAME_COLOR, 1));
      }
      const minors = minor.length ? minor : autoTicks(c.min, c.max, minorDiv);
      for (const t of minors) {
        if (!withinLimits(t, c.min, c.max)) continue;
        const ang = startAngle + sweep * frac(t);
        const [ox, oy] = dialPoint(cxg, cyg, radius - 2, ang);
        const [ix, iy] = dialPoint(cxg, cyg, radius - 7, ang);
        out.push(line(ox, oy, ix, iy, TICK_COLOR, 1));
      }
      major.forEach((t, k) => {
        if (!withinLimits(t, c.min, c.max)) return;
        const ang = startAngle + sweep * frac(t);
        const [ox, oy] = dialPoint(cxg, cyg, radius - 2, ang);
        const [ix, iy] = dialPoint(cxg, cyg, radius - 12, ang);
        out.push(line(ox, oy, ix, iy, TICK_COLOR, 1));
        if (k < labels.length) {
          const [lx, ly] = dialPoint(cxg, cyg, radius - 25, ang);
          out.push(
            `<text x="${lx}" y="${ly + fs * 0.35}" font-size="${fs}" font-family="sans-serif" ` +
              `text-anchor="middle" fill="${TICK_COLOR}">${escapeXml(labels[k])}</text>`
          );
        }
      });
      const va = startAngle + sweep * frac(c.value);
      const [nx, ny] = dialPoint(cxg, cyg, radius - 24, va);
      out.push(line(cxg, cyg, nx, ny, GAUGE_NEEDLE, 2));
      return out.join('');
    }
    case 'lamp':
      // Only the disc is painted: the corners keep the parent colour.
      return (
        `<circle cx="${cx}" cy="${cy}" r="${Math.max(0, Math.min(w, h) / 2 - 1)}" ` +
        `fill="${c.background || '#00ff00'}" stroke="#787878"/>`
      );
    case 'switch': {
      const on = c.value >= 1;
      const vertical = c.data?.vertical ?? false;
      const offLabel = c.items?.[0] ?? 'Off';
      const onLabel = c.items?.[1] ?? 'On';
      const thumb = (tx: number, ty: number): string =>
        `<circle cx="${tx}" cy="${ty}" r="9" fill="#ffffff" stroke="rgb(150,150,150)"/>`;
      const label = (lx: number, ly: number, anchor: string, txt: string): string =>
        `<text x="${lx}" y="${ly}" font-size="${fs}" font-family="sans-serif" ` +
        `text-anchor="${anchor}" fill="rgb(33,33,33)">${escapeXml(txt)}</text>`;
      if (vertical) {
        const tx = x + w / 2;
        return (
          `<rect x="${tx - 7}" y="${y}" width="14" height="${h}" rx="7" fill="${SWITCH_TRACK}"/>` +
          thumb(tx, on ? y + 7 : y + h - 7) +
          label(tx, y - 4, 'middle', onLabel) +
          label(tx, y + h + 4 + fs * 0.7, 'middle', offLabel)
        );
      }
      const cyt = y + h / 2;
      return (
        `<rect x="${x}" y="${cyt - 7}" width="${w}" height="14" rx="7" fill="${SWITCH_TRACK}"/>` +
        thumb(on ? x + w - 7 : x + 7, cyt) +
        label(x - 4, cyt + fs * 0.35, 'end', offLabel) +
        label(x + w + 4, cyt + fs * 0.35, 'start', onLabel)
      );
    }
    case 'frame':
      // A decorative filled, bordered rectangle (its BackgroundColor fill).
      return (
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" ` +
        `fill="${c.background || '#f5f5f5'}" stroke="${c.foreground || '#7d7d7d'}"/>`
      );
    case 'panel':
      return (
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" fill="none" ` +
        `stroke="${c.foreground || '#7d7d7d'}"/>` +
        (c.text
          ? `<text x="${x + 8}" y="${y}" font-size="${fs}" font-family="sans-serif" ` +
            `dominant-baseline="central" fill="${fg}">${escapeXml(c.text)}</text>`
          : '')
      );
    case 'tree': {
      const out: string[] = [
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" fill="#ffffff" stroke="#8f8f8f"/>`,
      ];
      const rowHeight = Math.max(fs + 6, 18);
      const indent = 16;
      let row = 0;
      const walk = (nodes: TreeNode[] | undefined, depth: number): void => {
        if (!nodes) return;
        for (const node of nodes) {
          const rowY = y + 4 + row * rowHeight;
          if (rowY + rowHeight > y + h) return;
          const nx = x + 6 + depth * indent;
          const ncy = rowY + rowHeight / 2;
          if (node.children && node.children.length) {
            out.push(
              `<polygon points="${nx},${ncy - 3} ${nx + 8},${ncy - 3} ${nx + 4},${ncy + 3}" ` +
                `fill="rgb(90,90,90)"/>`
            );
          }
          out.push(
            `<text x="${nx + 14}" y="${ncy + fs * 0.35}" font-size="${fs}" ` +
              `font-family="sans-serif" fill="${fg}">${escapeXml(node.text)}</text>`
          );
          row++;
          walk(node.children, depth + 1);
        }
      };
      walk(c.data?.nodes, 0);
      return out.join('');
    }
    default:
      // tables, tabgroups and anything else: a bordered box with its label.
      return (
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" fill="#ffffff" stroke="#b0b0b0"/>` +
        label(x + 4, 'start', c.text)
      );
  }
}
//=============================================================================
export function displayListToSvg(
  primitives: DrawPrimitive[],
  width: number,
  height: number,
  controls: UIControl[] = []
): string {
  const out: string[] = [];
  out.push(
    `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" ` +
      `viewBox="0 0 ${width} ${height}">`
  );
  // Figures are opaque white; guarantee a non-transparent background.
  out.push(`<rect x="0" y="0" width="${width}" height="${height}" fill="#ffffff"/>`);
  for (const p of primitives) {
    out.push(primitiveToSvg(p));
  }
  for (const c of controls) {
    out.push(controlToSvg(c));
  }
  out.push('</svg>');
  return out.join('\n');
}
//=============================================================================
