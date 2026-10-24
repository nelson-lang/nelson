//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect } from 'vitest';
import { displayListToSvg } from '../../src/panels/figureSvg';
import type { DrawPrimitive, UIControl } from '../../src/rpc/types';
//=============================================================================
// polyline/polygon/segments carry vertices as a base64 LE float32 blob; encode
// test points the same way the backend does.
function encPts(points: [number, number][]): string {
  const buffer = Buffer.alloc(points.length * 8);
  points.forEach((pt, i) => {
    buffer.writeFloatLE(pt[0], i * 8);
    buffer.writeFloatLE(pt[1], i * 8 + 4);
  });
  return buffer.toString('base64');
}
//=============================================================================
// The SVG exporter must mirror what FigureCanvas draws: same primitives, same
// anchoring, fonts carried through, uicontrols rendered as static widgets.
const control = (over: Partial<UIControl>): UIControl => ({
  handle: 1,
  style: 'pushbutton',
  text: '',
  x: 10,
  y: 20,
  w: 100,
  h: 30,
  value: 0,
  min: 0,
  max: 1,
  fontSize: 12,
  enabled: true,
  ...over,
});
//=============================================================================
describe('displayListToSvg: envelope', () => {
  it('emits a sized svg root with a white background rect', () => {
    const svg = displayListToSvg([], 640, 480);
    expect(svg).toContain('<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480"');
    expect(svg).toContain('viewBox="0 0 640 480"');
    expect(svg).toContain('<rect x="0" y="0" width="640" height="480" fill="#ffffff"/>');
    expect(svg.trim().endsWith('</svg>')).toBe(true);
  });
});
//=============================================================================
describe('displayListToSvg: primitives', () => {
  it('maps rect with its fill', () => {
    const p: DrawPrimitive = { kind: 'rect', x: 1, y: 2, w: 3, h: 4, fill: 'rgba(245,245,245,1)' };
    expect(displayListToSvg([p], 10, 10)).toContain(
      '<rect x="1" y="2" width="3" height="4" fill="rgba(245,245,245,1)"/>'
    );
  });

  it('maps line with stroke, width and dash array', () => {
    const p: DrawPrimitive = {
      kind: 'line',
      x1: 0,
      y1: 0,
      x2: 5,
      y2: 5,
      stroke: 'rgba(0,0,0,1)',
      width: 2,
      dash: [4, 2],
    };
    const svg = displayListToSvg([p], 10, 10);
    expect(svg).toContain('<line x1="0" y1="0" x2="5" y2="5"');
    expect(svg).toContain('stroke="rgba(0,0,0,1)"');
    expect(svg).toContain('stroke-width="2"');
    expect(svg).toContain('stroke-dasharray="4,2"');
  });

  it('maps polyline as an unfilled stroke', () => {
    const p: DrawPrimitive = {
      kind: 'polyline',
      count: 3,
      pointsB64: encPts([
        [0, 0],
        [1, 2],
        [3, 4],
      ]),
      stroke: 'rgba(255,0,0,1)',
      width: 1,
    };
    const svg = displayListToSvg([p], 10, 10);
    expect(svg).toContain('<polyline points="0,0 1,2 3,4"');
    expect(svg).toContain('fill="none"');
    expect(svg).toContain('stroke="rgba(255,0,0,1)"');
  });

  it('maps polygon with fill and optional stroke', () => {
    const noStroke: DrawPrimitive = {
      kind: 'polygon',
      count: 3,
      pointsB64: encPts([
        [0, 0],
        [4, 0],
        [2, 3],
      ]),
      fill: 'rgba(0,0,255,1)',
    };
    const withStroke: DrawPrimitive = { ...noStroke, stroke: 'rgba(0,0,0,1)', width: 2 };
    expect(displayListToSvg([noStroke], 10, 10)).toContain(
      '<polygon points="0,0 4,0 2,3" fill="rgba(0,0,255,1)"/>'
    );
    const svg = displayListToSvg([withStroke], 10, 10);
    expect(svg).toContain('stroke="rgba(0,0,0,1)" stroke-width="2"');
  });

  it('maps image with the data URI and no aspect-ratio distortion guard', () => {
    const p: DrawPrimitive = {
      kind: 'image',
      x: 5,
      y: 6,
      w: 20,
      h: 10,
      image: 'data:image/png;base64,AAAA',
    };
    const svg = displayListToSvg([p], 40, 40);
    expect(svg).toContain('preserveAspectRatio="none"');
    expect(svg).toContain('href="data:image/png;base64,AAAA"');
  });
});
//=============================================================================
describe('displayListToSvg: text', () => {
  const text = (over: Partial<Extract<DrawPrimitive, { kind: 'text' }>>): DrawPrimitive => ({
    kind: 'text',
    x: 50,
    y: 60,
    text: 'hello',
    align: 'left',
    color: 'rgba(0,0,0,1)',
    size: 12,
    ...over,
  });

  it('anchors per alignment (left/center/right -> start/middle/end)', () => {
    expect(displayListToSvg([text({})], 100, 100)).toContain('text-anchor="start"');
    expect(displayListToSvg([text({ align: 'center' })], 100, 100)).toContain(
      'text-anchor="middle"'
    );
    expect(displayListToSvg([text({ align: 'right' })], 100, 100)).toContain('text-anchor="end"');
  });

  it('rotates about the anchor with a translate+rotate transform', () => {
    const svg = displayListToSvg([text({ rotation: 90 })], 100, 100);
    expect(svg).toContain('transform="translate(50,60) rotate(-90)"');
    expect(svg).toContain('<text x="0" y="0"');
  });

  it('defaults to sans-serif and honors font family / bold / italic', () => {
    expect(displayListToSvg([text({})], 100, 100)).toContain('font-family="sans-serif"');
    const styled = displayListToSvg([text({ font: 'serif', bold: true, italic: true })], 100, 100);
    expect(styled).toContain('font-family="serif"');
    expect(styled).toContain('font-weight="bold"');
    expect(styled).toContain('font-style="italic"');
  });

  it('escapes XML metacharacters in the text', () => {
    const svg = displayListToSvg([text({ text: 'a<b & "c">' })], 100, 100);
    expect(svg).toContain('a&lt;b &amp; &quot;c&quot;&gt;');
    expect(svg).not.toContain('a<b');
  });
});
//=============================================================================
describe('displayListToSvg: uicontrols', () => {
  it('draws controls after (above) the primitives', () => {
    const p: DrawPrimitive = { kind: 'rect', x: 0, y: 0, w: 5, h: 5, fill: 'rgba(1,2,3,1)' };
    const svg = displayListToSvg([p], 200, 200, [control({ text: 'OK' })]);
    expect(svg.indexOf('rgba(1,2,3,1)')).toBeLessThan(svg.indexOf('>OK<'));
  });

  it('pushbutton: rounded box + centered label', () => {
    const svg = displayListToSvg([], 200, 200, [control({ text: 'Go' })]);
    expect(svg).toContain('rx="3"');
    expect(svg).toContain('text-anchor="middle"');
    expect(svg).toContain('>Go<');
  });

  it('checkbox: box always, check mark only when checked', () => {
    const un = displayListToSvg([], 200, 200, [
      control({ style: 'checkbox', text: 'c', value: 0 }),
    ]);
    const chk = displayListToSvg([], 200, 200, [
      control({ style: 'checkbox', text: 'c', value: 1 }),
    ]);
    expect(un).not.toContain('<path');
    expect(chk).toContain('<path');
  });

  it('slider: thumb position follows the value fraction', () => {
    const mk = (value: number) =>
      displayListToSvg([], 200, 200, [control({ style: 'slider', value, min: 0, max: 100 })]);
    // x=10, w=100 -> value 50 puts the 10px-wide thumb at x = 10+50-5 = 55
    expect(mk(50)).toContain('<rect x="55"');
    expect(mk(0)).toContain('<rect x="5"');
    expect(mk(100)).toContain('<rect x="105"');
  });

  it('slider: the range variant draws two thumbs', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({ style: 'slider', value: 20, min: 0, max: 100, data: { range: [20, 70] } }),
    ]);
    // low thumb at 10+20-5 = 25, high thumb at 10+70-5 = 75
    expect(svg).toContain('<rect x="25"');
    expect(svg).toContain('<rect x="75"');
  });

  it('lamp: a disc in its colour, no box behind it', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({ style: 'lamp', w: 20, h: 20, background: '#ff0000' }),
    ]);
    // x=10, y=20, 20x20 -> centre (20, 30), radius 9
    expect(svg).toContain('<circle cx="20" cy="30" r="9" fill="#ff0000"');
    expect(svg).not.toContain('<rect x="10"');
  });

  it('switch: knob slides with the state', () => {
    const off = displayListToSvg([], 200, 200, [control({ style: 'switch', value: 0 })]);
    const on = displayListToSvg([], 200, 200, [control({ style: 'switch', value: 1 })]);
    // Dark track either way; the white thumb slides from x+7 (off) to x+w-7 (on).
    expect(off).toContain('fill="rgb(112,112,112)"');
    expect(off).toContain('cx="17"');
    expect(on).toContain('cx="103"');
    expect(off).not.toEqual(on);
  });

  it('gauge: draws scale ticks, numeric labels and a red needle', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({
        style: 'gauge',
        value: 75,
        min: 0,
        max: 100,
        w: 120,
        h: 120,
        data: { shape: 'circular', major: [0, 50, 100], labels: ['0', '50', '100'] },
      }),
    ]);
    expect(svg).toContain('rgb(200,30,30)');
    expect(svg).toContain('>50<');
  });

  it('knob: draws ticks and a red triangle pointer', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({
        style: 'knob',
        value: 50,
        min: 0,
        max: 100,
        w: 120,
        h: 120,
        data: { discrete: false, major: [0, 50, 100], labels: ['0', '50', '100'] },
      }),
    ]);
    expect(svg).toContain('rgb(196,38,46)');
    expect(svg).toContain('<polygon');
  });

  it('knob: keeps a small authored face visible with ticks outside its position', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({ style: 'knob', x: 50, y: 50, w: 70, h: 70, min: 0, max: 6, value: 1 }),
    ]);
    expect(svg).toContain('r="29"');
    expect(svg).toContain('rgb(196,38,46)');
    expect(svg).toContain('<line');
  });

  it('discrete knob: retains its face and item labels at the default size', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({
        style: 'knob',
        w: 60,
        h: 60,
        value: 1,
        items: ['Off', 'On'],
        data: { discrete: true },
      }),
    ]);
    expect(svg).toContain('r="28"');
    expect(svg).toContain('>Off<');
    expect(svg).toContain('>On<');
  });

  it('tree: draws the node rows', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({
        style: 'tree',
        w: 150,
        h: 120,
        data: { nodes: [{ text: 'Fruits', children: [{ text: 'Apple' }, { text: 'Banana' }] }] },
      }),
    ]);
    expect(svg).toContain('>Fruits<');
    expect(svg).toContain('>Apple<');
    expect(svg).toContain('>Banana<');
  });

  it('edit: a multi-line value lays out on separate lines', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({ style: 'edit', text: 'A\nB\nC', w: 120, h: 80 }),
    ]);
    const texts = svg.match(/<text /g) ?? [];
    expect(texts.length).toBeGreaterThanOrEqual(3);
  });

  it('panel: unfilled bordered box with its title', () => {
    const svg = displayListToSvg([], 200, 200, [control({ style: 'panel', text: 'My Panel' })]);
    expect(svg).toContain('fill="none"');
    expect(svg).toContain('>My Panel<');
  });

  it('frame: filled bordered box using its BackgroundColor', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({ style: 'frame', background: 'rgb(245,245,245)', foreground: 'rgb(33,33,33)' }),
    ]);
    expect(svg).toContain('fill="rgb(245,245,245)"');
    expect(svg).toContain('stroke="rgb(33,33,33)"');
  });

  it('popupmenu shows its current entry', () => {
    const svg = displayListToSvg([], 200, 200, [
      control({ style: 'popupmenu', text: '', items: ['alpha', 'beta'] }),
    ]);
    expect(svg).toContain('>alpha<');
  });

  it('unknown/rich styles degrade to a bordered box (no crash)', () => {
    const svg = displayListToSvg([], 200, 200, [control({ style: 'table', text: '' })]);
    expect(svg).toContain('stroke="#b0b0b0"');
    expect(svg).toContain('</svg>');
  });
});
//=============================================================================
describe('displayListToSvg: button label alignment', () => {
  const texts = (svg: string) =>
    Array.from(svg.matchAll(/<text x="([^"]+)" y="([^"]+)"[^>]*text-anchor="([^"]+)"/g)).map(
      (m) => ({ x: Number(m[1]), y: Number(m[2]), anchor: m[3] })
    );

  it('keeps a centered label by default', () => {
    const [t] = texts(displayListToSvg([], 200, 200, [control({ text: 'X', h: 100 })]));
    expect(t).toEqual({ x: 60, y: 70, anchor: 'middle' });
  });

  it('puts the label at the bottom right, one line per text row', () => {
    const c = control({ text: 'top\nbottom', h: 100, hAlign: 'right', vAlign: 'bottom' });
    const [first, second] = texts(displayListToSvg([], 200, 200, [c]));
    expect(first.anchor).toBe('end');
    expect(first.x).toBe(106);
    expect(second.y).toBeCloseTo(20 + 100 - 4 - 14.4 / 2, 6);
    expect(first.y).toBeLessThan(second.y);
  });

  it('puts the label at the top left', () => {
    const c = control({ text: 'X', h: 100, hAlign: 'left', vAlign: 'top' });
    const [t] = texts(displayListToSvg([], 200, 200, [c]));
    expect(t.anchor).toBe('start');
    expect(t.x).toBe(14);
    expect(t.y).toBeCloseTo(20 + 4 + 14.4 / 2, 6);
  });
});
//=============================================================================
