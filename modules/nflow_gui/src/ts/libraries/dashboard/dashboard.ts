//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { Block, Helpers } from '../types.js';
import { nflowRpc } from '../../app/native-rpc.js';
import {
  applyDashboardOpacity,
  bindContinuousKeyboard,
  createDashboardThrottle,
  dashboardColor as rgb,
  dashboardLimits as limitsOf,
  dashboardRatio,
  dashboardScaleType as scaleTypeOf,
  dashboardValue,
  formatDashboardValue,
} from './dashboard-values.js';
import { emitAppEvent } from '../../app/app-events.js';
import { subscribeDashboardUpdates } from './dashboard-updates.js';
//=============================================================================
type StateItem = { Value?: number | string; Label?: string };
type Binding = {
  kind?: 'signal' | 'parameter' | 'variable';
  blockPath?: string | string[];
  outputPortIndex?: number;
  frameProcessingMode?: 'sample' | 'frame';
  paramName?: string;
  varName?: string;
  workspace?: string;
  element?: string;
};
//=============================================================================
const SVG_NS = 'http://www.w3.org/2000/svg';
let dashboardScopeClipSequence = 0;
function svg<K extends keyof SVGElementTagNameMap>(
  tag: K,
  attrs: Record<string, string | number> = {}
): SVGElementTagNameMap[K] {
  const el = document.createElementNS(SVG_NS, tag);
  for (const [key, value] of Object.entries(attrs)) el.setAttribute(key, String(value));
  return el;
}
//=============================================================================
function statesOf(block: Block): StateItem[] {
  const states = block.params?.States;
  return Array.isArray(states) && states.length
    ? states.map((item) => (item && typeof item === 'object' ? (item as StateItem) : {}))
    : [
        { Value: 0, Label: 'Off' },
        { Value: 1, Label: 'On' },
      ];
}
//=============================================================================
function bindingOf(block: Block): Binding | null {
  const binding = block.params?.Binding;
  return binding && typeof binding === 'object' ? (binding as Binding) : null;
}
//=============================================================================
function designTargetValue(block: Block): unknown {
  return block.params?.DashboardValue;
}
//=============================================================================
function sendValue(block: Block, value: number): void {
  block.params.DashboardValue = value;
  emitAppEvent('nf-dashboard-control-change', {
    id: String(block.id ?? ''),
    binding: bindingOf(block),
    value,
  });
  try {
    void nflowRpc('nflow.sim.event', {
      name: 'DashboardControl',
      data: JSON.stringify({ type: 'setDashboardValue', id: String(block.id ?? ''), value }),
    }).catch(() => {});
  } catch {
    // A stopped simulation has no event consumer; the design target was updated above.
  }
}
//=============================================================================
function invokeCallback(block: Block, code: unknown): void {
  if (!String(code ?? '').trim()) return;
  void nflowRpc('nflow.callback.invoke', {
    code: String(code ?? ''),
    blockId: String(block.id ?? ''),
  }).catch((error: unknown) => console.error('NFlow callback:', error));
}
//=============================================================================
function appendText(
  group: Element,
  text: string,
  x: number,
  y: number,
  className: string,
  anchor = 'middle'
): SVGTextElement {
  const el = svg('text', { x, y, class: className, 'text-anchor': anchor });
  el.textContent = text;
  group.appendChild(el);
  return el;
}
//=============================================================================
function renderFrame(block: Block): SVGRectElement {
  const frame = svg('rect', {
    x: 0,
    y: 0,
    width: block.width,
    height: block.height,
    rx: 3,
    class: 'dashboard-frame',
  });
  block.group.appendChild(frame);
  return frame;
}
//=============================================================================
type ScaleRange = { Min?: number; Max?: number; Color?: unknown };
function scaleRangesOf(block: Block): ScaleRange[] {
  const ranges = block.params?.ScaleColors;
  if (Array.isArray(ranges)) return ranges.filter((item) => item && typeof item === 'object');
  return ranges && typeof ranges === 'object' ? [ranges as ScaleRange] : [];
}
//=============================================================================
function renderGauge(block: Block, kind: string): (value: number) => void {
  const group = block.group;
  const [lo, hi] = limitsOf(block);
  const cx = block.width / 2;
  const cy = kind === 'dashboardGauge' ? block.height * 0.57 : block.height * 0.78;
  const radius = Math.max(18, Math.min(block.width * 0.38, block.height * 0.48));
  let start = -225;
  let sweep = 270;
  if (kind === 'dashboardHalfGauge') {
    start = -180;
    sweep = 180;
  } else if (kind === 'dashboardQuarterGauge') {
    start = -180;
    sweep = 90;
  }
  const polar = (angle: number, r: number) => {
    const a = (angle * Math.PI) / 180;
    return [cx + Math.cos(a) * r, cy + Math.sin(a) * r];
  };
  const direction = block.params?.ScaleDirection === 'Counterclockwise' ? -1 : 1;
  const directedSweep = sweep * direction;
  const p0 = polar(start, radius);
  const p1 = polar(start + directedSweep, radius);
  const large = sweep > 180 ? 1 : 0;
  const arc = svg('path', {
    d: `M ${p0[0]} ${p0[1]} A ${radius} ${radius} 0 ${large} ${direction > 0 ? 1 : 0} ${p1[0]} ${
      p1[1]
    }`,
    class: 'dashboard-scale',
  });
  group.appendChild(arc);
  for (const range of scaleRangesOf(block)) {
    const rangeLo = Math.max(lo, Number(range.Min));
    const rangeHi = Math.min(hi, Number(range.Max));
    if (!Number.isFinite(rangeLo) || !Number.isFinite(rangeHi) || rangeHi <= rangeLo) continue;
    const a0 = start + dashboardRatio(rangeLo, [lo, hi]) * directedSweep;
    const a1 = start + dashboardRatio(rangeHi, [lo, hi]) * directedSweep;
    const q0 = polar(a0, radius);
    const q1 = polar(a1, radius);
    const rangeSweep = Math.abs(a1 - a0);
    const zone = svg('path', {
      d: `M ${q0[0]} ${q0[1]} A ${radius} ${radius} 0 ${rangeSweep > 180 ? 1 : 0} ${
        direction > 0 ? 1 : 0
      } ${q1[0]} ${q1[1]}`,
      class: 'dashboard-scale-zone',
    });
    zone.setAttribute('stroke', rgb(range.Color, '#2f80ed'));
    group.appendChild(zone);
  }
  const needle = svg('line', {
    x1: cx,
    y1: cy,
    x2: cx,
    y2: cy - radius * 0.75,
    class: 'dashboard-needle',
  });
  group.appendChild(needle);
  group.appendChild(svg('circle', { cx, cy, r: 4, class: 'dashboard-hub' }));
  const valueText = appendText(
    group,
    '--',
    cx,
    Math.min(block.height - 8, cy + 22),
    'dashboard-value'
  );
  return (value: number) => {
    const clamped = Math.max(lo, Math.min(hi, value));
    const angle = start + dashboardRatio(clamped, [lo, hi]) * directedSweep;
    const tip = polar(angle, radius * 0.75);
    needle.setAttribute('x2', String(tip[0]));
    needle.setAttribute('y2', String(tip[1]));
    valueText.textContent = Number.isFinite(value) ? String(Number(value.toPrecision(6))) : '--';
  };
}
//=============================================================================
function renderLinearGauge(block: Block): (value: number) => void {
  const [lo, hi] = limitsOf(block);
  const x = 18;
  const y = block.height / 2;
  const width = Math.max(20, block.width - 36);
  block.group.appendChild(
    svg('line', { x1: x, y1: y, x2: x + width, y2: y, class: 'dashboard-scale' })
  );
  for (const range of scaleRangesOf(block)) {
    const rangeLo = Math.max(lo, Number(range.Min));
    const rangeHi = Math.min(hi, Number(range.Max));
    if (!Number.isFinite(rangeLo) || !Number.isFinite(rangeHi) || rangeHi <= rangeLo) continue;
    block.group.appendChild(
      svg('line', {
        x1: x + dashboardRatio(rangeLo, [lo, hi]) * width,
        y1: y,
        x2: x + dashboardRatio(rangeHi, [lo, hi]) * width,
        y2: y,
        class: 'dashboard-scale-zone',
        stroke: rgb(range.Color, '#2f80ed'),
      })
    );
  }
  const marker = svg('path', { d: `M ${x} ${y - 10} l 6 10 l -12 0 z`, class: 'dashboard-marker' });
  block.group.appendChild(marker);
  const valueText = appendText(block.group, '--', block.width / 2, y + 28, 'dashboard-value');
  return (value: number) => {
    const clamped = Math.max(lo, Math.min(hi, value));
    const px = x + ((clamped - lo) / (hi - lo)) * width;
    marker.setAttribute('transform', `translate(${px - x} 0)`);
    valueText.textContent = Number.isFinite(value) ? String(Number(value.toPrecision(6))) : '--';
  };
}
//=============================================================================
function renderDisplay(block: Block): (value: number | number[]) => void {
  const alignment = String(block.params?.Alignment ?? 'Center');
  const anchor = alignment === 'Left' ? 'start' : alignment === 'Right' ? 'end' : 'middle';
  const x = alignment === 'Left' ? 6 : alignment === 'Right' ? block.width - 6 : block.width / 2;
  const text = appendText(
    block.group,
    '--',
    x,
    block.height / 2 + 7,
    'dashboard-display-value',
    anchor
  );
  return (value) => {
    const format = String(block.params?.Format ?? 'short');
    const pattern = String(block.params?.FormatString ?? '%d');
    text.textContent = Array.isArray(value)
      ? value.map((item) => formatDashboardValue(Number(item), format, pattern)).join(' ')
      : formatDashboardValue(Number(value), format, pattern);
  };
}
//=============================================================================
function renderLamp(block: Block): (value: number) => void {
  const radius = Math.max(10, Math.min(block.width, block.height) * 0.34);
  const lamp = svg('circle', {
    cx: block.width / 2,
    cy: block.height / 2,
    r: radius,
    class: 'dashboard-lamp',
    fill: rgb(block.params?.ColorDefault, '#c0c0c0'),
  });
  block.group.appendChild(lamp);
  return (value) => {
    const states = Array.isArray(block.params?.StateColors) ? block.params.StateColors : [];
    const match = states.find((entry: any) => Number(entry?.Value) === Number(value));
    lamp.setAttribute('fill', rgb(match?.Color, rgb(block.params?.ColorDefault, '#c0c0c0')));
  };
}
//=============================================================================
function renderImage(block: Block): (value: number) => void {
  const image = svg('image', {
    x: 4,
    y: 4,
    width: block.width - 8,
    height: block.height - 8,
    class: 'dashboard-state-image',
  });
  const scaleMode = String(block.params?.ScaleMode ?? 'Fill with fixed aspect ratio');
  image.setAttribute(
    'preserveAspectRatio',
    scaleMode === 'Fill' ? 'none' : scaleMode === 'Fixed' ? 'xMidYMid' : 'xMidYMid meet'
  );
  block.group.appendChild(image);
  const fallback = appendText(
    block.group,
    'No image',
    block.width / 2,
    block.height / 2 + 5,
    'dashboard-placeholder'
  );
  return (value) => {
    const states = Array.isArray(block.params?.States) ? block.params.States : [];
    const match = states.find((entry: any) => Number(entry?.State) === Number(value));
    const source = String(match?.Image || (block.params?.DefaultImage as any)?.Image || '');
    if (source) {
      image.setAttribute('href', source);
      fallback.setAttribute('display', 'none');
    } else {
      image.removeAttribute('href');
      fallback.setAttribute('display', 'block');
    }
  };
}
//=============================================================================
function renderScope(
  block: Block
): (value: number | number[], series?: number[][], times?: number[]) => void {
  const left = 28;
  const top = 18;
  const width = Math.max(20, block.width - left - 12);
  const height = Math.max(20, block.height - top - 22);
  const clipId = `nflow-dashboard-scope-clip-${++dashboardScopeClipSequence}`;
  const defs = svg('defs');
  const clip = svg('clipPath', { id: clipId, clipPathUnits: 'userSpaceOnUse' });
  clip.appendChild(svg('rect', { x: left, y: top, width, height }));
  defs.appendChild(clip);
  block.group.appendChild(defs);
  block.group.appendChild(
    svg('path', {
      d: `M ${left} ${top} V ${top + height} H ${left + width}`,
      class: 'dashboard-axis',
    })
  );
  const path = svg('path', {
    d: '',
    class: 'dashboard-scope-path',
    'clip-path': `url(#${clipId})`,
  });
  block.group.appendChild(path);
  const markerGroup = svg('g', {
    class: 'dashboard-scope-markers',
    'clip-path': `url(#${clipId})`,
  });
  block.group.appendChild(markerGroup);
  const update = (_value: number | number[], series?: number[][], times?: number[]) => {
    let data = Array.isArray(series?.[0]) ? series![0] : [];
    let time = Array.isArray(times) && times.length === data.length ? times : data.map((_, i) => i);
    const configuredSpan = Number(block.params?.TimeSpan);
    const timeSpan = Number.isFinite(configuredSpan) && configuredSpan > 0 ? configuredSpan : null;
    if (timeSpan && time.length) {
      const maxTime = time[time.length - 1];
      const first = Math.max(
        0,
        time.findIndex((item) => item >= maxTime - timeSpan)
      );
      data = data.slice(first);
      time = time.slice(first);
    }
    if (data.length < 2) {
      path.setAttribute('d', '');
      markerGroup.replaceChildren();
      return;
    }
    const finite = data.filter(Number.isFinite);
    const configuredY = Array.isArray(block.params?.YLimits) ? block.params.YLimits : [-3, 3];
    const normalized = block.params?.NormalizeYAxis === 'on';
    const yMin = normalized ? 0 : Number(configuredY[0]);
    const yMax = normalized ? 1 : Number(configuredY[1]);
    const dataMin = finite.length ? Math.min(...finite) : -1;
    const dataMax = finite.length ? Math.max(...finite) : 1;
    const dataSpan = dataMax > dataMin ? dataMax - dataMin : 1;
    const span = yMax > yMin ? yMax - yMin : 1;
    const startTime = time[0];
    const endTime = time[time.length - 1];
    const xSpan = timeSpan ?? Math.max(1e-12, endTime - startTime);
    let previousRatio = -1;
    const d = data
      .map((v, index) => {
        if (!Number.isFinite(Number(v))) {
          previousRatio = -1;
          return '';
        }
        const rawRatio = timeSpan
          ? block.params?.UpdateMode === 'Wrap'
            ? (((time[index] % timeSpan) + timeSpan) % timeSpan) / timeSpan
            : (time[index] - (endTime - timeSpan)) / timeSpan
          : (time[index] - startTime) / xSpan;
        const ratio = Math.max(0, Math.min(1, rawRatio));
        const displayed = normalized ? (Number(v) - dataMin) / dataSpan : Number(v);
        const x = left + ratio * width;
        const rawY = top + height - ((displayed - yMin) / span) * height;
        const y = Math.max(top, Math.min(top + height, rawY));
        const command = index && previousRatio >= 0 && ratio >= previousRatio ? 'L' : 'M';
        previousRatio = ratio;
        return `${command} ${x.toFixed(2)} ${y.toFixed(2)}`;
      })
      .filter(Boolean)
      .join(' ');
    path.setAttribute('d', d);
    markerGroup.replaceChildren();
    if (block.params?.Markers === 'on') {
      for (const segment of d.matchAll(/[ML] ([\d.]+) ([\d.]+)/g)) {
        markerGroup.appendChild(
          svg('circle', { cx: segment[1], cy: segment[2], r: 2, class: 'dashboard-scope-marker' })
        );
      }
    }
  };
  return update;
}
//=============================================================================
function attachSlider(block: Block, vertical = false): (value: number) => void {
  const [lo, hi] = limitsOf(block);
  const scaleType = scaleTypeOf(block);
  const step = Array.isArray(block.params?.Limits) ? Number(block.params.Limits[1]) : -1;
  const pad = 18;
  const x1 = vertical ? block.width / 2 : pad;
  const y1 = vertical ? pad : block.height / 2;
  const x2 = vertical ? block.width / 2 : block.width - pad;
  const y2 = vertical ? block.height - pad : block.height / 2;
  const hitTrack = svg('line', {
    x1,
    y1,
    x2,
    y2,
    class: 'dashboard-control-hit',
  });
  const track = svg('line', { x1, y1, x2, y2, class: 'dashboard-control-track' });
  const thumb = svg('circle', { cx: x1, cy: y1, r: 8, class: 'dashboard-control-thumb' });
  block.group.append(hitTrack, track, thumb);
  let current = lo;
  const update = (value: number) => {
    current = Math.max(lo, Math.min(hi, value));
    const ratio = dashboardRatio(current, [lo, hi], scaleType);
    thumb.setAttribute('cx', String(x1 + (x2 - x1) * ratio));
    thumb.setAttribute('cy', String(y1 + (y2 - y1) * ratio));
    block.group.setAttribute('aria-valuenow', String(current));
  };
  const pointerValue = (event: PointerEvent) => {
    const rect = track.getBoundingClientRect();
    const ratio = vertical
      ? (event.clientY - rect.top) / Math.max(1, rect.height)
      : (event.clientX - rect.left) / Math.max(1, rect.width);
    return dashboardValue(ratio, [lo, hi], scaleType, step);
  };
  const sender = createDashboardThrottle((value) => sendValue(block, value));
  track.setAttribute('style', 'pointer-events:stroke;cursor:pointer');
  thumb.setAttribute('style', 'pointer-events:all;cursor:pointer');
  const down = (event: PointerEvent) => {
    if (event.button !== 0) return;
    event.stopPropagation();
    const move = (ev: PointerEvent) => {
      const value = pointerValue(ev);
      update(value);
      sender.push(value);
    };
    const up = () => {
      sender.flush(current);
      window.removeEventListener('pointermove', move, true);
      window.removeEventListener('pointerup', up, true);
    };
    move(event);
    window.addEventListener('pointermove', move, true);
    window.addEventListener('pointerup', up, true);
  };
  for (const target of [hitTrack, track, thumb]) {
    target.addEventListener('pointerdown', down as EventListener, true);
  }
  bindContinuousKeyboard(
    block,
    () => current,
    (value) => {
      update(value);
      sendValue(block, value);
    }
  );
  return update;
}
//=============================================================================
function attachKnob(block: Block): (value: number) => void {
  const [lo, hi] = limitsOf(block);
  const scaleType = scaleTypeOf(block);
  const step = Array.isArray(block.params?.Limits) ? Number(block.params.Limits[1]) : -1;
  const cx = block.width / 2;
  const cy = block.height / 2;
  const radius = Math.max(15, Math.min(block.width, block.height) * 0.3);
  block.group.appendChild(svg('circle', { cx, cy, r: radius, class: 'dashboard-knob' }));
  const needle = svg('line', {
    x1: cx,
    y1: cy,
    x2: cx,
    y2: cy - radius * 0.8,
    class: 'dashboard-knob-needle',
  });
  block.group.appendChild(needle);
  let current = lo;
  const update = (value: number) => {
    current = Math.max(lo, Math.min(hi, value));
    const ratio = dashboardRatio(current, [lo, hi], scaleType);
    needle.setAttribute('transform', `rotate(${-135 + ratio * 270} ${cx} ${cy})`);
    block.group.setAttribute('aria-valuenow', String(current));
  };
  const onPointer = (event: PointerEvent) => {
    event.stopPropagation();
    const group = block.group as SVGGraphicsElement;
    let localX = cx;
    let localY = cy;
    try {
      const matrix = group.getScreenCTM?.();
      if (!matrix) throw new Error('Missing screen transform');
      const point = new DOMPoint(event.clientX, event.clientY).matrixTransform(matrix.inverse());
      localX = point.x;
      localY = point.y;
    } catch {
      const rect = group.getBoundingClientRect();
      localX = ((event.clientX - rect.left) / Math.max(1, rect.width)) * block.width;
      localY = ((event.clientY - rect.top) / Math.max(1, rect.height)) * block.height;
    }
    const angle = Math.atan2(localY - cy, localX - cx);
    let degrees = (angle * 180) / Math.PI;
    if (degrees >= 135) degrees -= 360;
    const ratio = Math.max(0, Math.min(1, (degrees + 225) / 270));
    const value = dashboardValue(ratio, [lo, hi], scaleType, step);
    update(value);
    return value;
  };
  const sender = createDashboardThrottle((value) => sendValue(block, value));
  const down = (event: PointerEvent) => {
    if (event.button !== 0) return;
    const pointerId = event.pointerId;
    const matchesPointer = (candidate: PointerEvent) =>
      pointerId === 0 || candidate.pointerId === 0 || candidate.pointerId === pointerId;
    const move = (moveEvent: PointerEvent) => {
      if (!matchesPointer(moveEvent)) return;
      const value = onPointer(moveEvent);
      if (value !== undefined) sender.push(value);
    };
    const up = (upEvent: PointerEvent) => {
      if (!matchesPointer(upEvent)) return;
      sender.flush(current);
      window.removeEventListener('pointermove', move, true);
      window.removeEventListener('pointerup', up, true);
      window.removeEventListener('pointercancel', up, true);
    };
    move(event);
    window.addEventListener('pointermove', move, true);
    window.addEventListener('pointerup', up, true);
    window.addEventListener('pointercancel', up, true);
  };
  (block.group as Element).addEventListener('pointerdown', down as EventListener, true);
  bindContinuousKeyboard(
    block,
    () => current,
    (value) => {
      update(value);
      sendValue(block, value);
    }
  );
  return update;
}
//=============================================================================
function attachStateControl(block: Block, kind: string): (value: number) => void {
  const items =
    kind === 'dashboardCheckBox'
      ? [
          {
            Value: Number((block.params?.Values as any)?.[0] ?? 0),
            Label: String(block.params?.Label ?? 'Label'),
          },
          {
            Value: Number((block.params?.Values as any)?.[1] ?? 1),
            Label: String(block.params?.Label ?? 'Label'),
          },
        ]
      : statesOf(block);
  let selected = 0;
  const nodes: SVGElement[] = [];
  let comboSelect: HTMLSelectElement | null = null;
  if (kind === 'dashboardComboBox') {
    const foreign = svg('foreignObject', {
      x: 3,
      y: 0,
      width: block.width - 6,
      height: block.height,
      class: 'dashboard-combo-foreign',
    });
    comboSelect = document.createElement('select');
    comboSelect.className = 'dashboard-combo-select';
    items.forEach((item, index) => {
      const option = document.createElement('option');
      option.value = String(index);
      option.textContent = String(item.Label ?? item.Value ?? '');
      comboSelect?.appendChild(option);
    });
    comboSelect.addEventListener('pointerdown', (event) => event.stopPropagation());
    comboSelect.addEventListener('click', (event) => event.stopPropagation());
    comboSelect.addEventListener('keydown', (event) => {
      if (event.key === 'Escape') comboSelect?.blur();
    });
    comboSelect.addEventListener('change', () => {
      const index = comboSelect?.selectedIndex ?? 0;
      sendValue(block, Number(items[index]?.Value ?? index));
    });
    foreign.appendChild(comboSelect);
    block.group.appendChild(foreign);
  } else if (kind === 'dashboardRadioButton') {
    items.forEach((item, index) => {
      const y = 24 + index * 25;
      const circle = svg('circle', { cx: 16, cy: y, r: 6, class: 'dashboard-radio' });
      circle.setAttribute('data-dashboard-index', String(index));
      block.group.appendChild(circle);
      const label = appendText(
        block.group,
        String(item.Label ?? item.Value ?? ''),
        30,
        y + 4,
        'dashboard-choice-label',
        'start'
      );
      label.setAttribute('data-dashboard-index', String(index));
      nodes.push(circle);
    });
  } else if (kind === 'dashboardRotarySwitch') {
    const cx = block.width / 2;
    const cy = block.height / 2;
    block.group.appendChild(
      svg('circle', {
        cx,
        cy,
        r: Math.min(block.width, block.height) * 0.27,
        class: 'dashboard-knob',
      })
    );
    const needle = svg('line', {
      x1: cx,
      y1: cy,
      x2: cx,
      y2: cy - 22,
      class: 'dashboard-knob-needle',
    });
    block.group.appendChild(needle);
    nodes.push(needle);
  } else {
    const track = svg('rect', {
      x: 10,
      y: block.height / 2 - 11,
      width: block.width - 20,
      height: 22,
      rx: 11,
      class: 'dashboard-switch-track',
    });
    const thumb = svg('circle', {
      cx: 22,
      cy: block.height / 2,
      r: 9,
      class: 'dashboard-control-thumb',
    });
    block.group.append(track, thumb);
    nodes.push(thumb);
    if (kind === 'dashboardCheckBox') {
      track.setAttribute('width', '22');
      track.setAttribute('rx', '2');
      thumb.setAttribute('r', '0');
      appendText(
        block.group,
        String(block.params?.Label ?? 'Label'),
        42,
        block.height / 2 + 5,
        'dashboard-choice-label',
        'start'
      );
    }
  }
  const update = (value: number) => {
    const index = Math.max(
      0,
      items.findIndex((item) => Number(item.Value) === Number(value))
    );
    selected = index >= 0 ? index : 0;
    if (kind === 'dashboardComboBox' && comboSelect) comboSelect.selectedIndex = selected;
    else if (kind === 'dashboardRadioButton')
      nodes.forEach((node, i) => node.classList.toggle('selected', i === selected));
    else if (kind === 'dashboardRotarySwitch') {
      const ratio = items.length > 1 ? selected / (items.length - 1) : 0;
      nodes[0].setAttribute(
        'transform',
        `rotate(${-120 + ratio * 240} ${block.width / 2} ${block.height / 2})`
      );
    } else if (kind === 'dashboardCheckBox') {
      nodes[0].parentElement?.classList.toggle('dashboard-checked', selected > 0);
    } else {
      const x = selected > 0 ? block.width - 22 : 22;
      nodes[0].setAttribute('cx', String(x));
      nodes[0].parentElement?.classList.toggle('dashboard-on', selected > 0);
    }
  };
  block.group.setAttribute('tabindex', '0');
  block.group.setAttribute('role', kind === 'dashboardRadioButton' ? 'radiogroup' : 'switch');
  (block.group as Element).addEventListener(
    'pointerup',
    (event) => {
      if ((event as PointerEvent).button !== 0) return;
      if ((event.target as Element | null)?.closest?.('select')) return;
      event.stopPropagation();
      const indexed = (event.target as Element | null)?.closest?.('[data-dashboard-index]');
      const requested = Number(indexed?.getAttribute('data-dashboard-index'));
      const next = Number.isInteger(requested) ? requested : (selected + 1) % items.length;
      const value = Number(items[next]?.Value ?? next);
      update(value);
      sendValue(block, value);
    },
    true
  );
  block.group.addEventListener('keydown', (rawEvent) => {
    const event = rawEvent as KeyboardEvent;
    let next = selected;
    if (event.key === 'ArrowRight' || event.key === 'ArrowDown')
      next = (selected + 1) % items.length;
    else if (event.key === 'ArrowLeft' || event.key === 'ArrowUp')
      next = (selected - 1 + items.length) % items.length;
    else if (event.key === 'Home') next = 0;
    else if (event.key === 'End') next = items.length - 1;
    else if (event.key !== ' ' && event.key !== 'Enter') return;
    else next = (selected + 1) % items.length;
    event.preventDefault();
    event.stopPropagation();
    const value = Number(items[next]?.Value ?? next);
    update(value);
    sendValue(block, value);
  });
  return update;
}
//=============================================================================
function attachPushButton(block: Block, callback: boolean): (value: number) => void {
  const label = appendText(
    block.group,
    String(block.params?.ButtonText ?? (callback ? 'Callback Button' : 'Button')),
    block.width / 2,
    block.height / 2 + 5,
    'dashboard-button-label'
  );
  void label;
  const latchedMode = block.params?.ButtonType === 'Latch';
  let latched = false;
  let prior = Number(designTargetValue(block) ?? 0);
  let delayTimer = 0;
  let repeatTimer = 0;
  const stopPressCallback = () => {
    window.clearTimeout(delayTimer);
    window.clearInterval(repeatTimer);
    delayTimer = 0;
    repeatTimer = 0;
  };
  const startPressCallback = () => {
    stopPressCallback();
    const code = String(block.params?.PressFcn ?? '');
    if (!code) return;
    const delay = Math.max(0, Number(block.params?.PressDelay ?? 500));
    const repeatInterval = Math.max(0, Number(block.params?.RepeatInterval ?? 0));
    delayTimer = window.setTimeout(() => {
      invokeCallback(block, code);
      if (repeatInterval > 0)
        repeatTimer = window.setInterval(() => invokeCallback(block, code), repeatInterval);
    }, delay);
  };
  const begin = () => {
    block.group.classList.add('dashboard-pressed');
    if (callback && !latchedMode) startPressCallback();
    if (!callback && !latchedMode) {
      prior = Number(designTargetValue(block) ?? 0);
      sendValue(block, Number(block.params?.OnValue ?? 1));
    }
  };
  const release = () => {
    if (callback) {
      invokeCallback(block, block.params?.ClickFcn);
      if (latchedMode) {
        latched = !latched;
        if (latched) startPressCallback();
        else stopPressCallback();
      } else stopPressCallback();
    } else if (latchedMode) {
      if (!latched) prior = Number(designTargetValue(block) ?? 0);
      latched = !latched;
      sendValue(block, latched ? Number(block.params?.OnValue ?? 1) : prior);
    } else sendValue(block, prior);
    block.group.classList.toggle('dashboard-pressed', latched);
  };
  const press = (event: PointerEvent) => {
    if (event.button !== 0 || event.shiftKey) return;
    event.stopPropagation();
    begin();
    const onRelease = () => {
      release();
      window.removeEventListener('pointerup', onRelease, true);
    };
    window.addEventListener('pointerup', onRelease, true);
  };
  block.group.setAttribute('tabindex', '0');
  block.group.setAttribute('role', 'button');
  block.group.addEventListener('pointerdown', press as EventListener, true);
  block.group.addEventListener('keydown', (rawEvent) => {
    const event = rawEvent as KeyboardEvent;
    if ((event.key !== ' ' && event.key !== 'Enter') || event.repeat) return;
    event.preventDefault();
    begin();
  });
  block.group.addEventListener('keyup', (rawEvent) => {
    const event = rawEvent as KeyboardEvent;
    if (event.key !== ' ' && event.key !== 'Enter') return;
    event.preventDefault();
    release();
  });
  return () => {};
}
//=============================================================================
function attachEdit(block: Block): (value: number) => void {
  const foreign = svg('foreignObject', {
    x: 5,
    y: 4,
    width: block.width - 10,
    height: block.height - 8,
  });
  const input = document.createElement('input');
  input.type = 'text';
  input.className = 'dashboard-edit-input';
  input.value = String(block.params?.DashboardValue ?? '');
  input.addEventListener('pointerdown', (event) => event.stopPropagation());
  input.addEventListener('change', () => {
    const value = Number(input.value);
    if (Number.isFinite(value)) sendValue(block, value);
  });
  foreign.appendChild(input);
  block.group.appendChild(foreign);
  return (value: number) => {
    input.value = Number.isFinite(value) ? String(value) : '';
  };
}
//=============================================================================
export function dashboardRenderer(block: Block, _helpers: Helpers): void {
  const type = String((block as any).type ?? '');
  block.group.classList.add('dashboard-block');
  applyDashboardOpacity(block);
  (block.group as SVGGraphicsElement).style.setProperty(
    '--dashboard-font-color',
    rgb(block.params?.FontColor, '#111827')
  );
  renderFrame(block);
  let update: (value: any, series?: number[][], times?: number[]) => void = () => {};
  if (type === 'dashboardScope') update = renderScope(block);
  else if (type === 'dashboardDisplay') update = renderDisplay(block);
  else if (
    type === 'dashboardGauge' ||
    type === 'dashboardHalfGauge' ||
    type === 'dashboardQuarterGauge'
  )
    update = renderGauge(block, type);
  else if (type === 'dashboardLinearGauge') update = renderLinearGauge(block);
  else if (type === 'dashboardLamp') update = renderLamp(block);
  else if (type === 'dashboardMultiStateImage') update = renderImage(block);
  else if (type === 'dashboardSlider') update = attachSlider(block);
  else if (type === 'dashboardKnob') update = attachKnob(block);
  else if (type === 'dashboardEdit') update = attachEdit(block);
  else if (type === 'dashboardPushButton') update = attachPushButton(block, false);
  else if (type === 'dashboardCallbackButton') update = attachPushButton(block, true);
  else update = attachStateControl(block, type);

  const initial = Number(block.params?.DashboardValue ?? 0);
  update(initial);
  subscribeDashboardUpdates(block, update);
  block.group.classList.toggle('dashboard-unbound', !bindingOf(block));
}
//=============================================================================
export default dashboardRenderer;
//=============================================================================
