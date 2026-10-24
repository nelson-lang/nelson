//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Live member tree for the Bus Selector inspector: lists the members of the
// bus feeding the block (rich descriptor from the last simulation via
// signalBus, static busCreator MemberNames as a pre-sim fallback) with one
// checkbox per selectable path, kept in sync with the SelectedSignals param.
//=============================================================================
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import type { BusInfo, BusMemberInfo } from '../../sim/parse-results.js';
import * as FieldNames from '../../FieldNames.js';
import { state } from '../../app/state.js';
import { getSignalBusInfo } from '../../app/signal-badges.js';
import { onAppEvent } from '../../app/app-events.js';
//=============================================================================
export type BusTreeNode = {
  /** Dotted selection path ("pos", "sub.a"). */
  path: string;
  label: string;
  /** Short descriptor ("double [3]", "int64", "bus Inner"). */
  meta: string;
  isBus: boolean;
  children: BusTreeNode[];
};
//=============================================================================
function memberMeta(m: BusMemberInfo): string {
  if (m.bus) return 'bus' + (m.bus.name ? ' ' + m.bus.name : '');
  let t = m.type || 'double';
  if (m.complex) t += ' complex';
  const dims = Array.isArray(m.dims) ? m.dims.map((d) => Math.max(1, Number(d) || 1)) : [];
  while (dims.length > 1 && dims[dims.length - 1] === 1) dims.pop();
  if (dims.length >= 1 && !(dims.length === 1 && dims[0] <= 1)) t += ' [' + dims.join('x') + ']';
  else if ((m.width ?? 1) > 1) t += ' [' + m.width + ']';
  return t;
}
//=============================================================================
function memberNodes(members: BusMemberInfo[], prefix: string): BusTreeNode[] {
  const out: BusTreeNode[] = [];
  for (const m of members || []) {
    if (!m || !m.name) continue;
    const path = prefix ? prefix + '.' + m.name : m.name;
    out.push({
      path,
      label: m.name,
      meta: memberMeta(m),
      isBus: Boolean(m.bus),
      children: m.bus ? memberNodes(m.bus.members || [], path) : [],
    });
  }
  return out;
}
//=============================================================================
/** Pure tree builder: rich BusInfo wins, flat MemberNames fallback otherwise. */
export function buildBusMemberTree(
  busInfo: BusInfo | null,
  fallbackMemberNames: string[] | null
): BusTreeNode[] {
  if (busInfo && Array.isArray(busInfo.members) && busInfo.members.length) {
    return memberNodes(busInfo.members, '');
  }
  if (Array.isArray(fallbackMemberNames)) {
    return fallbackMemberNames
      .map((n) => String(n ?? '').trim())
      .filter(Boolean)
      .map((n) => ({ path: n, label: n, meta: '', isBus: false, children: [] }));
  }
  return [];
}
//=============================================================================
/** Bus descriptor + static fallback for the wire feeding (block, input 0). */
export function upstreamBusSources(block: Block): {
  busInfo: BusInfo | null;
  fallbackMemberNames: string[] | null;
  connected: boolean;
} {
  const conns = (state.connections as Array<Record<string, unknown>>) || [];
  const conn = conns.find((c) => String(c.to) === String(block.id) && Number(c.toIndex ?? 0) === 0);
  if (!conn) return { busInfo: null, fallbackMemberNames: null, connected: false };
  const busInfo = getSignalBusInfo(conn.from, conn.fromIndex ?? 0) as BusInfo | null;
  let fallback: string[] | null = null;
  const upstream = (state.blocks as Map<unknown, Block>).get(conn.from as never);
  if (upstream && String(upstream.type) === 'busCreator') {
    const names = upstream.params?.[FieldNames.kMemberNames];
    if (Array.isArray(names)) fallback = names.map((n) => String(n));
  }
  return { busInfo, fallbackMemberNames: fallback, connected: true };
}
//=============================================================================
function selectedPaths(block: Block): string[] {
  const v = block.params?.[FieldNames.kSelectedSignals];
  return Array.isArray(v) ? v.map((s) => String(s)) : [];
}
//=============================================================================
function togglePath(block: Block, deps: BlockPanelDeps, path: string, on: boolean): void {
  const cur = selectedPaths(block).filter((p) => p !== path);
  if (on) cur.push(path);
  if (!block.params) block.params = {} as Block['params'];
  (block.params as Record<string, unknown>)[FieldNames.kSelectedSignals] = cur;
  const ta = deps.inspectorBody?.querySelector<HTMLTextAreaElement>(
    `textarea[data-edit='${FieldNames.kSelectedSignals}']`
  );
  if (ta) ta.value = cur.join('\n');
  deps.callUpdate?.(block);
}
//=============================================================================
function renderNodes(
  container: HTMLElement,
  nodes: BusTreeNode[],
  block: Block,
  deps: BlockPanelDeps
): void {
  const selected = new Set(selectedPaths(block));
  for (const node of nodes) {
    const row = document.createElement('label');
    row.className = 'bus-tree-row';
    const cb = document.createElement('input');
    cb.type = 'checkbox';
    cb.checked = selected.has(node.path);
    cb.setAttribute('data-bus-path', node.path);
    cb.addEventListener('change', () => togglePath(block, deps, node.path, cb.checked));
    const name = document.createElement('span');
    name.className = 'bus-tree-name';
    name.textContent = node.label;
    row.appendChild(cb);
    row.appendChild(name);
    if (node.meta) {
      const meta = document.createElement('span');
      meta.className = 'bus-tree-meta';
      meta.textContent = node.meta;
      row.appendChild(meta);
    }
    container.appendChild(row);
    if (node.children.length) {
      const kids = document.createElement('div');
      kids.className = 'bus-tree-children';
      renderNodes(kids, node.children, block, deps);
      container.appendChild(kids);
    }
  }
}
//=============================================================================
function fillTreeBody(body: HTMLElement, block: Block, deps: BlockPanelDeps): void {
  body.innerHTML = '';
  const { busInfo, fallbackMemberNames, connected } = upstreamBusSources(block);
  const nodes = buildBusMemberTree(busInfo, fallbackMemberNames);
  if (nodes.length) {
    renderNodes(body, nodes, block, deps);
    return;
  }
  const hint = document.createElement('div');
  hint.className = 'bus-tree-hint';
  hint.textContent = connected
    ? 'Run a simulation to list the bus members.'
    : 'Connect a bus signal to list its members.';
  body.appendChild(hint);
}
//=============================================================================
/** Append the member tree below the generic busSelector fields. */
export function appendBusMemberTree(block: Block, deps: BlockPanelDeps): void {
  const host = deps.inspectorBody;
  if (!host) return;
  const container = document.createElement('div');
  container.className = 'bus-member-tree';
  container.setAttribute('data-bus-tree', '');
  const title = document.createElement('span');
  title.className = 'bus-tree-title';
  title.textContent = 'Bus members';
  container.appendChild(title);
  const body = document.createElement('div');
  body.setAttribute('data-bus-tree-body', '');
  container.appendChild(body);
  fillTreeBody(body, block, deps);
  host.appendChild(container);

  // Refresh once a simulation finishes (signalBus becomes available); the
  // listener removes itself when the inspector has moved on.
  const off = onAppEvent('nf-sim-time', (detail) => {
    if (!container.isConnected) {
      off();
      return;
    }
    if (detail?.done === true) fillTreeBody(body, block, deps);
  });
}
//=============================================================================
