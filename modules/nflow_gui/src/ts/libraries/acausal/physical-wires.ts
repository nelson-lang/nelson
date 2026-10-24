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
// Physical pins are UNDIRECTED: a wire between two of them joins them into an
// electrical / mechanical / thermal node, not a signal source -> sink edge.
// This module converts the editor's undirected connections to the engine's
// `physicalWires` format ({ pins: ["blockId.pinName", ...] } N-ary nets, read by
// nflow_acausal_expand / nflow_planar_expand). It has no DOM dependency.
//=============================================================================
// "<blockId>.<pinName>", the engine wire format.
export type PinKey = string;
export type PhysicalConnection = { a: PinKey; b: PinKey };
export type PhysicalWire = { pins: PinKey[] };
// Union-find over the pin endpoints, grouping like nflow_acausal_expand so the
// exported document round-trips.
export function buildPhysicalWires(connections: PhysicalConnection[]): PhysicalWire[] {
  const parent = new Map<PinKey, PinKey>();
  const find = (x: PinKey): PinKey => {
    let r = x;
    while (parent.get(r) !== r) {
      r = parent.get(r) as PinKey;
    }
    let c = x;
    while (parent.get(c) !== r) {
      const next = parent.get(c) as PinKey;
      parent.set(c, r);
      c = next;
    }
    return r;
  };
  const ensure = (x: PinKey) => {
    if (!parent.has(x)) {
      parent.set(x, x);
    }
  };
  const insertionOrder: PinKey[] = [];
  const seen = new Set<PinKey>();
  const note = (x: PinKey) => {
    if (!seen.has(x)) {
      seen.add(x);
      insertionOrder.push(x);
    }
  };
  for (const c of connections) {
    if (c.a === c.b) {
      continue; // ignore a self-loop
    }
    ensure(c.a);
    ensure(c.b);
    note(c.a);
    note(c.b);
    const ra = find(c.a);
    const rb = find(c.b);
    if (ra !== rb) {
      parent.set(rb, ra);
    }
  }
  // First-seen order keeps the output deterministic.
  const netOf = new Map<PinKey, PinKey[]>();
  const order: PinKey[] = [];
  for (const pin of insertionOrder) {
    const root = find(pin);
    let arr = netOf.get(root);
    if (!arr) {
      arr = [];
      netOf.set(root, arr);
      order.push(root);
    }
    arr.push(pin);
  }
  return order.map((root) => ({ pins: netOf.get(root) as PinKey[] }));
}
//=============================================================================
// A net must be single-domain (an electrical pin cannot share a node with a
// thermal pin). domainOf returns undefined for an unknown pin. Reports the first
// offending net.
export function validateNetDomains(
  wires: PhysicalWire[],
  domainOf: (pin: PinKey) => string | undefined
): { ok: boolean; error?: string; net?: PhysicalWire } {
  for (const w of wires) {
    let domain: string | undefined;
    for (const pin of w.pins) {
      const d = domainOf(pin);
      if (d === undefined) {
        continue;
      }
      if (domain === undefined) {
        domain = d;
      } else if (d !== domain) {
        return {
          ok: false,
          error: `Physical net joins two domains (${domain} and ${d}); connect only same-domain pins.`,
          net: w,
        };
      }
    }
  }
  return { ok: true };
}
//=============================================================================
// Physical = both endpoints are physical pins; everything else is signal.
export function partitionConnections(
  connections: PhysicalConnection[],
  isPhysical: (pin: PinKey) => boolean
): { physical: PhysicalConnection[]; signal: PhysicalConnection[] } {
  const physical: PhysicalConnection[] = [];
  const signal: PhysicalConnection[] = [];
  for (const c of connections) {
    if (isPhysical(c.a) && isPhysical(c.b)) {
      physical.push(c);
    } else {
      signal.push(c);
    }
  }
  return { physical, signal };
}
//=============================================================================
export function pinKey(blockId: string | number, pinName: string): PinKey {
  return `${blockId}.${pinName}`;
}
//=============================================================================
// Splits at the last dot, so block ids may contain dots.
export function splitPinKey(key: PinKey): { blockId: string; pinName: string } {
  const i = key.lastIndexOf('.');
  if (i < 0) {
    return { blockId: key, pinName: '' };
  }
  return { blockId: key.slice(0, i), pinName: key.slice(i + 1) };
}
//=============================================================================
