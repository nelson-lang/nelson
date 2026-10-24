//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { state, markDirty, signalDiagramChanged } from './state.js';
import { writeStatus } from './status-service.js';
import { serializeBlock, serializeConnection, serializeDiagram } from './file-operations.js';
import { captureRoutePointsSnapshot } from '../router/route-points.js';
import { collectExternalPorts } from '../utils/subsystem-ports.js';
import { t } from '../utils/i18n.js';
import * as FieldNames from '../FieldNames.js';
import {
  loadDiagram,
  updateSubsystemNavUi,
  getRendererRef,
  getStatusEl,
  getRuntimeInput,
  connectionKey,
} from './diagram-handlers.js';
//=============================================================================
const deepClone = <T>(value: T): T => JSON.parse(JSON.stringify(value));
type ConnectionLikeLocal = {
  from: unknown;
  fromIndex?: unknown;
  to: unknown;
  toIndex?: unknown;
  points?: unknown;
};
//=============================================================================
const captureUiState = () => {
  const routePoints = captureRoutePointsSnapshot(
    state.connections as unknown as ConnectionLikeLocal[],
    connectionKey
  );
  const scopeState: Record<string, unknown> = {};
  state.blocks.forEach((block: any) => {
    const b = block as unknown as Record<string, unknown>;
    const idKey = String(b.id);
    if (b.type === 'scope' && b.scopeData) {
      scopeState[idKey] = { kind: 'scope', data: deepClone(b.scopeData) };
    } else if (b.type === 'xyScope' && b.xyScopeData) {
      scopeState[idKey] = { kind: 'xyScope', data: deepClone(b.xyScopeData) };
    } else if (b.type === 'xyzScope' && b.xyzScopeData) {
      scopeState[idKey] = { kind: 'xyzScope', data: deepClone(b.xyzScopeData) };
    }
  });
  return { routePoints, scopeState };
};
//=============================================================================
const buildSubsystemSpec = (data: unknown, fallbackName = 'Subsystem') => {
  const blocks: Array<unknown> = Array.isArray((data as Record<string, unknown>)?.blocks)
    ? (deepClone((data as Record<string, unknown>).blocks) as Array<unknown>)
    : [];
  const connections: Array<unknown> = Array.isArray((data as Record<string, unknown>)?.connections)
    ? (deepClone((data as Record<string, unknown>).connections) as Array<unknown>)
    : [];
  // serializeDiagram lifts the inner canvas's physical connections into
  // physicalWires; they must reach the stored spec, or closing the subsystem view
  // drops the nets and the engine sees a floating (singular) island.
  const physicalWires: Array<unknown> = Array.isArray(
    (data as Record<string, unknown>)?.physicalWires
  )
    ? (deepClone((data as Record<string, unknown>).physicalWires) as Array<unknown>)
    : [];
  if (!blocks.length) throw new Error('Subsystem YAML has no blocks');
  const name =
    String((data as Record<string, unknown>).name || fallbackName).trim() || fallbackName;
  const externalInputs = collectExternalPorts(blocks, FieldNames.kLabelSource);
  const externalOutputs = collectExternalPorts(blocks, FieldNames.kLabelSink);
  if (!externalInputs.length && !externalOutputs.length) {
    throw new Error("No external ports found. Mark label blocks with 'Is external port'");
  }
  const spec: Record<string, unknown> = {
    name,
    blocks,
    connections,
    externalInputs,
    externalOutputs,
  };
  if (physicalWires.length) {
    spec.physicalWires = physicalWires;
  }
  return spec;
};
//=============================================================================
export async function openSubsystemFromBlock(block: unknown): Promise<void> {
  const rendererRef = getRendererRef();
  const statusEl = getStatusEl();
  const runtimeInput = getRuntimeInput();

  const blk = block as unknown as Record<string, unknown> | null;
  if (!blk || (blk as Record<string, unknown>).type !== FieldNames.kSubsystem) {
    return;
  }
  let spec: any = ((blk as Record<string, unknown>).params as Record<string, unknown> | undefined)
    ?.subsystem;
  if (
    !spec ||
    !Array.isArray((spec as Record<string, unknown>)?.blocks) ||
    !Array.isArray((spec as Record<string, unknown>)?.connections)
  ) {
    const fallbackName = String(
      ((blk as Record<string, unknown>).params as Record<string, unknown> | undefined)?.name ||
        ((blk as Record<string, unknown>).params as Record<string, unknown> | undefined)?.label ||
        'Subsystem'
    );
    const placeholderBlockId = `s_${Date.now().toString(36)}`;
    const defaultSpec = {
      name: fallbackName,
      blocks: [
        {
          id: placeholderBlockId,
          type: 'constant',
          x: 100,
          y: 100,
          width: 60,
          height: 40,
          params: { value: 0 },
        },
      ],
      connections: [],
    };
    // The host block must store the new spec, or edits are lost.
    try {
      (blk as Record<string, unknown>).params =
        (blk as Record<string, unknown>).params || ({} as Record<string, unknown>);
      ((blk as Record<string, unknown>).params as Record<string, unknown>).subsystem = defaultSpec;
    } catch (_e) {}
    spec = defaultSpec;
    writeStatus(statusEl, t('Opened empty subsystem for editing.'));
  }
  const snapshot = serializeDiagram(state);
  const parentUiState = captureUiState();
  state.subsystemStack.push({
    parentDiagram: snapshot,
    parentUiState,
    hostBlockId: (blk as unknown as Record<string, unknown>).id,
  });
  updateSubsystemNavUi();
  // loadDiagram rebuilds the drawable physical connections from physicalWires;
  // without them an acausal sub-circuit shows blocks but no wires, and the nets
  // are lost on close. They are re-lifted to physicalWires on save.
  const innerPhysicalWires = Array.isArray((spec as Record<string, unknown>).physicalWires)
    ? deepClone((spec as Record<string, unknown>).physicalWires)
    : [];
  await loadDiagram(
    {
      name: String(
        (spec as Record<string, unknown>)?.name ||
          ((blk as Record<string, unknown>).params as Record<string, unknown> | undefined)?.name ||
          'Subsystem'
      ),
      blocks: deepClone((spec as Record<string, unknown>).blocks),
      connections: deepClone((spec as Record<string, unknown>).connections),
      physicalWires: innerPhysicalWires,
      variables: snapshot.variables || '',
      sampleTime: Number(snapshot.sampleTime) || state.sampleTime || 0.01,
      runtime: Number(snapshot.runtime) || Number(runtimeInput?.value) || 1,
    },
    { preserveSubsystemStack: true }
  );
  if (rendererRef?.current) {
    rendererRef.current.selectBlock?.(null as unknown as number);
  }
  // Wires need a render pass once the DOM has settled.
  requestAnimationFrame(() => {
    const rr = getRendererRef();
    if (rr?.current?.updateConnections) {
      rr.current.updateConnections(true);
    }
    requestAnimationFrame(() => {
      if (rr?.current?.forceFullRoute) {
        rr.current.forceFullRoute(10000);
      }
    });
  });
  writeStatus(
    statusEl,
    t('Opened subsystem: {name}', {
      name: String(
        ((blk as Record<string, unknown>).params as Record<string, unknown> | undefined)?.name ||
          'Subsystem'
      ),
    })
  );
}
//=============================================================================
export async function closeSubsystemView(): Promise<void> {
  const rendererRef = getRendererRef();
  const statusEl = getStatusEl();

  if (!state.subsystemStack.length) return;
  const innerSnapshot = serializeDiagram(state);
  const entry = state.subsystemStack.pop() as unknown as Record<string, unknown>;
  await loadDiagram(entry.parentDiagram as unknown as Record<string, unknown>, {
    preserveSubsystemStack: true,
    restoreUiState: deepClone(entry.parentUiState || null),
  });
  const host = state.blocks.get(entry.hostBlockId as unknown as number);
  if (host && (host as unknown as Record<string, unknown>).type === FieldNames.kSubsystem) {
    let spec: unknown;
    try {
      spec = buildSubsystemSpec(
        innerSnapshot,
        String(
          (
            (host as unknown as Record<string, unknown>).params as
              | Record<string, unknown>
              | undefined
          )?.name || 'Subsystem'
        )
      );
    } catch (error) {
      spec = {
        name: String(
          (
            (host as unknown as Record<string, unknown>).params as
              | Record<string, unknown>
              | undefined
          )?.name ||
            (innerSnapshot as unknown as Record<string, unknown>).name ||
            'Subsystem'
        ),
        blocks: deepClone(
          ((innerSnapshot as unknown as Record<string, unknown>).blocks as Array<unknown>) || []
        ),
        connections: deepClone(
          ((innerSnapshot as unknown as Record<string, unknown>).connections as Array<unknown>) ||
            []
        ),
        externalInputs: [],
        externalOutputs: [],
      };
    }
    (host as unknown as Record<string, unknown>).params =
      (host as unknown as Record<string, unknown>).params || ({} as Record<string, unknown>);
    ((host as unknown as Record<string, unknown>).params as Record<string, unknown>).subsystem =
      spec;
    ((host as unknown as Record<string, unknown>).params as Record<string, unknown>).name = (
      spec as unknown as Record<string, unknown>
    ).name;
    markDirty();
    if (rendererRef?.current) {
      rendererRef.current.updateBlockLabel?.(host as unknown as number);
    }
  }
  updateSubsystemNavUi();
  writeStatus(statusEl, t('Closed subsystem view'));
}
//=============================================================================
export function createSubsystemFromSelection(): void {
  const rendererRef = getRendererRef();
  const statusEl = getStatusEl();

  const selIds: number[] = Array.from(
    state.selectedIds && state.selectedIds.size
      ? state.selectedIds
      : state.selectedId != null
      ? [state.selectedId]
      : []
  ) as number[];

  if (selIds.length < 2) {
    writeStatus(statusEl, t('Select 2 or more blocks to create a subsystem.'));
    return;
  }

  const selSet = new Set(selIds.map((id) => String(id)));

  const selectedBlocks: Array<Record<string, unknown>> = [];
  for (const id of selIds) {
    const b = state.blocks.get(id) as Record<string, unknown> | undefined;
    if (!b) return;
    selectedBlocks.push(b);
  }

  type RawConn = Record<string, unknown>;
  const allConns = state.connections as RawConn[];

  const incomingBoundary: RawConn[] = [];
  const outgoingBoundary: RawConn[] = [];
  const internalConns: RawConn[] = [];

  for (const c of allConns) {
    const fromKey = String(c.from);
    const toKey = String(c.to);
    const fromIn = selSet.has(fromKey);
    const toIn = selSet.has(toKey);
    if (fromIn && toIn) {
      internalConns.push(c);
    } else if (!fromIn && toIn) {
      incomingBoundary.push(c);
    } else if (fromIn && !toIn) {
      outgoingBoundary.push(c);
    }
  }

  // Sort by block y for stable port ordering. Block ids may be strings (named ids
  // from .nflow files), so do not convert them with Number().
  incomingBoundary.sort((a, b) => {
    const blkA = state.blocks.get(a.to as number) as Record<string, unknown> | undefined;
    const blkB = state.blocks.get(b.to as number) as Record<string, unknown> | undefined;
    return (Number(blkA?.y) || 0) - (Number(blkB?.y) || 0);
  });
  outgoingBoundary.sort((a, b) => {
    const blkA = state.blocks.get(a.from as number) as Record<string, unknown> | undefined;
    const blkB = state.blocks.get(b.from as number) as Record<string, unknown> | undefined;
    return (Number(blkA?.y) || 0) - (Number(blkB?.y) || 0);
  });

  let minX = Infinity,
    minY = Infinity,
    maxX = -Infinity,
    maxY = -Infinity;
  for (const b of selectedBlocks) {
    const bx = Number(b.x) || 0;
    const by = Number(b.y) || 0;
    const bw = Number(b.width) || 60;
    const bh = Number(b.height) || 40;
    if (bx < minX) minX = bx;
    if (by < minY) minY = by;
    if (bx + bw > maxX) maxX = bx + bw;
    if (by + bh > maxY) maxY = by + bh;
  }
  const centerX = (minX + maxX) / 2;
  const centerY = (minY + maxY) / 2;

  // selIds may be strings, so Math.max(0, ...selIds) would give NaN: use a
  // timestamp+counter string id for the inner label blocks.
  const _lblEpoch = Date.now().toString(36);
  let _lblSeq = 0;
  const nextInnerId = () => `ep_${_lblEpoch}_${_lblSeq++}`;

  // The selection's top-left maps to (INNER_PAD, INNER_PAD) inside the subsystem.
  const INNER_PAD = 100;
  const offsetX = minX - INNER_PAD;
  const offsetY = minY - INNER_PAD;

  const getPortDisplayName = (block: Record<string, unknown> | undefined): string => {
    if (!block) return '';
    const label = String(block.label ?? '').trim();
    if (label) return label;
    const paramsName = String(
      (block.params as Record<string, unknown> | undefined)?.name ?? ''
    ).trim();
    return paramsName;
  };
  const usedPortNames = new Set<string>();
  const uniquePortName = (name: string): string => {
    if (!usedPortNames.has(name)) {
      usedPortNames.add(name);
      return name;
    }
    let _n = 2;
    while (usedPortNames.has(`${name}_${_n}`)) _n++;
    const unique = `${name}_${_n}`;
    usedPortNames.add(unique);
    return unique;
  };

  const innerBlocks: Array<Record<string, unknown>> = selectedBlocks.map((b) => {
    const clone = deepClone(serializeBlock(b));
    clone.x = (Number(clone.x) || 0) - offsetX;
    clone.y = (Number(clone.y) || 0) - offsetY;
    return clone;
  });
  const innerConnections: Array<Record<string, unknown>> = internalConns.map((connection) =>
    serializeConnection(connection)
  );

  for (let i = 0; i < incomingBoundary.length; i++) {
    const conn = incomingBoundary[i];
    const targetBlock = state.blocks.get(conn.to as number) as Record<string, unknown> | undefined;
    const srcBlock = state.blocks.get(conn.from as number) as Record<string, unknown> | undefined;
    const rawPortName = getPortDisplayName(srcBlock);
    const portName = uniquePortName(rawPortName || `in${i + 1}`);
    const labelId = nextInnerId();
    const labelBlock: Record<string, unknown> = {
      id: labelId,
      type: FieldNames.kLabelSource,
      x: minX - 80 - offsetX,
      y: (Number(targetBlock?.y) || minY) - offsetY,
      width: 40,
      height: 40,
      params: {
        name: portName,
        [FieldNames.kIsExternalPort]: true,
      },
    };
    innerBlocks.push(labelBlock);
    innerConnections.push({
      from: labelId,
      fromIndex: 0,
      to: conn.to,
      toIndex: conn.toIndex ?? 0,
    });
  }

  for (let j = 0; j < outgoingBoundary.length; j++) {
    const conn = outgoingBoundary[j];
    const sourceBlock = state.blocks.get(conn.from as number) as
      | Record<string, unknown>
      | undefined;
    const tgtBlock = state.blocks.get(conn.to as number) as Record<string, unknown> | undefined;
    const rawPortName = getPortDisplayName(tgtBlock);
    const portName = uniquePortName(rawPortName || `out${j + 1}`);
    const labelId = nextInnerId();
    const labelBlock: Record<string, unknown> = {
      id: labelId,
      type: FieldNames.kLabelSink,
      x: maxX + 80 - offsetX,
      y: (Number(sourceBlock?.y) || minY) - offsetY,
      width: 40,
      height: 40,
      params: {
        name: portName,
        [FieldNames.kIsExternalPort]: true,
        showNode: true,
      },
    };
    innerBlocks.push(labelBlock);
    innerConnections.push({
      from: conn.from,
      fromIndex: conn.fromIndex ?? 0,
      to: labelId,
      toIndex: 0,
    });
  }

  const externalInputs = collectExternalPorts(innerBlocks, FieldNames.kLabelSource);
  const externalOutputs = collectExternalPorts(innerBlocks, FieldNames.kLabelSink);

  const subsystemSpec = {
    name: 'Subsystem',
    blocks: innerBlocks,
    connections: innerConnections,
    externalInputs,
    externalOutputs,
  };

  if (!rendererRef?.current) return;
  const renderer = rendererRef.current;

  // Commit the current state to history before mutating the diagram.
  try {
    window.dispatchEvent(new Event('diagramCommitted'));
  } catch (_e) {}

  const newBlock = renderer.createBlock?.(FieldNames.kSubsystem, centerX, centerY, {
    params: {
      name: 'Subsystem',
      externalInputs,
      externalOutputs,
      subsystem: subsystemSpec,
    },
  }) as Record<string, unknown> | null | undefined;

  if (!newBlock) {
    writeStatus(statusEl, t('Failed to create subsystem block.'));
    return;
  }
  const subsystemId = newBlock.id as number;

  // Deleting a block also removes its connections from state.
  for (const id of selIds) {
    renderer.deleteBlock?.(id);
  }

  for (let i = 0; i < incomingBoundary.length; i++) {
    const conn = incomingBoundary[i];
    renderer.createConnection?.(conn.from as number, subsystemId, i, Number(conn.fromIndex ?? 0));
  }
  for (let j = 0; j < outgoingBoundary.length; j++) {
    const conn = outgoingBoundary[j];
    renderer.createConnection?.(subsystemId, conn.to as number, Number(conn.toIndex ?? 0), j);
  }

  state.selectedIds = new Set();
  state.selectedId = null;
  markDirty();

  // Commit at once so the whole creation is a single undo step.
  try {
    signalDiagramChanged();
  } catch (_e) {}
  try {
    window.dispatchEvent(new Event('diagramCommitted'));
  } catch (_e) {}

  writeStatus(statusEl, t('Created subsystem from selection.'));
}
//=============================================================================
