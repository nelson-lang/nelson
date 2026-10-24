//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test } from 'vitest';
import {
  sanitizeFilename,
  sanitizeParamsForSave,
  parseDiagram,
  serializeBlock,
  serializeConnection,
  serializeDiagram,
  serializeDiagramForSave,
  commitSerializedSaveMetadata,
  serializeNBlock,
} from './file-operations.js';
import { state } from './state';
//=============================================================================
describe('file-operations utilities', () => {
  test('sanitizeFilename replaces unsafe chars', () => {
    expect(sanitizeFilename('My File.nflow')).toBe('My_File_nflow');
    expect(sanitizeFilename('   ')).toBe('nflow');
    expect(sanitizeFilename(null)).toBe('nflow');
  });

  test('sanitizeParamsForSave strips empty _visible entries and keeps truthy', () => {
    const params = {
      _visible: {
        '{}': true,
        a: false,
        b: 1,
        c: null,
      },
      other: 'x',
    };
    const out = sanitizeParamsForSave(params);
    expect(out).toHaveProperty('other', 'x');
    expect(out).not.toHaveProperty('_visible');
  });

  test('parseDiagram uses JSON for .nflow filenames', () => {
    const jsonText = JSON.stringify({ x: 1 });
    expect(parseDiagram(jsonText, 'file.nflow')).toEqual({ x: 1 });
  });

  test('parseDiagram preserves signed block coordinates', () => {
    const jsonText = JSON.stringify({
      blocks: [{ id: 'src', type: 'sine', x: -40, y: -20 }],
      connections: [],
    });
    const parsed = parseDiagram(jsonText, 'signed.nflow') as any;
    expect(parsed.blocks[0].x).toBe(-40);
    expect(parsed.blocks[0].y).toBe(-20);
  });

  test('parseDiagram treats .nblock as JSON', () => {
    const jsonText = JSON.stringify({ a: 2 });
    expect(parseDiagram(jsonText, 'file.nblock')).toEqual({ a: 2 });
  });

  test('parseDiagram accepts the current nflow-2 and nblock-2 formats', () => {
    expect(parseDiagram(JSON.stringify({ format: 'nflow-2', blocks: [] }), 'a.nflow')).toEqual({
      format: 'nflow-2',
      blocks: [],
    });
    expect(parseDiagram(JSON.stringify({ format: 'nblock-2', blocks: [] }), 'a.nblock')).toEqual({
      format: 'nblock-2',
      blocks: [],
    });
  });

  test('parseDiagram rejects legacy nflow-1 files', () => {
    const jsonText = JSON.stringify({ format: 'nflow-1', blocks: [] });
    expect(() => parseDiagram(jsonText, 'old.nflow')).toThrow(/legacy nflow-1/);
  });

  test('parseDiagram rejects a newer/unknown format with a clear error', () => {
    const jsonText = JSON.stringify({ format: 'nflow-3', blocks: [] });
    expect(() => parseDiagram(jsonText, 'future.nflow')).toThrow(
      /newer version of nflow.*up to "nflow-2"/
    );
  });

  test('serializeNBlock produces expected fields', () => {
    const base = { version: '1.0.0', format: 'nflow-1', blocks: [], connections: [] } as any;
    const out = serializeNBlock(base, [{ name: 'u1' }], [{ name: 'y' }], true) as any;
    expect(out.format).toBe('nblock-2');
    expect(out.inputs).toEqual([{ name: 'u1' }]);
    expect(out.outputs).toEqual([{ name: 'y' }]);
    expect(out.masked).toBe(true);
  });

  test('serializeNBlock includes description and metadata author when extra provided', () => {
    const base = { version: '1.0.0', format: 'nflow-1', blocks: [], connections: [] } as any;
    const out = serializeNBlock(base, [{ name: 'u1' }], [{ name: 'y' }], false, {
      description: 'MyDesc',
      author: 'Alice',
    }) as any;
    expect(out.format).toBe('nblock-2');
    expect(out.description).toBe('MyDesc');
    expect(out.metadata).toBeDefined();
    expect((out.metadata as any).author).toBe('Alice');
  });

  test('serializeNBlock falls back to diagramData description/metadata when extra not provided', () => {
    const base = {
      version: '1.0.0',
      format: 'nflow-1',
      description: 'DiagDesc',
      metadata: { author: 'Bob', created: '2020-01-01' },
      blocks: [],
      connections: [],
    } as any;
    const out = serializeNBlock(base, [], [], false) as any;
    expect(out.format).toBe('nblock-2');
    expect(out.description).toBe('DiagDesc');
    expect((out.metadata as any).author).toBe('Bob');
  });

  test('serializeNBlock includes variables when non-empty and omits when empty', () => {
    const withVars = {
      version: '1.0.0',
      format: 'nflow-1',
      blocks: [],
      connections: [],
      variables: 'a=1',
    } as any;
    const outWithVars = serializeNBlock(withVars, [], [], false) as any;
    expect(outWithVars).toHaveProperty('variables', 'a=1');

    const emptyVars = {
      version: '1.0.0',
      format: 'nflow-1',
      blocks: [],
      connections: [],
      variables: '',
    } as any;
    const outEmptyVars = serializeNBlock(emptyVars, [], [], false) as any;
    expect(outEmptyVars).not.toHaveProperty('variables');
  });

  test('serializeNBlock carries maskParameters so a reusable block round-trips its params', () => {
    const base = { version: '1.0.0', format: 'nflow-1', blocks: [], connections: [] } as any;
    const withMask = serializeNBlock(base, [], [], true, {
      maskParameters: { K: 5, Ti: 0.2 },
    }) as any;
    expect(withMask.maskParameters).toEqual({ K: 5, Ti: 0.2 });

    const noMask = serializeNBlock(base, [], [], true, {}) as any;
    expect(noMask).not.toHaveProperty('maskParameters');

    const emptyMask = serializeNBlock(base, [], [], true, { maskParameters: {} }) as any;
    expect(emptyMask).not.toHaveProperty('maskParameters');
  });

  test('serializeNBlock includes sampleTime when defined (including 0) and omits when undefined', () => {
    const withSample = {
      version: '1.0.0',
      format: 'nflow-1',
      blocks: [],
      connections: [],
      sampleTime: 0,
    } as any;
    const outWithSample = serializeNBlock(withSample, [], [], false) as any;
    expect(outWithSample.sampleTime).toBe(0);

    const noSample = { version: '1.0.0', format: 'nflow-1', blocks: [], connections: [] } as any;
    const outNoSample = serializeNBlock(noSample, [], [], false) as any;
    expect(outNoSample).not.toHaveProperty('sampleTime');
  });

  test('serializeNBlock trims and includes canvasBackground when non-empty', () => {
    const withCanvas = {
      version: '1.0.0',
      format: 'nflow-1',
      blocks: [],
      connections: [],
      canvasBackground: '  #abc ',
    } as any;
    const out = serializeNBlock(withCanvas, [], [], false) as any;
    expect(out.canvasBackground).toBe('#abc');

    const blankCanvas = {
      version: '1.0.0',
      format: 'nflow-1',
      blocks: [],
      connections: [],
      canvasBackground: '   ',
    } as any;
    const outBlank = serializeNBlock(blankCanvas, [], [], false) as any;
    expect(outBlank).not.toHaveProperty('canvasBackground');
  });

  test('serializeNBlock uses exposedParams when provided and otherwise uses diagram params (without sanitization)', () => {
    const base = {
      version: '1.0.0',
      format: 'nflow-1',
      blocks: [],
      connections: [],
      params: { foo: 'bar', _visible: { a: true } },
    } as any;
    const outBase = serializeNBlock(base, [], [], false) as any;
    expect(outBase.params).toHaveProperty('foo', 'bar');
    // Note: serializeNBlock does not run sanitizeParamsForSave, so `_visible` stays
    expect(outBase.params).toHaveProperty('_visible');

    const extra = {
      version: '1.0.0',
      format: 'nflow-1',
      blocks: [],
      connections: [],
      params: { foo: 'bar' },
    } as any;
    const outExtra = serializeNBlock(extra, [], [], false, {
      exposedParams: { exposed: true },
    }) as any;
    expect(outExtra.params).toEqual({ exposed: true });
  });

  test('serializeDiagram includes locked field', () => {
    state.blocks = new Map();
    state.connections = [];
    state.diagramName = 't';
    state.locked = true;
    const out = serializeDiagram(state as any);
    expect(out.locked).toBe(true);
  });

  test('serializeDiagram preserves creation and saved metadata without mutating state', () => {
    state.blocks = new Map();
    state.connections = [];
    state.modelVersion = '1.2.0';
    state.author = 'Alice';
    state.createdBy = 'Carol';
    state.created = '2024-01-02T03:04:05.000Z';
    state.lastSavedBy = 'Bob';
    state.lastSavedAt = '2025-02-03T04:05:06.000Z';

    const out = serializeDiagram(state as any) as any;

    expect(out.version).toBe('1.2.0');
    expect(out.metadata).toMatchObject({
      author: 'Alice',
      createdBy: 'Carol',
      created: '2024-01-02T03:04:05.000Z',
      lastSavedBy: 'Bob',
      lastSavedAt: '2025-02-03T04:05:06.000Z',
    });
    expect(state.created).toBe('2024-01-02T03:04:05.000Z');
  });

  test('save serialization stamps the current author and commits only on request', () => {
    state.blocks = new Map();
    state.connections = [];
    state.modelVersion = '1.0.0';
    state.currentUser = 'Carol';
    state.author = 'Alice';
    state.created = '2024-01-02T03:04:05.000Z';
    state.lastSavedBy = '';
    state.lastSavedAt = '';

    const out = serializeDiagramForSave(state as any, '2026-08-08T18:35:29.000Z') as any;

    expect(out.metadata.lastSavedBy).toBe('Carol');
    expect(out.metadata.lastSavedAt).toBe('2026-08-08T18:35:29.000Z');
    expect(state.lastSavedAt).toBe('');

    commitSerializedSaveMetadata(state as any, out);
    expect(state.lastSavedBy).toBe('Carol');
    expect(state.lastSavedAt).toBe('2026-08-08T18:35:29.000Z');
  });

  test('serializeDiagram preserves signed block coordinates', () => {
    state.blocks = new Map([
      [
        'src',
        {
          id: 'src',
          type: 'sine',
          x: -40,
          y: -20,
          rotation: 0,
          params: {},
        },
      ],
    ] as any);
    state.connections = [];
    state.diagramName = 'signed';
    const out = serializeDiagram(state as any) as any;
    expect(out.blocks[0].x).toBe(-40);
    expect(out.blocks[0].y).toBe(-20);
  });

  test('serializeBlock ignores circular renderer state', () => {
    const block: Record<string, any> = {
      id: 'gain',
      type: 'gain',
      x: 21,
      y: 39,
      params: { Gain: 2 },
    };
    block.renderSegments = [{ owner: block }];

    expect(serializeBlock(block)).toEqual({
      id: 'gain',
      type: 'gain',
      x: 21,
      y: 39,
      rotation: 0,
      params: { Gain: 2 },
    });
  });

  test('serializeConnection ignores circular renderer state', () => {
    const connection: Record<string, any> = {
      from: 'source',
      to: 'sink',
      fromIndex: 0,
      toIndex: 1,
      points: [
        { x: 1, y: 2 },
        { x: 3, y: 4, pin: true },
      ],
    };
    connection.renderSegments = [{ owner: connection }];

    expect(serializeConnection(connection)).toEqual({
      from: 'source',
      to: 'sink',
      fromIndex: 0,
      toIndex: 1,
      points: [
        [1, 2],
        [3, 4, 1],
      ],
    });
  });

  test('serializeDiagram preserves explicit realTime mode only when set', () => {
    state.blocks = new Map();
    state.connections = [];
    state.diagramName = 't';
    state.realTime = false;
    let out = serializeDiagram(state as any);
    expect(out.realTime).toBe(false);

    state.realTime = undefined;
    out = serializeDiagram(state as any);
    expect(out).not.toHaveProperty('realTime');
  });
});
//=============================================================================
export {};
//=============================================================================
