//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createInspector } from './inspector';
import { state } from '../app/state';
import { Simulate } from 'react-dom/test-utils';

test('the wire inspector edits the selected signal name', () => {
  const body = document.createElement('div');
  document.body.appendChild(body);
  const old = state.selectedConnection;
  const connection = { from: 'a', to: 'b', label: 'old' };
  try {
    (state as any).selectedConnection = connection;
    const inspector = createInspector({ inspectorBody: body });
    inspector.renderInspector({
      kind: 'connection',
      fromId: 'a',
      toId: 'b',
      fromType: 'constant',
      toType: 'scope',
      params: {},
    } as any);
    const input = body.querySelector<HTMLInputElement>("input[data-edit='signalName']")!;
    expect(input.value).toBe('old');
    input.value = 'new speed';
    Simulate.change(input);
    expect(connection.label).toBe('new speed');
    const second = { from: 'c', to: 'd', label: 'temperature' };
    (state as any).selectedConnection = second;
    inspector.renderInspector({ kind: 'connection', fromId: 'c', toId: 'd', params: {} } as any);
    expect(body.querySelector<HTMLInputElement>("input[data-edit='signalName']")?.value).toBe(
      'temperature'
    );
  } finally {
    state.selectedConnection = old;
    body.remove();
  }
});
