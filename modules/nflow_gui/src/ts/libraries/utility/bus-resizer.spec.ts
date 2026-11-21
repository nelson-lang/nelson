//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { busCreatorResizer, busSelectorResizer } from './blocks';
import { desiredPortCounts } from '../../blocks/port-rebuild';

test('bus creator exposes the configured number of input ports', () => {
  const block: any = { type: 'busCreator', params: { Inputs: 5, MemberNames: ['a', 'b'] } };
  busCreatorResizer(block);
  expect(block.dynamicInputs).toHaveLength(5);
  expect(block.dynamicInputs.map((p: any) => p.y)).toEqual([10, 30, 50, 70, 90]);
  expect(desiredPortCounts(block)).toEqual({ inputs: 5, outputs: 1 });
});

test('bus selector exposes selected members or one bus output', () => {
  const block: any = { type: 'busSelector', params: { SelectedSignals: ['a', 'b', 'c'] } };
  busSelectorResizer(block);
  expect(block.dynamicOutputs).toHaveLength(3);
  expect(desiredPortCounts(block)).toEqual({ inputs: 1, outputs: 3 });
  block.params.OutputAsBus = true;
  busSelectorResizer(block);
  expect(block.dynamicOutputs).toHaveLength(1);
  expect(desiredPortCounts(block)).toEqual({ inputs: 1, outputs: 1 });
});

test('bus blocks retain custom dimensions and place ports on the edges', () => {
  const creator: any = { params: { Inputs: 3, width: 150, height: 120 } };
  busCreatorResizer(creator);
  expect([creator.width, creator.height]).toEqual([150, 120]);
  expect(creator.dynamicInputs.map((p: any) => [p.x, p.y])).toEqual([
    [0, 20],
    [0, 60],
    [0, 100],
  ]);
  expect(creator.dynamicOutputs[0]).toMatchObject({ x: 150, y: 60 });
  const selector: any = { params: { SelectedSignals: ['a', 'b'], width: 160, height: 100 } };
  busSelectorResizer(selector);
  expect(selector.dynamicInputs[0]).toMatchObject({ x: 0, y: 50 });
  expect(selector.dynamicOutputs.map((p: any) => [p.x, p.y])).toEqual([
    [160, 25],
    [160, 75],
  ]);
});
