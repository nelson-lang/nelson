//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The document shown in a fresh Notebook. Commands are kept simple enough to
// run both on the real engine and on the standalone mock engine.
export const initialContent = [
  { type: 'heading', props: { level: 1 }, content: 'Notebook' },
  {
    type: 'paragraph',
    content:
      'A Notebook mixes formatted text, Nelson code and its output in one document. ' +
      'Press a cell’s ▶ button or Run all; the workspace is shared between cells.',
  },

  { type: 'heading', props: { level: 2 }, content: '1. A matrix' },
  { type: 'codecell', props: { code: 'A = magic(4)' } },

  { type: 'heading', props: { level: 2 }, content: '2. A curve' },
  {
    type: 'paragraph',
    content: 'Drag the sample count below and re-run — the figure appears inline.',
  },
  {
    type: 'numberfield',
    props: { label: 'Samples', varName: 'N', value: 50, min: 10, max: 200, step: 10 },
  },
  { type: 'codecell', props: { code: 'x = linspace(0, 6.28, N);\nplot(x)' } },

  { type: 'heading', props: { level: 2 }, content: '3. A surface' },
  { type: 'codecell', props: { code: 'surf(magic(12))' } },

  {
    type: 'paragraph',
    content:
      'Use “Hide code” to turn this into a shareable view. Type “/” to insert a cell or control. Export to a Jupyter notebook from the toolbar.',
  },
] as const;
//=============================================================================
