//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { t } from '../i18n';
//=============================================================================
// The document shown in a fresh Notebook, built at call time so its text is translated.
// Commands are kept simple enough to run both on the real engine and on the standalone mock engine.
export function initialContent() {
  return [
    { type: 'heading', props: { level: 1 }, content: t('&Notebook') },
    {
      type: 'paragraph',
      content: t(
        'A Notebook mixes formatted text, Nelson code and its output in one document. Press a cell’s ▶ button or Run all; the workspace is shared between cells.'
      ),
    },

    { type: 'heading', props: { level: 2 }, content: t('1. A matrix') },
    { type: 'codecell', props: { code: 'A = magic(4)' } },

    { type: 'heading', props: { level: 2 }, content: t('2. A curve') },
    {
      type: 'paragraph',
      content: t('Drag the sample count below and re-run — the figure appears inline.'),
    },
    {
      type: 'numberfield',
      props: { label: t('Samples'), varName: 'N', value: 50, min: 10, max: 200, step: 10 },
    },
    { type: 'codecell', props: { code: 'x = linspace(0, 6.28, N);\nplot(x)' } },

    { type: 'heading', props: { level: 2 }, content: t('3. A surface') },
    { type: 'codecell', props: { code: 'surf(magic(12))' } },

    {
      type: 'paragraph',
      content: t(
        'Use “Hide code” to turn this into a shareable view. Type “/” to insert a cell or control. Export to a Jupyter notebook from the toolbar.'
      ),
    },
  ] as const;
}
//=============================================================================
