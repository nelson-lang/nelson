//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { mountModals } from './components/modals.js';
import { EXAMPLE_BROWSER_SHOW_EVENT, OPEN_MODEL_PATH_EVENT } from './components/modal-events.js';
import { listExamples } from './example-service.js';
//=============================================================================
const catalog = {
  locale: 'en_US',
  diagnostics: [],
  groups: [
    {
      name: 'nflow:causal',
      title: 'Causal',
      source: 'nflow',
      examples: [
        {
          title: 'PID',
          description: 'A loop',
          kind: 'model',
          file: '/examples/PID_Demo.nflow',
          tags: ['causal'],
          requirements: [],
          blockCount: 6,
        },
      ],
    },
    {
      name: 'nflow:acausal',
      title: 'Acausal',
      source: 'nflow',
      examples: [
        {
          title: 'Brake',
          description: '',
          kind: 'script',
          file: '/examples/brake.m',
          tags: ['acausal'],
          requirements: [],
        },
      ],
    },
    {
      name: 'graphics',
      title: 'graphics',
      source: 'nelson',
      examples: [
        {
          title: 'Sine',
          description: 'Nelson script',
          kind: 'script',
          file: '/examples/sine.m',
          tags: [],
          requirements: [],
        },
      ],
    },
  ],
};
//=============================================================================
async function flush(): Promise<void> {
  await act(async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
}
//=============================================================================
async function openBrowser(): Promise<HTMLElement> {
  document.body.innerHTML = '';
  window.nflow_rpc = async (payload: string) => {
    const request = JSON.parse(payload) as { method: string };
    if (request.method === 'nflow.examples.list') return JSON.stringify({ result: catalog });
    return JSON.stringify({ result: { success: false } });
  };
  await listExamples({ refresh: true });
  await act(async () => mountModals());
  await act(async () => {
    document.dispatchEvent(new CustomEvent(EXAMPLE_BROWSER_SHOW_EVENT, { cancelable: true }));
  });
  await flush();
  const dialog = document.querySelector('.nf-ex-dialog') as HTMLElement;
  expect(dialog).toBeTruthy();
  return dialog;
}
//=============================================================================
afterEach(() => {
  delete window.nflow_rpc;
});
//=============================================================================
test('hosts the shared gallery with NFlow categories first', async () => {
  const dialog = await openBrowser();
  expect(dialog.querySelector('.example-gallery')).toBeTruthy();
  const headings = Array.from(dialog.querySelectorAll('.example-gallery-rail-heading')).map(
    (node) => node.textContent
  );
  expect(headings).toEqual(['NFlow', 'Nelson']);
  const cards = dialog.querySelectorAll('.example-gallery-card');
  expect(cards.length).toBe(3);
  expect(dialog.querySelector('.example-gallery-count')?.textContent).toContain('3');
});
//=============================================================================
test('selecting a category filters the cards and resets the scroll position', async () => {
  const dialog = await openBrowser();
  const content = dialog.querySelector('.example-gallery-main') as HTMLElement;
  content.scrollTop = 240;
  const category = Array.from(
    dialog.querySelectorAll<HTMLButtonElement>('.example-gallery-category')
  ).find((button) => button.textContent?.includes('Acausal'));
  expect(category).toBeTruthy();
  await act(async () => category!.click());
  expect(content.scrollTop).toBe(0);
  expect(dialog.querySelectorAll('.example-gallery-card').length).toBe(1);
  expect(dialog.querySelector('.example-gallery-group h3')?.textContent).toContain('Acausal');
});
//=============================================================================
test('opening a model dispatches the open event and closes the sheet', async () => {
  const dialog = await openBrowser();
  const opened: string[] = [];
  const onOpen = (event: Event) => {
    opened.push(String((event as CustomEvent<{ path: string }>).detail.path));
  };
  document.addEventListener(OPEN_MODEL_PATH_EVENT, onOpen);
  const open = Array.from(
    dialog.querySelectorAll<HTMLButtonElement>(
      '.example-gallery-card[data-kind="model"] .example-gallery-primary'
    )
  )[0];
  expect(open).toBeTruthy();
  await act(async () => open.click());
  await flush();
  document.removeEventListener(OPEN_MODEL_PATH_EVENT, onOpen);
  expect(opened).toEqual(['/examples/PID_Demo.nflow']);
  expect(document.querySelector('.nf-ex-dialog')).toBeNull();
});
//=============================================================================
test('escape closes the sheet', async () => {
  await openBrowser();
  await act(async () => {
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
  });
  expect(document.querySelector('.nf-ex-dialog')).toBeNull();
});
//=============================================================================
