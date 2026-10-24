//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { afterEach, describe, expect, it } from 'vitest';
import { createRoot, type Root } from 'react-dom/client';
import { ExamplesPanel, exampleCommand } from '../../src/panels/ExamplesPanel';
import { emitLocalPush } from '../../src/rpc/client';
//=============================================================================
type Call = (name: string, payload: string) => Promise<string>;
const g = window as unknown as { webui?: { call: Call } };
const catalog = {
  locale: 'en_US',
  diagnostics: [],
  groups: [
    {
      name: 'graphics',
      title: 'graphics',
      source: 'nelson',
      examples: [
        {
          title: 'Sine wave',
          description: 'Plot it',
          kind: 'script',
          file: "C:/examples/it's.m",
          tags: ['plot'],
          requirements: ['gui'],
        },
        {
          title: 'Trainer',
          description: 'Interactive',
          kind: 'command',
          command: 'morse.practice()',
          tags: [],
          requirements: [],
        },
      ],
    },
    {
      name: 'nflow:causal',
      title: 'Causal',
      source: 'nflow',
      examples: [
        {
          title: 'PID',
          description: 'Loop',
          kind: 'model',
          file: 'C:/examples/PID_Demo.nflow',
          tags: ['causal'],
          requirements: [],
          blockCount: 6,
        },
      ],
    },
  ],
};
//=============================================================================
const refreshedCatalog = {
  ...catalog,
  groups: [
    ...catalog.groups,
    {
      name: 'external:morse',
      title: 'morse',
      source: 'external',
      examples: [
        {
          title: 'A tour of the morse toolbox',
          description: 'Encode and decode Morse.',
          kind: 'script',
          file: '/workspace/modules/morse/1.0.0/examples/morse_demo.m',
          tags: ['morse'],
          requirements: [],
        },
      ],
    },
  ],
};
//=============================================================================
const mounted: Array<{ root: Root; host: HTMLDivElement }> = [];
const sent: Array<{ method: string; params: Record<string, unknown> }> = [];
function installBackend(): void {
  g.webui = {
    call: async (_name, payload) => {
      const request = JSON.parse(payload) as {
        id: number;
        method: string;
        params: Record<string, unknown>;
      };
      sent.push({ method: request.method, params: request.params });
      if (request.method === 'examples.list') {
        return JSON.stringify({ id: request.id, result: catalog });
      }
      if (request.method === 'examples.refresh') {
        return JSON.stringify({ id: request.id, result: refreshedCatalog });
      }
      return JSON.stringify({ id: request.id, result: {} });
    },
  };
}
//=============================================================================
async function flush(): Promise<void> {
  await act(async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
}
//=============================================================================
async function renderPanel(): Promise<HTMLDivElement> {
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  mounted.push({ root, host });
  await act(async () => root.render(<ExamplesPanel />));
  await flush();
  return host;
}
//=============================================================================
afterEach(() => {
  mounted.splice(0).forEach(({ root, host }) => {
    act(() => root.unmount());
    host.remove();
  });
  sent.splice(0);
  delete g.webui;
});
//=============================================================================
describe('exampleCommand', () => {
  it('builds the console command for each kind of entry', () => {
    expect(
      exampleCommand({
        title: 't',
        description: '',
        kind: 'script',
        file: "C:/x/it's.m",
        tags: [],
        requirements: [],
      })
    ).toBe("run('C:/x/it''s.m')");
    expect(
      exampleCommand({
        title: 't',
        description: '',
        kind: 'command',
        command: 'morse.practice()',
        tags: [],
        requirements: [],
      })
    ).toBe('morse.practice()');
    expect(
      exampleCommand({
        title: 't',
        description: '',
        kind: 'model',
        file: 'C:/x/PID.nflow',
        tags: [],
        requirements: [],
      })
    ).toBe("nflow('C:/x/PID.nflow')");
  });
});
//=============================================================================
describe('ExamplesPanel', () => {
  it('lists the catalog served by examples.list, Nelson modules first', async () => {
    installBackend();
    const host = await renderPanel();
    expect(sent.some((call) => call.method === 'examples.list')).toBe(true);
    expect(host.querySelectorAll('.example-gallery-card').length).toBe(3);
    const headings = Array.from(host.querySelectorAll('.example-gallery-rail-heading')).map(
      (node) => node.textContent
    );
    expect(headings).toEqual(['Nelson', 'NFlow']);
    expect(host.querySelector('.example-gallery-badge.requirement')?.textContent).toBe('gui');
  });

  it('runs a script through the examples backend and keeps the panel open', async () => {
    installBackend();
    const host = await renderPanel();
    const run = Array.from(
      host.querySelectorAll<HTMLButtonElement>('.example-gallery-card[data-kind="script"] button')
    ).find((button) => button.textContent === 'Run');
    expect(run).toBeTruthy();
    await act(async () => run!.click());
    await flush();
    expect(sent).toContainEqual({
      method: 'examples.run',
      params: { path: "C:/examples/it's.m" },
    });
    expect(sent.some((call) => call.method === 'term.input')).toBe(false);
    expect(host.querySelector('.example-gallery')).toBeTruthy();
    expect(run!.classList.contains('running')).toBe(true);
  });

  it('opens a script through the examples backend', async () => {
    installBackend();
    const host = await renderPanel();
    const open = Array.from(
      host.querySelectorAll<HTMLButtonElement>('.example-gallery-card[data-kind="script"] button')
    ).find((button) => button.textContent === 'Open in editor');
    expect(open).toBeTruthy();
    await act(async () => open!.click());
    await flush();
    expect(sent).toContainEqual({
      method: 'examples.open',
      params: { path: "C:/examples/it's.m" },
    });
    expect(sent.some((call) => call.method === 'term.input')).toBe(false);
  });

  it('reloads the catalog when installed packages change', async () => {
    installBackend();
    const host = await renderPanel();
    expect(host.textContent).not.toContain('A tour of the morse toolbox');

    await act(async () => {
      emitLocalPush('examples.changed', { reason: 'packages' });
      await new Promise((resolve) => setTimeout(resolve, 0));
    });
    await flush();

    expect(sent.some((call) => call.method === 'examples.refresh')).toBe(true);
    expect(host.textContent).toContain('A tour of the morse toolbox');
    const headings = Array.from(host.querySelectorAll('.example-gallery-rail-heading')).map(
      (node) => node.textContent
    );
    expect(headings).toEqual(['Nelson', 'NFlow', 'External libraries']);
  });
});
//=============================================================================
