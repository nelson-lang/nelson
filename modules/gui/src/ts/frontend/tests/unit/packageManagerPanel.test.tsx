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
import { PackageManagerPanel } from '../../src/panels/PackageManagerPanel';
import {
  emitToolEvent,
  showTool,
  subscribeToolEvents,
  takeToolArgs,
  TOOL_PANELS,
} from '../../src/desktop/webTools';
//=============================================================================
type Call = (name: string, payload: string) => Promise<string>;
const g = window as unknown as { webui?: { call: Call } };
const mounted: Array<{ root: Root; host: HTMLDivElement }> = [];
const sent: Array<{ method: string; params: Record<string, unknown> }> = [];
function toolResult(method: string): unknown {
  if (method === 'i18n')
    return { ok: true, locale: 'en_US', en_US: '{"title":"Packages"}', strings: '' };
  if (method === 'snapshot') {
    return {
      ok: true,
      arch: 'test',
      registry: null,
      list: {
        ok: true,
        installed: [
          {
            name: 'sample_toolbox',
            version: '1.2.0',
            summary: 'A sample installed package.',
            installed: { version: '1.2.0', path: '/x/sample_toolbox' },
          },
        ],
      },
      search: { ok: true, packages: [] },
    };
  }
  if (method === 'updates') return { count: 0 };
  return {};
}
//=============================================================================
function installBackend(): void {
  g.webui = {
    call: async (_name, payload) => {
      const request = JSON.parse(payload) as {
        id: number;
        method: string;
        params: { tool?: string; method?: string; params?: Record<string, unknown> };
      };
      if (request.method === 'tool.dispatch') {
        sent.push({ method: String(request.params.method), params: request.params.params ?? {} });
        return JSON.stringify({
          id: request.id,
          result: toolResult(String(request.params.method)),
        });
      }
      return JSON.stringify({ id: request.id, result: {} });
    },
  };
}
//=============================================================================
async function flush(): Promise<void> {
  for (let i = 0; i < 6; i += 1) {
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });
  }
}
//=============================================================================
async function renderPanel(): Promise<HTMLDivElement> {
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  mounted.push({ root, host });
  await act(async () => root.render(<PackageManagerPanel />));
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
describe('web tools', () => {
  it('maps tools to panels and hands a request to a panel that is not open yet', () => {
    expect(TOOL_PANELS.nmm).toBe('packages');
    expect(TOOL_PANELS.examples).toBe('examples');
    showTool('nmm', { action: 'show', name: 'sample_toolbox' });
    expect(takeToolArgs('nmm')).toEqual({ action: 'show', name: 'sample_toolbox' });
    expect(takeToolArgs('nmm')).toBeNull();
  });

  it('delivers tool events to subscribers as text', () => {
    const received: Array<[string, string]> = [];
    const off = subscribeToolEvents('nmm', (channel, payload) => received.push([channel, payload]));
    expect(emitToolEvent('nmm', 'progress', { name: 'x', step: 'fetch' })).toBe(true);
    expect(emitToolEvent('nmm', 'failed', 'boom')).toBe(true);
    off();
    expect(emitToolEvent('nmm', 'changed', null)).toBe(false);
    expect(received).toEqual([
      ['progress', '{"name":"x","step":"fetch"}'],
      ['failed', 'boom'],
    ]);
  });
});
//=============================================================================
describe('PackageManagerPanel', () => {
  it('runs the package manager through tool.dispatch and lists the packages', async () => {
    installBackend();
    const host = await renderPanel();
    expect(sent.map((call) => call.method)).toContain('i18n');
    expect(sent.map((call) => call.method)).toContain('snapshot');
    expect(host.querySelector('.nmm-app')).toBeTruthy();
    expect(host.textContent).toContain('sample_toolbox');
  });
});
//=============================================================================
