//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { DiagnosticsPanel, formatDiagnosticLine } from './DiagnosticsPanel.js';
import {
  _resetDiagnosticsForTests,
  addDiagnostic,
  clearDiagnostics,
} from '../app/diagnostics-service.js';
import { setUiPref } from '../app/ui-prefs.js';
//=============================================================================
let host: HTMLDivElement;
let root: Root;
let writeText: ReturnType<typeof vi.fn>;
beforeEach(() => {
  _resetDiagnosticsForTests();
  localStorage.clear();
  // ui-prefs keeps an in-memory cache that outlives localStorage.clear(), so
  // reset the severity filters explicitly or a prior test's toggle leaks here.
  setUiPref('nf-diagnostics-filters', JSON.stringify({ error: true, warning: true, info: true }));
  setUiPref('nf-diagnostics-source', 'all');
  host = document.createElement('div');
  document.body.appendChild(host);
  root = createRoot(host);
  writeText = vi.fn().mockResolvedValue(undefined);
  Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText } });
});
//=============================================================================
afterEach(async () => {
  await act(async () => {
    root.unmount();
  });
  host.remove();
  vi.restoreAllMocks();
});
//=============================================================================
function selectText(text: string): void {
  vi.spyOn(window, 'getSelection').mockReturnValue({
    toString: () => text,
  } as unknown as Selection);
}
//=============================================================================
function pressCopy(): void {
  act(() => {
    host
      .querySelector('.nf-diagnostics-panel')!
      .dispatchEvent(
        new KeyboardEvent('keydown', { key: 'c', ctrlKey: true, bubbles: true, cancelable: true })
      );
  });
}
//=============================================================================
function rightClick(el: Element): void {
  act(() => {
    el.dispatchEvent(
      new MouseEvent('contextmenu', { bubbles: true, cancelable: true, clientX: 20, clientY: 30 })
    );
  });
}
//=============================================================================
function menuItems(): HTMLButtonElement[] {
  return Array.from(host.querySelectorAll<HTMLButtonElement>('[role="menuitem"]'));
}
//=============================================================================
async function renderWithTwoDiagnostics(): Promise<void> {
  await act(async () => {
    root.render(<DiagnosticsPanel />);
  });
  await act(async () => {
    addDiagnostic({ severity: 'error', message: 'Invalid gain.', source: 'model', blockId: 'g1' });
    addDiagnostic({ severity: 'warning', message: 'Step too large.', source: 'simulation' });
  });
}
//=============================================================================
test('formats a diagnostic as one plain-text line', () => {
  expect(
    formatDiagnosticLine({
      id: 'd1',
      severity: 'error',
      message: 'Invalid gain.',
      source: 'model',
      createdAt: 0,
    })
  ).toBe('error: Invalid gain. (model)');
});
//=============================================================================
test('a row message carries its full text as a title tooltip', async () => {
  await renderWithTwoDiagnostics();
  const messages = Array.from(host.querySelectorAll('.nf-diagnostic-message'));
  expect(messages.map((m) => m.getAttribute('title'))).toEqual([
    'Invalid gain.',
    'Step too large.',
  ]);
});
//=============================================================================
test('right-click on a row offers Copy and Copy all', async () => {
  await renderWithTwoDiagnostics();
  expect(host.querySelector('[role="menu"]')).toBeNull();

  rightClick(host.querySelectorAll('.nf-diagnostic-row')[1]!);

  expect(menuItems().map((b) => b.textContent)).toEqual(['Copy', 'Copy all']);
});
//=============================================================================
test('Copy puts the clicked row on the clipboard and closes the menu', async () => {
  await renderWithTwoDiagnostics();
  rightClick(host.querySelectorAll('.nf-diagnostic-row')[1]!);

  await act(async () => {
    menuItems()[0]!.click();
  });

  expect(writeText).toHaveBeenCalledWith('warning: Step too large. (simulation)');
  expect(host.querySelector('[role="menu"]')).toBeNull();
});
//=============================================================================
test('Copy all puts every visible row on the clipboard, one per line', async () => {
  await renderWithTwoDiagnostics();
  rightClick(host.querySelector('.nf-diagnostic-row')!);

  await act(async () => {
    menuItems()[1]!.click();
  });

  expect(writeText).toHaveBeenCalledWith(
    'error: Invalid gain. (model)\nwarning: Step too large. (simulation)'
  );
});
//=============================================================================
test('Copy all honours the severity filters', async () => {
  await renderWithTwoDiagnostics();
  await act(async () => {
    (host.querySelector('.nf-diagnostics-filter.nf-diagnostic-error') as HTMLButtonElement).click();
  });
  expect(host.querySelectorAll('.nf-diagnostic-row')).toHaveLength(1);
  rightClick(host.querySelector('.nf-diagnostic-row')!);

  await act(async () => {
    menuItems()[1]!.click();
  });

  expect(writeText).toHaveBeenCalledWith('warning: Step too large. (simulation)');
});
//=============================================================================
test('Escape and an outside mousedown close the menu without copying', async () => {
  await renderWithTwoDiagnostics();
  rightClick(host.querySelector('.nf-diagnostic-row')!);
  expect(host.querySelector('[role="menu"]')).not.toBeNull();

  act(() => {
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
  });
  expect(host.querySelector('[role="menu"]')).toBeNull();

  rightClick(host.querySelector('.nf-diagnostic-row')!);
  act(() => {
    document.body.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
  });
  expect(host.querySelector('[role="menu"]')).toBeNull();
  expect(writeText).not.toHaveBeenCalled();
});
//=============================================================================
test('the toolbar Copy all button copies every visible row', async () => {
  await renderWithTwoDiagnostics();
  const copyAll = host.querySelector('.nf-diagnostics-copy') as HTMLButtonElement;
  expect(copyAll).not.toBeNull();
  expect(copyAll.disabled).toBe(false);

  await act(async () => {
    copyAll.click();
  });

  expect(writeText).toHaveBeenCalledWith(
    'error: Invalid gain. (model)\nwarning: Step too large. (simulation)'
  );
});
//=============================================================================
test('the toolbar Copy all button is disabled when nothing is visible', async () => {
  await act(async () => {
    root.render(<DiagnosticsPanel />);
  });
  const copyAll = host.querySelector('.nf-diagnostics-copy') as HTMLButtonElement;
  expect(copyAll.disabled).toBe(true);
});
//=============================================================================
test('Ctrl+C copies every visible row when no text is selected', async () => {
  await renderWithTwoDiagnostics();
  selectText('');
  pressCopy();
  expect(writeText).toHaveBeenCalledWith(
    'error: Invalid gain. (model)\nwarning: Step too large. (simulation)'
  );
});
//=============================================================================
test('Ctrl+C defers to the browser when message text is selected', async () => {
  await renderWithTwoDiagnostics();
  selectText('Invalid gain.');
  pressCopy();
  expect(writeText).not.toHaveBeenCalled();
});
//=============================================================================
test('clicking a row does not navigate while text is selected', async () => {
  await renderWithTwoDiagnostics();
  const navigated: string[] = [];
  const onNav = (e: Event) => navigated.push((e as CustomEvent).detail?.blockId);
  document.addEventListener('nf-select-block', onNav);

  selectText('Invalid gain.');
  act(() => {
    (host.querySelector('.nf-diagnostic-main') as HTMLButtonElement).click();
  });
  expect(navigated).toEqual([]);

  selectText('');
  act(() => {
    (host.querySelector('.nf-diagnostic-main') as HTMLButtonElement).click();
  });
  expect(navigated).toEqual(['g1']);
  document.removeEventListener('nf-select-block', onNav);
});
//=============================================================================
test('the source filter appears with more than one source and narrows the list', async () => {
  await renderWithTwoDiagnostics();
  const select = host.querySelector('.nf-diagnostics-source-filter') as HTMLSelectElement;
  expect(select).not.toBeNull();
  expect(Array.from(select.options).map((o) => o.value)).toEqual(['all', 'model', 'simulation']);

  await act(async () => {
    select.value = 'simulation';
    select.dispatchEvent(new Event('change', { bubbles: true }));
  });

  const rows = host.querySelectorAll('.nf-diagnostic-message');
  expect(Array.from(rows).map((r) => r.textContent)).toEqual(['Step too large.']);
});
//=============================================================================
test('the source filter is hidden when every diagnostic shares one source', async () => {
  await act(async () => {
    root.render(<DiagnosticsPanel />);
  });
  await act(async () => {
    addDiagnostic({ severity: 'error', message: 'One.', source: 'model' });
    addDiagnostic({ severity: 'warning', message: 'Two.', source: 'model' });
  });
  expect(host.querySelector('.nf-diagnostics-source-filter')).toBeNull();
});
//=============================================================================
test('a source filter left dangling by a clear falls back to all', async () => {
  await renderWithTwoDiagnostics();
  const select = host.querySelector('.nf-diagnostics-source-filter') as HTMLSelectElement;
  await act(async () => {
    select.value = 'simulation';
    select.dispatchEvent(new Event('change', { bubbles: true }));
  });
  // The simulation diagnostic goes away; the remaining model row must still show
  // even though the source filter still points at "simulation".
  await act(async () => {
    clearDiagnostics();
    addDiagnostic({ severity: 'error', message: 'Invalid gain.', source: 'model' });
  });
  const rows = host.querySelectorAll('.nf-diagnostic-message');
  expect(Array.from(rows).map((r) => r.textContent)).toEqual(['Invalid gain.']);
});
//=============================================================================
