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
import { createRoot, type Root } from 'react-dom/client';
import { vi } from 'vitest';
import ConsolePanel from './ConsolePanel.js';
import { pushStatus, _resetForTests } from '../app/status-service.js';
//=============================================================================
(
  globalThis as typeof globalThis & { IS_REACT_ACT_ENVIRONMENT?: boolean }
).IS_REACT_ACT_ENVIRONMENT = true;
//=============================================================================
let host: HTMLDivElement;
let root: Root;
beforeEach(() => {
  _resetForTests();
  host = document.createElement('div');
  document.body.appendChild(host);
  root = createRoot(host);
});
//=============================================================================
afterEach(() => {
  act(() => root.unmount());
  host.remove();
  _resetForTests();
});
//=============================================================================
test('renders the empty state before any status is pushed', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  const panel = host.querySelector('[data-testid="console-panel"]');
  expect(panel).not.toBeNull();
  expect(panel?.getAttribute('tabindex')).toBe('0');
  expect(panel?.getAttribute('aria-label')).toBe('Console');
  expect(host.querySelector('.nf-console-empty')).not.toBeNull();
});
//=============================================================================
test('select-all is scoped to console contents', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  act(() => {
    pushStatus({ level: 'info', message: 'console-only selection' });
  });
  const outside = document.createElement('div');
  outside.textContent = 'outside console';
  document.body.appendChild(outside);
  const panel = host.querySelector('[data-testid="console-panel"]') as HTMLElement;

  act(() => {
    panel.dispatchEvent(
      new KeyboardEvent('keydown', {
        key: 'a',
        ctrlKey: true,
        bubbles: true,
        cancelable: true,
      })
    );
  });

  expect(window.getSelection()?.toString()).toContain('console-only selection');
  expect(window.getSelection()?.toString()).not.toContain('outside console');
  outside.remove();
  window.getSelection()?.removeAllRanges();
});
//=============================================================================
test('appends status-service messages with their level', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  act(() => {
    pushStatus({ level: 'info', message: 'hello world' });
  });
  act(() => {
    pushStatus({ level: 'error', message: 'boom' });
  });
  const entries = host.querySelectorAll('.nf-console-entry');
  expect(entries.length).toBe(2);
  expect(entries[0].textContent).toContain('hello world');
  expect(entries[1].textContent).toContain('boom');
  expect(entries[1].classList.contains('nf-console-error')).toBe(true);
});
//=============================================================================
test('coalesces consecutive identical messages instead of flooding', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  for (let i = 0; i < 5; i++) {
    act(() => {
      pushStatus({ level: 'info', message: 'same message' });
    });
  }
  act(() => {
    pushStatus({ level: 'info', message: 'different message' });
  });
  const entries = host.querySelectorAll('.nf-console-entry');
  expect(entries.length).toBe(2);
  expect(entries[0].textContent).toContain('same message');
  expect(entries[1].textContent).toContain('different message');
});
//=============================================================================
test('entries with a blockId are clickable and dispatch nf-select-block', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  act(() => {
    pushStatus({ level: 'error', message: 'Error: unsupported_block: userFunc', blockId: 'b42' });
  });
  const entry = host.querySelector('.nf-console-entry') as HTMLElement;
  expect(entry.classList.contains('nf-console-link')).toBe(true);
  let detail: unknown = null;
  const onSelect = (ev: Event) => {
    detail = (ev as CustomEvent<unknown>).detail;
  };
  document.addEventListener('nf-select-block', onSelect);
  act(() => {
    entry.click();
  });
  document.removeEventListener('nf-select-block', onSelect);
  expect(detail).toEqual({ blockId: 'b42' });
});
//=============================================================================
test('entries without blockId are not clickable', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  act(() => {
    pushStatus({ level: 'info', message: 'plain message' });
  });
  const entry = host.querySelector('.nf-console-entry') as HTMLElement;
  expect(entry.classList.contains('nf-console-link')).toBe(false);
});
//=============================================================================
test('renders progress percentage for progress items', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  act(() => {
    pushStatus({ level: 'progress', message: 'simulating', progress: 42 });
  });
  const progress = host.querySelector('.nf-console-pct');
  expect(progress).not.toBeNull();
  expect(progress?.textContent).toBe('42%');
});
//=============================================================================
test('copies selected console text from its context menu', async () => {
  const writeText = vi.fn().mockResolvedValue(undefined);
  Object.defineProperty(navigator, 'clipboard', {
    configurable: true,
    value: { writeText },
  });
  act(() => {
    root.render(<ConsolePanel />);
  });
  act(() => {
    pushStatus({ level: 'info', message: 'selected console text' });
  });

  const message = host.querySelector('.nf-console-message') as HTMLElement;
  const range = document.createRange();
  range.selectNodeContents(message);
  window.getSelection()?.removeAllRanges();
  window.getSelection()?.addRange(range);
  act(() => {
    message.dispatchEvent(
      new MouseEvent('contextmenu', {
        bubbles: true,
        cancelable: true,
        clientX: 20,
        clientY: 30,
      })
    );
  });

  const copyItem = host.querySelector('[role="menuitem"]') as HTMLButtonElement;
  expect(copyItem).not.toBeNull();
  expect(copyItem.textContent).toBe('Copy');
  await act(async () => {
    copyItem.click();
  });
  expect(writeText).toHaveBeenCalledWith('selected console text');
  expect(host.querySelector('[role="menu"]')).toBeNull();
});
//=============================================================================
test('does not replace the native context menu without a console selection', () => {
  act(() => {
    root.render(<ConsolePanel />);
  });
  act(() => {
    pushStatus({ level: 'info', message: 'plain console text' });
  });
  window.getSelection()?.removeAllRanges();

  const message = host.querySelector('.nf-console-message') as HTMLElement;
  const event = new MouseEvent('contextmenu', { bubbles: true, cancelable: true });
  act(() => {
    message.dispatchEvent(event);
  });

  expect(event.defaultPrevented).toBe(false);
  expect(host.querySelector('[role="menu"]')).toBeNull();
});
//=============================================================================
