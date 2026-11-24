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
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { createRoot, type Root } from 'react-dom/client';
import {
  BreakpointPopover,
  type BreakpointPopoverProps,
} from '../../src/components/BreakpointPopover';
//=============================================================================
let container: HTMLDivElement;
let root: Root;
beforeEach(() => {
  container = document.createElement('div');
  document.body.appendChild(container);
  root = createRoot(container);
});
afterEach(() => {
  act(() => root.unmount());
  container.remove();
});
//=============================================================================
function props(overrides: Partial<BreakpointPopoverProps> = {}): BreakpointPopoverProps {
  return {
    line: 4,
    x: 10,
    y: 20,
    hasBreakpoint: false,
    enabled: true,
    condition: '',
    hasAny: false,
    onToggle: vi.fn(),
    onSetCondition: vi.fn(),
    onSetEnabled: vi.fn(),
    onEnableAllInFile: vi.fn(),
    onDisableAllInFile: vi.fn(),
    onClearAllInFile: vi.fn(),
    onClearAll: vi.fn(),
    onClose: vi.fn(),
    ...overrides,
  };
}
function render(p: BreakpointPopoverProps) {
  act(() => root.render(<BreakpointPopover {...p} />));
}
function buttons(): string[] {
  return [...container.querySelectorAll('button.bp-popover__item')].map((b) => b.textContent ?? '');
}
function click(label: string) {
  const button = [...container.querySelectorAll('button')].find((b) => b.textContent === label);
  if (!button) throw new Error(`no button ${label}`);
  act(() => button.click());
}
//=============================================================================
describe('BreakpointPopover (gutter menu)', () => {
  it('offers Set Breakpoint and a conditional breakpoint on an empty line', () => {
    render(props());
    expect(buttons()).toEqual([
      'Set Breakpoint',
      'Set Conditional Breakpoint…',
      'Enable All Breakpoints in File',
      'Disable All Breakpoints in File',
      'Clear All Breakpoints in File',
      'Clear All Breakpoints',
    ]);
    // nothing to enable, disable or clear in the file yet
    const disabled = [...container.querySelectorAll('button.bp-popover__item:disabled')].map(
      (b) => b.textContent
    );
    expect(disabled).toEqual([
      'Enable All Breakpoints in File',
      'Disable All Breakpoints in File',
      'Clear All Breakpoints in File',
    ]);
  });

  it('offers Clear, Modify Condition and Disable on an existing breakpoint', () => {
    const p = props({ hasBreakpoint: true, hasAny: true, condition: 'k > 2' });
    render(p);
    expect(buttons().slice(0, 3)).toEqual([
      'Clear Breakpoint',
      'Set/Modify Condition…',
      'Disable Breakpoint',
    ]);
    click('Disable Breakpoint');
    expect(p.onSetEnabled).toHaveBeenCalledWith(false);
    expect(p.onClose).toHaveBeenCalled();
  });

  it('shows Enable Breakpoint for a disabled one and toggles through Clear', () => {
    const p = props({ hasBreakpoint: true, hasAny: true, enabled: false });
    render(p);
    expect(buttons()).toContain('Enable Breakpoint');
    click('Clear Breakpoint');
    expect(p.onToggle).toHaveBeenCalled();
  });

  it('edits the condition inline and reports the engine message', () => {
    const p = props({ hasBreakpoint: true, hasAny: true, condition: 'k > 2' });
    render(p);
    click('Set/Modify Condition…');
    const input = container.querySelector<HTMLInputElement>(
      '[data-testid="breakpoint-condition-input"]'
    );
    expect(input).not.toBeNull();
    expect(input!.value).toBe('k > 2');
    expect(container.textContent).toContain('Pause on line 4 only when this expression is true:');
    act(() => {
      // React tracks the value on the instance: write through the native setter
      const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')!.set!;
      setter.call(input, 'k >>> 2');
      input!.dispatchEvent(new Event('input', { bubbles: true }));
    });
    click('Apply');
    expect(p.onSetCondition).toHaveBeenCalledWith('k >>> 2');
    // the popover stays open with the error until a valid condition is applied
    render({ ...p, conditionError: "The condition 'k >>> 2' contains a syntax error." });
    expect(container.querySelector('.bp-popover__error')?.textContent).toBe(
      "The condition 'k >>> 2' contains a syntax error."
    );
    click('Cancel');
    expect(p.onClose).toHaveBeenCalled();
  });

  it('closes on Escape and on a click outside', () => {
    const p = props();
    render(p);
    act(() => {
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
    });
    expect(p.onClose).toHaveBeenCalledTimes(1);
    act(() => {
      document.body.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
    });
    expect(p.onClose).toHaveBeenCalledTimes(2);
  });

  it('runs the file-wide actions', () => {
    const p = props({ hasBreakpoint: true, hasAny: true });
    render(p);
    click('Enable All Breakpoints in File');
    expect(p.onEnableAllInFile).toHaveBeenCalled();
    render(p);
    click('Disable All Breakpoints in File');
    expect(p.onDisableAllInFile).toHaveBeenCalled();
    render(p);
    click('Clear All Breakpoints in File');
    expect(p.onClearAllInFile).toHaveBeenCalled();
    render(p);
    click('Clear All Breakpoints');
    expect(p.onClearAll).toHaveBeenCalled();
  });
});
//=============================================================================
