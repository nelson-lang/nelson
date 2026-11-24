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
import { useMenuDismiss } from '../../src/components/useMenuDismiss';
//=============================================================================
function Popover({ open, close }: { open: boolean; close: () => void }): JSX.Element {
  useMenuDismiss(open, close, '.pop');
  return (
    <div>
      <button className="outside">outside</button>
      <div className="pop">
        <button className="inside">inside</button>
      </div>
    </div>
  );
}
//=============================================================================
describe('useMenuDismiss', () => {
  let container: HTMLDivElement;
  let root: Root;
  const close = vi.fn();
  const press = (selector: string) =>
    act(() => {
      container.querySelector(selector)!.dispatchEvent(new Event('pointerdown', { bubbles: true }));
    });

  beforeEach(() => {
    close.mockClear();
    container = document.createElement('div');
    document.body.appendChild(container);
    root = createRoot(container);
  });
  afterEach(() => {
    act(() => root.unmount());
    container.remove();
  });

  it('closes on a pointer down outside, not inside', () => {
    act(() => root.render(<Popover open close={close} />));
    press('.inside');
    expect(close).not.toHaveBeenCalled();
    press('.outside');
    expect(close).toHaveBeenCalledTimes(1);
  });

  it('closes on Escape, ignores other keys', () => {
    act(() => root.render(<Popover open close={close} />));
    act(() => {
      document.dispatchEvent(new KeyboardEvent('keydown', { key: 'a' }));
    });
    expect(close).not.toHaveBeenCalled();
    act(() => {
      document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
    });
    expect(close).toHaveBeenCalledTimes(1);
  });

  it('listens only while open', () => {
    act(() => root.render(<Popover open={false} close={close} />));
    press('.outside');
    act(() => {
      document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
    });
    expect(close).not.toHaveBeenCalled();
  });
});
//=============================================================================
