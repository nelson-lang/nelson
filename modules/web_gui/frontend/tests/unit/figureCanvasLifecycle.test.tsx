//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act, StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { expect, it, vi } from 'vitest';
import { FigureCanvas } from '../../src/panels/FigureCanvas';
//=============================================================================
it('retains offscreen ownership during effect replay and releases the Worker on unmount', () => {
  vi.useFakeTimers();
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  const terminate = vi.fn();
  class CanvasWorker {
    addEventListener = vi.fn();
    postMessage = vi.fn();
    terminate = terminate;
  }
  vi.stubGlobal('Worker', CanvasWorker);
  const transferred = new WeakSet<HTMLCanvasElement>();
  const transfer = vi.fn(function (this: HTMLCanvasElement) {
    if (transferred.has(this)) throw new DOMException('Already transferred', 'InvalidStateError');
    transferred.add(this);
    return {};
  });
  const originalTransfer = Object.getOwnPropertyDescriptor(
    HTMLCanvasElement.prototype,
    'transferControlToOffscreen'
  );
  Object.defineProperty(HTMLCanvasElement.prototype, 'transferControlToOffscreen', {
    configurable: true,
    value: transfer,
  });
  const width = Object.getOwnPropertyDescriptor(HTMLCanvasElement.prototype, 'width')!;
  vi.spyOn(HTMLCanvasElement.prototype, 'width', 'set').mockImplementation(function (
    this: HTMLCanvasElement,
    value: number
  ) {
    if (transferred.has(this)) throw new DOMException('Canvas transferred', 'InvalidStateError');
    width.set!.call(this, value);
  });
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  try {
    act(() =>
      root.render(
        <StrictMode>
          <FigureCanvas handle={0} primitives={[]} width={320} height={240} />
        </StrictMode>
      )
    );
    expect(transfer).toHaveBeenCalledTimes(1);
    expect(terminate).not.toHaveBeenCalled();
    act(() => root.unmount());
    vi.runAllTimers();
    expect(terminate).toHaveBeenCalledTimes(1);
  } finally {
    host.remove();
    vi.restoreAllMocks();
    if (originalTransfer)
      Object.defineProperty(
        HTMLCanvasElement.prototype,
        'transferControlToOffscreen',
        originalTransfer
      );
    else Reflect.deleteProperty(HTMLCanvasElement.prototype, 'transferControlToOffscreen');
    vi.unstubAllGlobals();
    vi.useRealTimers();
  }
});
//=============================================================================
