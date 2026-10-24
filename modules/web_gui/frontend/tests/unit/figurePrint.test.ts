//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, it, vi } from 'vitest';
import { openFigurePrintPreview } from '../../src/panels/figurePrint';
//=============================================================================
afterEach(() => vi.restoreAllMocks());
function previewWindow() {
  return {
    document: document.implementation.createHTMLDocument(),
    closed: false,
    focus: vi.fn(),
    print: vi.fn(),
    close: vi.fn(),
    addEventListener: vi.fn(),
  };
}
//=============================================================================
it('opens during the click, waits for the image and keeps a manual print button', async () => {
  const preview = previewWindow();
  const open = vi.spyOn(window, 'open').mockReturnValue(preview as unknown as Window);
  const create = Object.getOwnPropertyDescriptor(URL, 'createObjectURL');
  const revoke = Object.getOwnPropertyDescriptor(URL, 'revokeObjectURL');
  Object.defineProperty(URL, 'createObjectURL', {
    configurable: true,
    value: vi.fn(() => 'blob:figure'),
  });
  Object.defineProperty(URL, 'revokeObjectURL', { configurable: true, value: vi.fn() });
  try {
    const render = vi.fn(async () => {
      expect(open).toHaveBeenCalledOnce();
      return new Blob(['<svg/>'], { type: 'image/svg+xml' });
    });
    const printing = openFigurePrintPreview(render, '<Figure 1>');
    expect(open).toHaveBeenCalledOnce();
    expect(preview.print).not.toHaveBeenCalled();
    await Promise.resolve();
    const image = preview.document.querySelector('img')!;
    expect(image.getAttribute('src')).toBe('blob:figure');
    image.dispatchEvent(new Event('load'));
    await printing;
    expect(preview.document.title).toBe('<Figure 1>');
    expect(preview.print).toHaveBeenCalledOnce();
    const button = preview.document.querySelector('button')!;
    expect(button.disabled).toBe(false);
    button.click();
    expect(preview.print).toHaveBeenCalledTimes(2);
  } finally {
    if (create) Object.defineProperty(URL, 'createObjectURL', create);
    else Reflect.deleteProperty(URL, 'createObjectURL');
    if (revoke) Object.defineProperty(URL, 'revokeObjectURL', revoke);
    else Reflect.deleteProperty(URL, 'revokeObjectURL');
  }
});
//=============================================================================
it('reports blocked popups without performing the export', async () => {
  vi.spyOn(window, 'open').mockReturnValue(null);
  const render = vi.fn();
  await expect(openFigurePrintPreview(render, 'Figure')).rejects.toThrow('Allow pop-up windows');
  expect(render).not.toHaveBeenCalled();
});
//=============================================================================
it('closes the loading preview and reports rendering errors', async () => {
  const preview = previewWindow();
  vi.spyOn(window, 'open').mockReturnValue(preview as unknown as Window);
  await expect(
    openFigurePrintPreview(() => Promise.reject(new Error('Render failed')), 'Figure')
  ).rejects.toThrow('Render failed');
  expect(preview.close).toHaveBeenCalledOnce();
});
//=============================================================================
it('uses the native print bridge after rendering, without a popup', async () => {
  const postMessage = vi.fn();
  const open = vi.spyOn(window, 'open').mockReturnValue(null);
  Object.defineProperty(window, 'webkit', {
    configurable: true,
    value: { messageHandlers: { nelsonFigurePrint: { postMessage } } },
  });
  try {
    const text = vi.fn(async () => '<svg>exported figure</svg>');
    await openFigurePrintPreview(async () => ({ text } as unknown as Blob), 'Native figure');
    expect(open).not.toHaveBeenCalled();
    expect(text).toHaveBeenCalledOnce();
    expect(postMessage).toHaveBeenCalledWith({
      svg: '<svg>exported figure</svg>',
      title: 'Native figure',
    });
    postMessage.mockClear();
    await expect(
      openFigurePrintPreview(() => Promise.reject(new Error('Export failed')), 'Figure')
    ).rejects.toThrow('Export failed');
    expect(postMessage).not.toHaveBeenCalled();
  } finally {
    Reflect.deleteProperty(window, 'webkit');
  }
});
//=============================================================================
