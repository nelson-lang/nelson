//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export async function openFigurePrintPreview(render: () => Promise<Blob>, title: string) {
  const native = (
    window as Window & {
      webkit?: {
        messageHandlers?: { nelsonFigurePrint?: { postMessage: (value: unknown) => void } };
      };
    }
  ).webkit?.messageHandlers?.nelsonFigurePrint;
  if (native) {
    const blob = await render();
    native.postMessage({ svg: await blob.text(), title });
    return;
  }
  // Opening after the asynchronous export loses the click's popup permission.
  const preview = window.open('', '_blank');
  if (!preview) throw new Error('Allow pop-up windows to open the figure print preview.');
  const doc = preview.document;
  doc.title = title;
  const style = doc.createElement('style');
  style.textContent = `body{margin:16px;font:14px system-ui;background:white;color:#222}
    .print-controls{display:flex;gap:16px;align-items:center;margin-bottom:16px}
    button{font:inherit;padding:6px 16px;cursor:pointer}img{display:block;max-width:100%;height:auto}
    @media print{body{margin:0}.print-controls{display:none}img{max-width:100%;break-inside:avoid}}`;
  doc.head.appendChild(style);
  const controls = doc.createElement('div');
  controls.className = 'print-controls';
  const button = doc.createElement('button');
  button.textContent = 'Print';
  button.disabled = true;
  button.onclick = () => {
    preview.focus();
    preview.print();
  };
  const status = doc.createElement('span');
  status.textContent = 'Preparing figure…';
  status.setAttribute('role', 'status');
  controls.append(button, status);
  const image = doc.createElement('img');
  image.alt = title;
  doc.body.replaceChildren(controls, image);
  let url: string | undefined;
  try {
    const blob = await render();
    if (preview.closed) return;
    url = URL.createObjectURL(blob);
    preview.addEventListener('pagehide', () => URL.revokeObjectURL(url!), { once: true });
    await new Promise<void>((resolve, reject) => {
      image.onload = () => resolve();
      image.onerror = () => reject(new Error('The figure print preview could not be loaded.'));
      image.src = url!;
    });
    if (preview.closed) return;
    status.textContent = 'Figure ready to print';
    button.disabled = false;
    preview.focus();
    preview.print();
  } catch (error) {
    if (url) URL.revokeObjectURL(url);
    preview.close();
    throw error;
  }
}
//=============================================================================
