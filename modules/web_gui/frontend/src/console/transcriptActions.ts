//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
function cloneTranscript(host: HTMLElement): HTMLElement {
  const body = host.cloneNode(true) as HTMLElement;
  body.querySelectorAll('.console-input-stack, .console-measure').forEach((node) => node.remove());
  return body;
}
//=============================================================================
export function selectElementContents(element: HTMLElement): void {
  const range = document.createRange();
  range.selectNodeContents(element);
  const selection = window.getSelection();
  selection?.removeAllRanges();
  selection?.addRange(range);
}
//=============================================================================
export function exportConsoleHtml(host: HTMLElement): void {
  const body = cloneTranscript(host);
  const doc =
    '<!doctype html><meta charset="utf-8"><title>Nelson command window</title>' +
    '<style>body{background:#1e1e1e;color:#d4d4d4;font-family:Consolas,monospace;font-size:13px;white-space:pre-wrap}</style>' +
    body.innerHTML;
  const url = URL.createObjectURL(new Blob([doc], { type: 'text/html' }));
  const link = document.createElement('a');
  link.href = url;
  link.download = 'nelson-command-window.html';
  link.click();
  URL.revokeObjectURL(url);
}
//=============================================================================
export function printConsole(host: HTMLElement): void {
  const body = cloneTranscript(host);
  const popup = window.open('', '_blank', 'width=800,height=600');
  if (!popup) return;
  popup.document.write(
    '<!doctype html><meta charset="utf-8"><title>Nelson command window</title>' +
      '<style>body{font-family:Consolas,monospace;font-size:13px;white-space:pre-wrap}</style>' +
      body.innerHTML +
      '<script>onload=function(){setTimeout(function(){print();close();},50);}</script>'
  );
  popup.document.close();
}
//=============================================================================
