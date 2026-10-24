//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
//=============================================================================
type WireLabelEditorProps = {
  x: number;
  y: number;
  value: string;
  placeholder: string;
  onCommit: (save: boolean, value: string) => void;
};
//=============================================================================
let editorRoot: Root | null = null;
let editorHost: HTMLDivElement | null = null;
function WireLabelEditor({
  x,
  y,
  value,
  placeholder,
  onCommit,
}: WireLabelEditorProps): JSX.Element {
  return (
    <div
      id="wireLabelEditor"
      style={{
        position: 'fixed',
        left: `${x}px`,
        top: `${y}px`,
        zIndex: 1000,
      }}
    >
      <input
        id="wireLabelInput"
        type="text"
        defaultValue={value}
        placeholder={placeholder}
        aria-label={placeholder}
        onKeyDown={(event) => {
          if (event.key === 'Enter') {
            event.preventDefault();
            onCommit(true, event.currentTarget.value);
          } else if (event.key === 'Escape') {
            event.preventDefault();
            onCommit(false, event.currentTarget.value);
          }
        }}
        onBlur={(event) => {
          const currentValue = event.currentTarget.value;
          setTimeout(() => onCommit(true, currentValue), 100);
        }}
        autoFocus
      />
    </div>
  );
}
//=============================================================================
export function closeWireLabelEditor(): void {
  if (editorRoot) {
    flushSync(() => {
      editorRoot?.unmount();
    });
  }
  editorRoot = null;
  editorHost?.remove();
  editorHost = null;
}
//=============================================================================
export function renderWireLabelEditor(props: WireLabelEditorProps): HTMLInputElement | null {
  closeWireLabelEditor();
  editorHost = document.createElement('div');
  editorHost.id = 'wireLabelEditorRoot';
  document.body.appendChild(editorHost);
  editorRoot = createRoot(editorHost);
  flushSync(() => {
    editorRoot?.render(<WireLabelEditor {...props} />);
  });
  return document.getElementById('wireLabelInput') as HTMLInputElement | null;
}
//=============================================================================
