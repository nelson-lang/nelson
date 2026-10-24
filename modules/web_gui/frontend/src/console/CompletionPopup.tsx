//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createPortal } from 'react-dom';
//=============================================================================
export type CompletionItem = { text: string; kind: string };
export type CompletionPopupState = {
  items: CompletionItem[];
  sel: number;
  prefix: string;
  anchor: { left: number; bottom: number; maxHeight: number };
};
//=============================================================================
type CompletionPopupProps = {
  popup: CompletionPopupState | null;
  onAccept: (item: CompletionItem, prefix: string) => void;
};
//=============================================================================
export function CompletionPopup({ popup, onAccept }: CompletionPopupProps): JSX.Element | null {
  if (!popup) return null;
  return createPortal(
    <div
      className="console-popup"
      // Dockview uses transformed containers. The portal keeps fixed viewport
      // coordinates aligned with the textarea caret.
      style={popup.anchor}
    >
      {popup.items.map((item, index) => (
        <div
          key={item.text + index}
          className={'console-popup__item' + (index === popup.sel ? ' sel' : '')}
          onMouseDown={(event) => {
            event.preventDefault();
            onAccept(item, popup.prefix);
          }}
        >
          <span>{item.text}</span>
          <span className="console-popup__kind">{item.kind}</span>
        </div>
      ))}
    </div>,
    document.querySelector('.nelson-shell') ?? document.body
  );
}
//=============================================================================
