//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  forwardRef,
  useRef,
  type ClipboardEvent,
  type FormEvent,
  type KeyboardEvent,
  type MouseEvent,
} from 'react';
import { tokenizeNelson } from '../editor/nelsonSyntax';
//=============================================================================
type CommandPromptProps = {
  value: string;
  disabled?: boolean;
  wrapLines: boolean;
  onChange: (event: FormEvent<HTMLTextAreaElement>) => void;
  onKeyDown: (event: KeyboardEvent<HTMLTextAreaElement>) => void;
  onPaste: (event: ClipboardEvent<HTMLTextAreaElement>) => void;
  onContextMenu: (event: MouseEvent<HTMLTextAreaElement>) => void;
};
//=============================================================================
export const CommandPrompt = forwardRef<HTMLTextAreaElement, CommandPromptProps>(
  ({ value, disabled = false, wrapLines, onChange, onKeyDown, onPaste, onContextMenu }, ref) => {
    const highlightRef = useRef<HTMLPreElement>(null);
    const syncScroll = (textarea: HTMLTextAreaElement) => {
      if (!highlightRef.current) return;
      highlightRef.current.scrollTop = textarea.scrollTop;
      highlightRef.current.scrollLeft = textarea.scrollLeft;
    };
    return (
      <span className="console-input-stack" data-wrap={wrapLines ? 'on' : 'off'}>
        <pre ref={highlightRef} className="console-input-highlight" aria-hidden="true">
          {tokenizeNelson(value).map((token, index) => (
            <span className={`console-syntax--${token.kind}`} key={index}>
              {token.text}
            </span>
          ))}
        </pre>
        <textarea
          ref={ref}
          className="console-input"
          aria-label="Command input"
          aria-keyshortcuts="Enter Shift+Enter"
          title="Enter to execute; Shift+Enter for a new line"
          value={value}
          disabled={disabled}
          rows={Math.min(12, Math.max(1, value.split('\n').length))}
          wrap={wrapLines ? 'soft' : 'off'}
          spellCheck={false}
          autoComplete="off"
          autoCapitalize="off"
          onContextMenu={onContextMenu}
          onChange={onChange}
          onKeyDown={onKeyDown}
          onPaste={onPaste}
          onScroll={(event) => syncScroll(event.currentTarget)}
        />
      </span>
    );
  }
);
//=============================================================================
CommandPrompt.displayName = 'CommandPrompt';
//=============================================================================
