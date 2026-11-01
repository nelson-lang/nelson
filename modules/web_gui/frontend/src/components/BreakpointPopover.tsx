//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// right-click menu of the breakpoint gutter with the inline condition editor
//=============================================================================
import { useEffect, useRef, useState } from 'react';
import { t } from '../i18n';
//=============================================================================
export interface BreakpointPopoverProps {
  line: number;
  x: number;
  y: number;
  hasBreakpoint: boolean;
  enabled: boolean;
  condition: string;
  hasAny: boolean;
  // message when the condition does not parse
  conditionError?: string;
  onToggle: () => void;
  onSetCondition: (condition: string) => void;
  onSetEnabled: (enabled: boolean) => void;
  onEnableAllInFile: () => void;
  onDisableAllInFile: () => void;
  onClearAllInFile: () => void;
  onClearAll: () => void;
  onClose: () => void;
}
//=============================================================================
export function BreakpointPopover(props: BreakpointPopoverProps) {
  const [editing, setEditing] = useState(false);
  const [condition, setCondition] = useState(props.condition);
  const inputRef = useRef<HTMLInputElement>(null);
  const rootRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (editing) inputRef.current?.focus();
  }, [editing]);

  useEffect(() => {
    const onDown = (e: MouseEvent) => {
      if (rootRef.current && !rootRef.current.contains(e.target as Node)) props.onClose();
    };
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape') props.onClose();
    };
    window.addEventListener('mousedown', onDown);
    window.addEventListener('keydown', onKey);
    return () => {
      window.removeEventListener('mousedown', onDown);
      window.removeEventListener('keydown', onKey);
    };
  }, [props]);

  const item = (label: string, onClick: () => void, disabled = false) => (
    <button
      type="button"
      className="bp-popover__item"
      disabled={disabled}
      onClick={() => {
        onClick();
      }}
    >
      {label}
    </button>
  );

  return (
    <div
      ref={rootRef}
      className="bp-popover"
      style={{ left: props.x, top: props.y }}
      role="menu"
      data-testid="breakpoint-popover"
    >
      {editing ? (
        <form
          className="bp-popover__condition"
          onSubmit={(e) => {
            e.preventDefault();
            props.onSetCondition(condition);
          }}
        >
          <label className="bp-popover__label">
            {t('Pause on line %1 only when this expression is true:').replace(
              '%1',
              String(props.line)
            )}
          </label>
          <input
            ref={inputRef}
            className="bp-popover__input"
            value={condition}
            onChange={(e) => setCondition(e.target.value)}
            spellCheck={false}
            data-testid="breakpoint-condition-input"
          />
          {props.conditionError && (
            <div className="bp-popover__error" role="alert">
              {props.conditionError}
            </div>
          )}
          <div className="bp-popover__buttons">
            <button type="submit" className="editor-toolbar__btn">
              {t('Apply')}
            </button>
            <button type="button" className="editor-toolbar__btn" onClick={props.onClose}>
              {t('Cancel')}
            </button>
          </div>
        </form>
      ) : (
        <>
          {item(props.hasBreakpoint ? t('Clear Breakpoint') : t('Set Breakpoint'), () => {
            props.onToggle();
            props.onClose();
          })}
          {item(
            props.hasBreakpoint ? t('Set/Modify Condition...') : t('Set Conditional Breakpoint...'),
            () => setEditing(true)
          )}
          {props.hasBreakpoint &&
            item(props.enabled ? t('Disable Breakpoint') : t('Enable Breakpoint'), () => {
              props.onSetEnabled(!props.enabled);
              props.onClose();
            })}
          <div className="bp-popover__separator" />
          {item(
            t('Enable All Breakpoints in File'),
            () => {
              props.onEnableAllInFile();
              props.onClose();
            },
            !props.hasAny
          )}
          {item(
            t('Disable All Breakpoints in File'),
            () => {
              props.onDisableAllInFile();
              props.onClose();
            },
            !props.hasAny
          )}
          {item(
            t('Clear All Breakpoints in File'),
            () => {
              props.onClearAllInFile();
              props.onClose();
            },
            !props.hasAny
          )}
          {item(t('Clear All Breakpoints'), () => {
            props.onClearAll();
            props.onClose();
          })}
        </>
      )}
    </div>
  );
}
//=============================================================================
