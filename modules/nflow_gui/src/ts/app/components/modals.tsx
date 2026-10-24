//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useRef, useState, type ChangeEvent, type MouseEvent } from 'react';
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import { localizeDOM } from '../../utils/i18n.js';
import {
  ABOUT_HIDE_EVENT,
  ABOUT_SHOW_EVENT,
  CONFIRM_SAVE_EVENT,
  GRID_SETTINGS_HIDE_EVENT,
  GRID_SETTINGS_SHOW_EVENT,
  MISSING_BLOCKS_EVENT,
} from './modal-events.js';
import { FilePickerHost } from './file-picker-host.js';
import { ImportFmuModal } from './import-fmu-modal.js';
import { ExampleBrowserModal } from '../example-browser.js';
import { NFLOW_VERSION } from '../version.js';
//=============================================================================
const MODALS_ROOT_ID = 'nflowModalsRoot';
export {
  ABOUT_HIDE_EVENT,
  ABOUT_SHOW_EVENT,
  CONFIRM_SAVE_EVENT,
  GRID_SETTINGS_HIDE_EVENT,
  GRID_SETTINGS_SHOW_EVENT,
  MISSING_BLOCKS_EVENT,
};
//=============================================================================
type ConfirmSaveCallback = (result: boolean | null) => void;
type MissingBlock = { type: string; libraryId?: string; libraryTitle?: string };
let modalsRoot: Root | null = null;
let modalsHost: HTMLElement | null = null;
function ensureModalsHost(): HTMLElement {
  let host = document.getElementById(MODALS_ROOT_ID);
  if (!host) {
    host = document.createElement('div');
    host.id = MODALS_ROOT_ID;
    host.dataset.reactRoot = 'modals';
    host.style.display = 'contents';
    document.body.appendChild(host);
  }
  if (host !== modalsHost) {
    modalsHost = host;
    modalsRoot = createRoot(host);
  }
  return host;
}
//=============================================================================
function ConfirmSaveModal(): JSX.Element {
  const [open, setOpen] = useState(false);
  const callbackRef = useRef<ConfirmSaveCallback | null>(null);

  useEffect(() => {
    const onConfirmSave = (event: Event) => {
      const customEvent = event as CustomEvent<{ callback?: ConfirmSaveCallback }>;
      if (typeof customEvent.detail?.callback !== 'function') return;
      event.preventDefault();
      callbackRef.current = customEvent.detail.callback;
      setOpen(true);
    };

    document.addEventListener(CONFIRM_SAVE_EVENT, onConfirmSave);
    return () => document.removeEventListener(CONFIRM_SAVE_EVENT, onConfirmSave);
  }, []);

  const resolve = useCallback((result: boolean | null) => {
    const callback = callbackRef.current;
    callbackRef.current = null;
    setOpen(false);
    callback?.(result);
  }, []);

  const onBackdropClick = useCallback(
    (event: MouseEvent<HTMLDivElement>) => {
      if (event.target === event.currentTarget) resolve(null);
    },
    [resolve]
  );

  return (
    <div
      className={open ? 'modal show' : 'modal'}
      id="confirmSaveModal"
      role="presentation"
      onClick={onBackdropClick}
    >
      <div className="modal-content" role="dialog" aria-modal="true" aria-label="Save Changes?">
        <div className="modal-header">
          <h3 data-i18n="Save Changes?">Save Changes?</h3>
          <button className="modal-close" id="closeConfirmSaveModal" onClick={() => resolve(null)}>
            &times;
          </button>
        </div>
        <div className="modal-body">
          <p data-i18n="Do you want to save your changes before proceeding?">
            Do you want to save your changes before proceeding?
          </p>
        </div>
        <div className="modal-footer">
          <button
            className="btn btn-secondary"
            id="dontSaveConfirm"
            data-i18n="Don't Save"
            onClick={() => resolve(false)}
          >
            Don't Save
          </button>
          <button
            className="btn btn-primary"
            id="saveConfirm"
            data-i18n="Save"
            onClick={() => resolve(true)}
          >
            Save
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function AboutModal(): JSX.Element {
  const [open, setOpen] = useState(false);

  useEffect(() => {
    const show = (event: Event) => {
      event.preventDefault();
      setOpen(true);
    };
    const hide = (event: Event) => {
      event.preventDefault();
      setOpen(false);
    };

    document.addEventListener(ABOUT_SHOW_EVENT, show);
    document.addEventListener(ABOUT_HIDE_EVENT, hide);
    return () => {
      document.removeEventListener(ABOUT_SHOW_EVENT, show);
      document.removeEventListener(ABOUT_HIDE_EVENT, hide);
    };
  }, []);

  const hide = useCallback(() => {
    document.dispatchEvent(new CustomEvent(ABOUT_HIDE_EVENT, { cancelable: true }));
  }, []);

  const onBackdropClick = useCallback(
    (event: MouseEvent<HTMLDivElement>) => {
      if (event.target === event.currentTarget) hide();
    },
    [hide]
  );

  return (
    <div
      className={open ? 'modal show' : 'modal'}
      id="aboutModal"
      role="presentation"
      onClick={onBackdropClick}
    >
      <div
        className="modal-content dialog-card about-card"
        role="dialog"
        aria-modal="true"
        aria-label="About NFlow"
      >
        <button
          className="modal-close about-card__close"
          id="closeAboutModal"
          data-i18n-title="Close"
          title="Close"
          onClick={hide}
        >
          &times;
        </button>
        <div className="about-card__title" data-i18n="NFlow">
          NFlow
        </div>
        <div className="about-card__version">
          <span data-i18n="Version:">Version:</span> <span id="aboutVersion">{NFLOW_VERSION}</span>
        </div>
        <p
          className="about-card__desc"
          data-i18n="NFlow is Nelson's visual block diagram environment for modeling, simulating, and inspecting dynamic systems."
        >
          NFlow is Nelson's visual block diagram environment for modeling, simulating, and
          inspecting dynamic systems.
        </p>
        <div className="about-card__links">
          <a
            href="https://nelson-lang.github.io/nelson-website/"
            target="_blank"
            rel="noopener noreferrer"
          >
            Website
          </a>
          <a href="https://github.com/nelson-lang/nelson" target="_blank" rel="noopener noreferrer">
            Source
          </a>
          <a
            href="https://nelson-lang.github.io/nelson-gitbook/releases/en_US/latest/index.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            Documentation
          </a>
        </div>
        <div className="modal-footer">
          <button
            className="dialog-btn dialog-btn--default"
            id="closeAboutModalBtn"
            data-i18n="OK"
            onClick={hide}
          >
            OK
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function GridSettingsModal(): JSX.Element {
  const [open, setOpen] = useState(false);

  useEffect(() => {
    const show = (event: Event) => {
      event.preventDefault();
      setOpen(true);
    };
    const hide = (event: Event) => {
      event.preventDefault();
      setOpen(false);
    };

    document.addEventListener(GRID_SETTINGS_SHOW_EVENT, show);
    document.addEventListener(GRID_SETTINGS_HIDE_EVENT, hide);
    return () => {
      document.removeEventListener(GRID_SETTINGS_SHOW_EVENT, show);
      document.removeEventListener(GRID_SETTINGS_HIDE_EVENT, hide);
    };
  }, []);

  const hide = useCallback(() => {
    document.dispatchEvent(new CustomEvent(GRID_SETTINGS_HIDE_EVENT, { cancelable: true }));
  }, []);

  const onBackdropClick = useCallback(
    (event: MouseEvent<HTMLDivElement>) => {
      if (event.target === event.currentTarget) hide();
    },
    [hide]
  );

  const syncInputValues = useCallback((event: ChangeEvent<HTMLInputElement>) => {
    const value = event.currentTarget.value;
    const input = document.getElementById('gridSizeInput') as HTMLInputElement | null;
    const slider = document.getElementById('gridSizeSlider') as HTMLInputElement | null;
    if (input && input.value !== value) input.value = value;
    if (slider && slider.value !== value) slider.value = value;
  }, []);

  return (
    <div
      id="gridSettingsModal"
      className={open ? 'modal show' : 'modal'}
      role="presentation"
      onClick={onBackdropClick}
    >
      <div className="modal-content" role="dialog" aria-modal="true" aria-label="Grid Settings">
        <div className="modal-header">
          <h3 data-i18n="Grid Settings">Grid Settings</h3>
          <button className="modal-close" data-i18n-title="Close" title="Close" onClick={hide}>
            &times;
          </button>
        </div>
        <div className="modal-body">
          <div className="control-group">
            <label htmlFor="gridSizeInput" data-i18n="Grid Size (px)">
              Grid Size (px)
            </label>
            <div className="control-group-inline">
              <input
                type="number"
                id="gridSizeInput"
                min="5"
                max="100"
                defaultValue="20"
                className="input"
                onInput={syncInputValues}
              />
              <input
                type="range"
                id="gridSizeSlider"
                min="5"
                max="100"
                defaultValue="20"
                className="slider"
                onInput={syncInputValues}
              />
            </div>
            <div className="control-note" data-i18n="Adjust the grid spacing between 5px and 100px">
              Adjust the grid spacing between 5px and 100px
            </div>
          </div>
        </div>
        <div className="modal-footer">
          <button id="gridSettingsApplyBtn" className="btn" data-i18n="Apply">
            Apply
          </button>
          <button
            id="gridSettingsCancelBtn"
            className="btn btn-secondary"
            data-i18n="Cancel"
            onClick={hide}
          >
            Cancel
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function MissingBlocksModal(): JSX.Element {
  const [missingBlocks, setMissingBlocks] = useState<MissingBlock[]>([]);
  const [open, setOpen] = useState(false);

  useEffect(() => {
    const onMissingBlocks = (event: Event) => {
      const customEvent = event as CustomEvent<{ missingBlocks?: MissingBlock[] }>;
      if (!Array.isArray(customEvent.detail?.missingBlocks)) return;
      event.preventDefault();
      setMissingBlocks(customEvent.detail.missingBlocks);
      setOpen(true);
    };

    document.addEventListener(MISSING_BLOCKS_EVENT, onMissingBlocks);
    return () => document.removeEventListener(MISSING_BLOCKS_EVENT, onMissingBlocks);
  }, []);

  const hide = useCallback(() => setOpen(false), []);
  const onBackdropClick = useCallback(
    (event: MouseEvent<HTMLDivElement>) => {
      if (event.target === event.currentTarget) hide();
    },
    [hide]
  );

  return (
    <div
      id="missingBlocksModal"
      className={open ? 'modal show' : 'modal'}
      role="presentation"
      onClick={onBackdropClick}
    >
      <div className="modal-content" role="dialog" aria-modal="true" aria-label="Missing Blocks">
        <div className="modal-header">
          <h3 data-i18n="Missing Blocks">Missing Blocks</h3>
          <button
            id="closeMissingBlocksModal"
            className="modal-close"
            data-i18n-title="Close"
            title="Close"
            onClick={hide}
          >
            &times;
          </button>
        </div>
        <div className="modal-body">
          <p data-i18n="Cannot open diagram. Missing block types:">
            Cannot open diagram. Missing block types:
          </p>
          <ul id="missingBlocksList">
            {missingBlocks.map((block) => (
              <li key={`${block.type}:${block.libraryTitle ?? block.libraryId ?? ''}`}>
                <code>{block.type}</code>
                {block.libraryTitle ? (
                  <span className="missing-blocks-lib"> - library: {block.libraryTitle}</span>
                ) : (
                  <span className="missing-blocks-lib missing-blocks-unknown">
                    {' '}
                    - unknown library
                  </span>
                )}
              </li>
            ))}
          </ul>
        </div>
        <div className="modal-footer">
          <button id="closeMissingBlocksBtn" className="btn" data-i18n="Close" onClick={hide}>
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function NFlowModals(): JSX.Element {
  return (
    <>
      <ConfirmSaveModal />
      <AboutModal />
      <GridSettingsModal />
      <MissingBlocksModal />
      <FilePickerHost />
      <ImportFmuModal />
      <ExampleBrowserModal />
    </>
  );
}
//=============================================================================
export function mountModals(): void {
  if (document.getElementById('confirmSaveModal')) return;
  const host = ensureModalsHost();
  flushSync(() => {
    modalsRoot?.render(<NFlowModals />);
  });
  try {
    localizeDOM(host);
  } catch (e) {
    /* ignore localization errors */
  }
}
//=============================================================================
