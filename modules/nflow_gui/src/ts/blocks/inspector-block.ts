//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { Block } from './inspector.js';
import * as FieldNames from '../FieldNames.js';
import { localizeDOM } from '../utils/i18n.js';
//=============================================================================
export type BlockPanelDeps = {
  inspectorBody: HTMLElement | null;
  callUpdate: (b: Block) => void;
  callResize: (b: Block, w: number, h: number) => void;
  renderScope?: (block: Block) => void;
  signalDiagramChanged?: () => void;
  getRuntimeSeconds?: () => number;
  renderInspector: (block: Block | null) => void;
  onOpenSubsystem?: (block: Block) => void;
  onSaveSubsystem?: (block: Block) => void;
  onGenerateBlockCode?: (block: Block) => void;
};
//=============================================================================
import { setupParamVisibility, applyDataEditNames } from './inspector-utils.js';
import './renderers/generators.js';
import './renderers/scopes.js';
import './renderers/controls.js';
import './renderers/io-transfer.js';
import './renderers/discrete.js';
import './renderers/math.js';
import './renderers/lookup.js';
import './renderers/sinks.js';
import './renderers/display.js';
import './renderers/logic.js';
import './renderers/utility.js';
import './renderers/subsystem.js';
import './renderers/fmi.js';
import './renderers/modelica.js';
import './renderers/userdefined.js';
import './renderers/dashboard.js';
import { renderWithRegistry } from './inspector-registry.js';
import { createInspectorCapabilities } from './components/inspector-capabilities.js';
import { readInspectorSectionOpen, writeInspectorSectionOpen } from './inspector-section-prefs.js';
import { openBlockHelp } from '../app/block-help.js';
export { applyDataEditNames } from './inspector-utils.js';
//=============================================================================
export function renderBlockPanel(block: Block, deps: BlockPanelDeps): void {
  const { inspectorBody, callUpdate } = deps;

  if (renderWithRegistry(block, deps)) {
    // handled by registered renderers
  } else {
    if (inspectorBody) inspectorBody.textContent = '';
  }

  // Title at the top of the inspector: params.name, else params.label, else block.label,
  // else block.type. Kept up to date when the user edits the name/label inputs.
  if (inspectorBody) {
    const computeCurrentName = () => {
      const nameInput = inspectorBody!.querySelector<HTMLInputElement | HTMLTextAreaElement>(
        `input[data-edit="${FieldNames.kName}"], input[data-edit="${FieldNames.kLabel}"], textarea[data-edit="${FieldNames.kName}"], textarea[data-edit="${FieldNames.kLabel}"]`
      );
      if (nameInput) {
        const v = (nameInput as HTMLInputElement | HTMLTextAreaElement).value;
        if (v != null && String(v).trim() !== '') return String(v).trim();
      }
      if (block.params && typeof block.params === 'object') {
        const pName = (block.params as Record<string, unknown>)[FieldNames.kName];
        if (pName != null && String(pName).trim() !== '') return String(pName).trim();
        const pLabel = (block.params as Record<string, unknown>)[FieldNames.kLabel];
        if (pLabel != null && String(pLabel).trim() !== '') return String(pLabel).trim();
      }
      if (block.label) return String(block.label);
      return String(block.type ?? '');
    };
    const initialName = String(
      block.params?.[FieldNames.kName] ||
        block.params?.[FieldNames.kLabel] ||
        block.label ||
        block.type ||
        ''
    );

    let titleEl = inspectorBody.querySelector<HTMLElement>('.inspector-title');
    if (!titleEl) {
      titleEl = document.createElement('div');
      titleEl.className = 'inspector-title';

      const icon = document.createElement('span');
      icon.className = 'inspector-icon';
      titleEl.appendChild(icon);

      const titleCopy = document.createElement('span');
      titleCopy.className = 'inspector-block-copy';

      const selectedName = document.createElement('strong');
      selectedName.className = 'inspector-selected-name';
      titleCopy.appendChild(selectedName);

      const typeText = document.createElement('small');
      typeText.className = 'inspector-type-text';
      titleCopy.appendChild(typeText);
      titleEl.appendChild(titleCopy);

      const helpButton = document.createElement('button');
      helpButton.type = 'button';
      helpButton.className = 'inspector-help-button';
      helpButton.textContent = '?';
      helpButton.dataset.i18nTitle = 'Block Help';
      helpButton.dataset.i18nAria = 'Block Help';
      helpButton.title = 'Block Help';
      helpButton.setAttribute('aria-label', 'Block Help');
      titleEl.appendChild(helpButton);

      const parameters = document.createElement('details');
      parameters.className = 'inspector-disclosure inspector-parameters';
      parameters.dataset.inspectorSection = 'parameters';
      parameters.open = readInspectorSectionOpen('parameters', true);
      parameters.addEventListener('toggle', () =>
        writeInspectorSectionOpen('parameters', parameters.open)
      );

      const parametersHeading = document.createElement('summary');
      parametersHeading.className = 'inspector-section-heading';
      parametersHeading.dataset.i18n = 'Parameters';
      parametersHeading.textContent = 'Parameters';

      const parametersContent = document.createElement('div');
      parametersContent.className = 'inspector-parameters-content';
      parameters.append(parametersHeading, parametersContent);

      const capabilities = createInspectorCapabilities(block);

      const nameWrap = document.createElement('div');
      nameWrap.className = 'inspector-name-wrap';

      const nameLabel = document.createElement('span');
      nameLabel.className = 'inspector-name-label';
      nameLabel.dataset.i18n = 'Name';
      nameLabel.textContent = 'Name';
      nameWrap.appendChild(nameLabel);

      const nameInput = document.createElement('input');
      nameInput.className = 'inspector-name-input';
      nameInput.setAttribute('data-edit', FieldNames.kName);
      // Prevent browser autofill for this app-specific field
      try {
        nameInput.setAttribute('autocomplete', 'off');
      } catch {}
      nameWrap.appendChild(nameInput);

      const renderedControls = Array.from(inspectorBody.childNodes);
      parametersContent.append(nameWrap, ...renderedControls);
      inspectorBody.append(titleEl, capabilities, parameters);

      nameInput.addEventListener('input', () => {
        try {
          if (block) {
            if (!block.params) block.params = {};
            block.params[FieldNames.kName] = nameInput.value.trim();
            callUpdate(block);
          }
        } catch (_e) {}
      });
    }

    const nameInputEl = inspectorBody.querySelector<HTMLInputElement>('.inspector-name-input');
    const selectedNameEl = titleEl.querySelector<HTMLElement>('.inspector-selected-name');
    const typeTextEl = titleEl.querySelector<HTMLElement>('.inspector-type-text');
    const iconEl = titleEl.querySelector<HTMLElement>('.inspector-icon');
    const helpButtonEl = titleEl.querySelector<HTMLButtonElement>('.inspector-help-button');

    if (nameInputEl) nameInputEl.value = initialName;
    if (selectedNameEl) selectedNameEl.textContent = initialName;
    if (typeTextEl) typeTextEl.textContent = String(block.type ?? 'Block');
    if (iconEl) iconEl.setAttribute('data-type', String(block.type || ''));
    if (helpButtonEl) {
      helpButtonEl.onclick = (event) => {
        event.stopPropagation();
        void openBlockHelp(block);
      };
    }
    const contextTitle = document.getElementById('inspectorContextTitle');
    if (contextTitle) contextTitle.textContent = initialName;

    // Update the Properties panel header icon to show the same SVG icon as the
    // palette button.  Fall back to the coloured-square if the SVG is not found.
    try {
      const propsIcon = document.querySelector(
        '#panel-properties .sidebar-header .sidebar-properties-icon'
      ) as HTMLElement | null;
      if (propsIcon) {
        const blockType = String(block.type || '');
        propsIcon.setAttribute('data-type', blockType);
        const safeType = blockType.replace(/[^a-zA-Z0-9_-]/g, '');
        const paletteSvg = safeType
          ? (document.querySelector(
              `#blockLibraryGroups .tool[data-type="${safeType}"] .tool-icon svg`
            ) as SVGElement | null)
          : null;
        if (paletteSvg) {
          const clone = paletteSvg.cloneNode(true) as SVGElement;
          clone.setAttribute('width', '22');
          clone.setAttribute('height', '22');
          propsIcon.innerHTML = '';
          propsIcon.appendChild(clone);
          if (iconEl) {
            iconEl.replaceChildren(clone.cloneNode(true));
          }
        } else {
          propsIcon.innerHTML = '';
          if (iconEl)
            iconEl.textContent = String(block.type || '?')
              .charAt(0)
              .toUpperCase();
        }
      }
    } catch (e) {}

    if (!inspectorBody.dataset.inspectorTitleBound) {
      inspectorBody.dataset.inspectorTitleBound = 'true';
      inspectorBody.addEventListener(
        'input',
        (event) => {
          const edited = event.target as HTMLInputElement | HTMLTextAreaElement | null;
          const editName = edited?.getAttribute('data-edit');
          const nameInput =
            editName === FieldNames.kName || editName === FieldNames.kLabel ? edited : null;
          const el = inspectorBody!.querySelector<HTMLInputElement>('.inspector-name-input');
          if (nameInput) {
            const value = (nameInput as HTMLInputElement | HTMLTextAreaElement).value;
            if (el) el.value = value;
            if (selectedNameEl) selectedNameEl.textContent = value;
            if (contextTitle) contextTitle.textContent = value;
          } else {
            if (el) el.value = computeCurrentName();
          }
        },
        true
      );
    }
  }

  applyDataEditNames(inspectorBody, (block && (block.id ?? '')) as string | undefined);
  setupParamVisibility(block, inspectorBody, callUpdate);
  try {
    if (inspectorBody) {
      // Mark legends for localization using their visible text (ignore chevrons/children)
      Array.from(
        inspectorBody.querySelectorAll<HTMLElement>('fieldset.param-group legend')
      ).forEach((legend) => {
        if (legend.hasAttribute('data-i18n')) return;
        const text = Array.from(legend.childNodes)
          .filter((node) => node.nodeType === Node.TEXT_NODE)
          .map((node) => node.textContent || '')
          .join(' ')
          .trim();
        if (text) legend.setAttribute('data-i18n', text);
      });

      // Mark param-name elements for localization so `localizeDOM` can translate them
      Array.from(inspectorBody.querySelectorAll<HTMLElement>('.param-name')).forEach((el) => {
        if (el.hasAttribute('data-i18n')) return;
        const txt = (el.textContent || '').trim();
        if (txt) el.setAttribute('data-i18n', txt);
      });

      localizeDOM(inspectorBody);
    }
  } catch (e) {}
}
//=============================================================================
