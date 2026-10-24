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
import { writeStatus } from './status-service.js';
import { state, signalDiagramChanged } from './state.js';
import { parseVariables } from '../utils/expr.js';
import { t } from '../utils/i18n.js';
//=============================================================================
// Diagram variables editor: apply, reset, preview and section collapse.
export function initVariablesPanel(
  variablesInput: HTMLTextAreaElement | null,
  variablesPreview: HTMLElement | null,
  statusEl: HTMLElement | null
): void {
  const applyVariablesBtn = document.getElementById('applyVariables');
  const setVariablesSectionCollapsed = (collapsed: boolean) => {
    try {
      const content =
        (variablesInput && (variablesInput.closest('.sidebar-section-content') as HTMLElement)) ||
        (document.querySelector('.sidebar-section-content.variables-editor') as HTMLElement | null);
      if (!content) return;
      if (collapsed) content.classList.add('collapsed');
      else content.classList.remove('collapsed');
      const title = content.previousElementSibling as HTMLElement | null;
      if (title && title.classList.contains('sidebar-section-title')) {
        if (collapsed) title.classList.add('collapsed');
        else title.classList.remove('collapsed');
        try {
          title.setAttribute('aria-expanded', String(!collapsed));
        } catch (_e) {}
        const toggle = title.querySelector('.section-toggle') as HTMLElement | null;
        if (toggle) toggle.textContent = collapsed ? '▸' : '▾';
      }
    } catch (_e) {}
  };
  const updateVariables = (announce = true) => {
    state.variablesText = variablesInput?.value || '';
    const parsed = parseVariables(state.variablesText);
    state.variables = parsed.vars;
    state.variablesDisplay = parsed.display;
    if (variablesPreview) {
      const entries = state.variablesDisplay.join('\n');
      variablesPreview.textContent = entries || t('No variables defined.');
    }
    try {
      setVariablesSectionCollapsed((state.variablesDisplay?.length || 0) === 0);
    } catch (_e) {}
    // Only announce user-triggered updates (silent during programmatic init).
    if (announce) writeStatus(statusEl, t('Variables updated'));
    signalDiagramChanged();
  };
  if (applyVariablesBtn)
    (applyVariablesBtn as HTMLElement).addEventListener('click', () => updateVariables());
  if (variablesInput) variablesInput.addEventListener('change', () => updateVariables());
  // expand section as user types, but only collapse via updateVariables/reset
  if (variablesInput)
    variablesInput.addEventListener('input', () => {
      try {
        if ((variablesInput as HTMLTextAreaElement).value.trim() !== '')
          setVariablesSectionCollapsed(false);
      } catch (_e) {}
    });
  if (variablesInput) updateVariables(false);

  const resetVariablesBtn = document.getElementById('resetVariables');
  if (resetVariablesBtn)
    (resetVariablesBtn as HTMLElement).addEventListener('click', () => {
      state.variablesText = '';
      if (variablesInput) variablesInput.value = '';
      const parsed = parseVariables(state.variablesText);
      state.variables = parsed.vars;
      state.variablesDisplay = parsed.display;
      if (variablesPreview) {
        const entries = state.variablesDisplay.join('\n');
        variablesPreview.textContent = entries || t('No variables defined.');
      }
      writeStatus(statusEl, t('Variables reset'));
      try {
        setVariablesSectionCollapsed(true);
      } catch (_e) {}
      signalDiagramChanged();
    });
}
//=============================================================================
