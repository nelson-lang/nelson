//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Block } from '../inspector.js';
import { getBlockCapabilities, type CodegenCapability } from '../block-capabilities.js';
import { readInspectorSectionOpen, writeInspectorSectionOpen } from '../inspector-section-prefs.js';
import { openBlockHelp, openHelpTopic } from '../../app/block-help.js';
import { t } from '../../utils/i18n.js';
//=============================================================================
const CODEGEN_COPY: Record<CodegenCapability, { label: string; title: string }> = {
  supported: {
    label: 'Supported',
    title: 'This block has direct code-generation support.',
  },
  'model-dependent': {
    label: 'Model dependent',
    title: 'Support is determined after the complete diagram is lowered.',
  },
  tunable: {
    label: 'Tunable',
    title: 'This control becomes a typed tunable field in generated code.',
  },
  instrumentation: {
    label: 'Instrumentation',
    title: 'This observer is validated and removed from generated code.',
  },
  unsupported: {
    label: 'Not supported',
    title: 'This block cannot be lowered to this target.',
  },
};
//=============================================================================
function createStatusRow(
  capability: string,
  label: string,
  status: string,
  statusLabel: string,
  title: string,
  openHelp: () => void
): HTMLElement {
  const row = document.createElement('div');
  row.className = 'inspector-capability-row';
  row.dataset.capability = capability;
  row.dataset.status = status;
  row.title = title;

  const name = document.createElement('span');
  name.className = 'inspector-capability-name';
  name.dataset.i18n = label;
  name.textContent = label;

  const value = document.createElement('button');
  value.type = 'button';
  value.className = 'inspector-capability-value';
  value.title = t('Open related help');
  value.setAttribute('aria-label', t('Open related help: {label}', { label }));
  value.addEventListener('click', openHelp);
  const indicator = document.createElement('span');
  indicator.className = 'inspector-capability-indicator';
  indicator.setAttribute('aria-hidden', 'true');
  value.appendChild(indicator);

  const copy = document.createElement('span');
  copy.dataset.i18n = statusLabel;
  copy.textContent = statusLabel;
  value.appendChild(copy);
  row.append(name, value);
  return row;
}
//=============================================================================
export function createInspectorCapabilities(block: Block): HTMLDetailsElement {
  const capabilities = getBlockCapabilities(block);
  const section = document.createElement('details');
  section.className = 'inspector-disclosure inspector-capabilities';
  section.dataset.inspectorSection = 'capabilities';
  section.open = readInspectorSectionOpen('capabilities', true);
  section.addEventListener('toggle', () => writeInspectorSectionOpen('capabilities', section.open));
  section.setAttribute('aria-label', 'Block capabilities');

  const heading = document.createElement('summary');
  heading.className = 'inspector-section-heading inspector-capabilities-heading';
  heading.dataset.i18n = 'Capabilities';
  heading.textContent = 'Capabilities';
  section.appendChild(heading);

  const rows = document.createElement('div');
  rows.className = 'inspector-capability-rows';
  const cCopy = CODEGEN_COPY[capabilities.c];
  const rustCopy = CODEGEN_COPY[capabilities.rust];
  const openBlockCodegenHelp = () => void openBlockHelp(block);
  rows.append(
    createStatusRow(
      'c',
      'C code generation',
      capabilities.c,
      cCopy.label,
      cCopy.title,
      openBlockCodegenHelp
    ),
    createStatusRow(
      'rust',
      'Rust code generation',
      capabilities.rust,
      rustCopy.label,
      rustCopy.title,
      openBlockCodegenHelp
    )
  );
  if (capabilities.modelicaRequired) {
    rows.appendChild(
      createStatusRow(
        'modelica',
        'OpenModelica',
        'required',
        'Required',
        'OpenModelica is required to compile this block for simulation.',
        () => void openHelpTopic('modelica')
      )
    );
  }
  section.appendChild(rows);
  return section;
}
//=============================================================================
