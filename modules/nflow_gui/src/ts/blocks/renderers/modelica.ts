//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Inspector panel for the Modelica bridge block. The block references a Modelica
// model (inline source or a .mo file) plus an optional model name; at simulation
// time sim() compiles it to an FMU with OpenModelica and runs it through the FMI
// path. This panel edits those parameters and lets the user browse for a .mo.
//=============================================================================
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import { registerRenderer } from '../inspector-registry.js';
import { openFilePicker } from '../../app/file-dialogs.js';
//=============================================================================
export function renderModelica(block: Block, deps: BlockPanelDeps): boolean {
  if (String(block.type) !== 'modelica') {
    return false;
  }
  const body = deps.inspectorBody;
  if (!body) {
    return true;
  }
  body.innerHTML = '';
  const params = block.params;

  const fileRow = document.createElement('div');
  fileRow.className = 'inspector-row';
  const fileLabel = document.createElement('label');
  fileLabel.textContent = 'Modelica file';
  const fileInput = document.createElement('input');
  fileInput.type = 'text';
  fileInput.value = String(params.file ?? '');
  fileInput.placeholder = 'path to a .mo file (or use inline source below)';
  fileInput.addEventListener('input', () => {
    params.file = fileInput.value;
    deps.signalDiagramChanged?.();
  });
  fileRow.appendChild(fileLabel);
  fileRow.appendChild(fileInput);
  body.appendChild(fileRow);

  const browse = document.createElement('button');
  browse.type = 'button';
  browse.className = 'inspector-button';
  browse.textContent = 'Browse .mo…';
  browse.addEventListener('click', async () => {
    const picked = await openFilePicker({
      mode: 'open',
      title: 'Open Modelica model',
      defaultPath: String(params.file || '.'),
      filters: ['mo'],
    });
    if (picked) {
      params.file = picked;
      fileInput.value = picked;
      deps.signalDiagramChanged?.();
    }
  });
  body.appendChild(browse);

  const nameRow = document.createElement('div');
  nameRow.className = 'inspector-row';
  const nameLabel = document.createElement('label');
  nameLabel.textContent = 'Model name';
  const nameInput = document.createElement('input');
  nameInput.type = 'text';
  nameInput.value = String(params.modelName ?? '');
  nameInput.placeholder = 'top-level Modelica class (optional)';
  nameInput.addEventListener('input', () => {
    params.modelName = nameInput.value;
    deps.signalDiagramChanged?.();
  });
  nameRow.appendChild(nameLabel);
  nameRow.appendChild(nameInput);
  body.appendChild(nameRow);

  const srcHeading = document.createElement('div');
  srcHeading.className = 'inspector-subheading';
  srcHeading.textContent = 'Inline source (used when no file is set)';
  body.appendChild(srcHeading);

  const srcArea = document.createElement('textarea');
  srcArea.className = 'inspector-textarea';
  srcArea.rows = 8;
  srcArea.spellcheck = false;
  srcArea.value = String(params.source ?? '');
  srcArea.placeholder = 'model M\n  output Real y(start = 0);\nequation\n  der(y) = 1 - y;\nend M;';
  srcArea.addEventListener('input', () => {
    params.source = srcArea.value;
    deps.signalDiagramChanged?.();
  });
  body.appendChild(srcArea);

  const note = document.createElement('div');
  note.className = 'inspector-note';
  note.textContent =
    'Mark measured variables output (e.g. output Real vC;) so they become ports. ' +
    'Requires OpenModelica; set its path in Settings if it is not auto-detected.';
  body.appendChild(note);

  return true;
}
//=============================================================================
try {
  registerRenderer(renderModelica);
} catch (e) {
  void e;
}
//=============================================================================
