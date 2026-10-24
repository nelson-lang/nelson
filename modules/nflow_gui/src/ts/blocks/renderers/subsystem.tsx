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
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import * as FieldNames from '../../FieldNames.js';
import { registerRenderer } from '../inspector-registry.js';
import type { FieldDef } from '../inspector-utils.js';
import { renderGenericFieldsPanel } from '../components/inspector-fields.js';
//=============================================================================
const SUBSYSTEM_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kName,
    label: 'Name',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
    coerce: (el) => (el as HTMLInputElement).value.trim(),
  },
  {
    key: 'description',
    label: 'Description',
    inputType: 'textarea',
    event: 'input',
    callUpdate: true,
    attrs: { rows: '2' },
  },
  {
    key: 'author',
    label: 'Author',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
  },
  {
    key: FieldNames.kMasked,
    label: 'Masked',
    inputType: 'checkbox',
    event: 'change',
    callUpdate: true,
  },
];
//=============================================================================
function detectHaveCToolchain(): boolean {
  try {
    let v: string | null = null;
    try {
      v = sessionStorage.getItem('nf-have-c-toolchain');
    } catch (_e) {
      v = null;
    }
    if (!v) {
      try {
        v = localStorage.getItem('nf-have-c-toolchain');
      } catch (_e) {
        v = null;
      }
    }
    if (v) return v === '1' || v === 'true';
  } catch (_e) {}
  try {
    const s = sessionStorage.getItem('nf-bootstrap');
    if (s) {
      try {
        const b = JSON.parse(s);
        if (b && typeof b.haveCToolChain === 'boolean') return b.haveCToolChain;
        if (b && (b.haveCToolChain === '1' || b.haveCToolChain === '0')) {
          return String(b.haveCToolChain) === '1';
        }
      } catch (_e) {}
    }
  } catch (_e) {}
  try {
    const bs = (window as any).__nf_bootstrap;
    if (bs && typeof bs === 'object') {
      if (typeof bs.haveCToolChain === 'boolean') return bs.haveCToolChain;
      if (bs.haveCToolChain === '1' || bs.haveCToolChain === '0') {
        return String(bs.haveCToolChain) === '1';
      }
    }
  } catch (_e) {}
  return false;
}
//=============================================================================
function SubsystemActions({
  block,
  inputCount,
  outputCount,
}: {
  block: Block;
  inputCount: number;
  outputCount: number;
}): JSX.Element {
  const isMasked = block.params?.masked === true;
  const hasCToolchain = detectHaveCToolchain();
  const forEach = (block.params?.forEach as { numIterations?: number; partition?: number[] }) ?? {};
  const feIterations =
    typeof forEach.numIterations === 'number' && forEach.numIterations > 1
      ? forEach.numIterations
      : 1;
  const fePartition = Array.isArray(forEach.partition) ? forEach.partition.join(', ') : '';
  const controlPort =
    (block.params?.controlPort as {
      kind?: string;
      IterationSource?: string;
      IterationLimit?: number;
      MaxIterations?: number;
    }) ?? {};
  const iterMode =
    controlPort.kind === 'forIterator'
      ? 'for'
      : controlPort.kind === 'whileIterator'
      ? 'while'
      : 'none';
  const iterSource = controlPort.IterationSource === 'external' ? 'external' : 'internal';
  const iterLimit = typeof controlPort.IterationLimit === 'number' ? controlPort.IterationLimit : 1;
  const iterMax = typeof controlPort.MaxIterations === 'number' ? controlPort.MaxIterations : 100;
  const maskParameters =
    block.params?.maskParameters && typeof block.params.maskParameters === 'object'
      ? (block.params.maskParameters as Record<string, unknown>)
      : {};
  const maskEntries = Object.keys(maskParameters).map((name) => ({
    name,
    value: maskParameters[name],
  }));

  return (
    <>
      <div className="param">Inputs: {inputCount}</div>
      <div className="param">Outputs: {outputCount}</div>
      <fieldset className="param-group">
        <legend data-i18n="Mask parameters">Mask parameters</legend>
        <div data-inspector-mask-rows>
          {maskEntries.map((entry, idx) => (
            <div className="param param-inline mask-param-row" key={idx}>
              <input
                type="text"
                className="input mask-param-name"
                defaultValue={entry.name}
                placeholder="name"
              />
              <input
                type="number"
                step="any"
                className="input mask-param-value"
                defaultValue={String(entry.value)}
                placeholder="value"
              />
              <button type="button" className="secondary mask-param-remove" data-i18n="Remove">
                Remove
              </button>
            </div>
          ))}
        </div>
        <button
          type="button"
          className="secondary"
          data-inspector-mask-add
          data-i18n="Add parameter"
        >
          Add parameter
        </button>
      </fieldset>
      {!isMasked && (
        <>
          <fieldset className="param-group">
            <legend data-i18n="For-Each">For-Each</legend>
            <div className="param param-inline">
              <span className="param-name" data-i18n="Iterations">
                Iterations
              </span>
              <input
                type="number"
                min="1"
                step="1"
                className="input"
                data-inspector-foreach-iterations
                defaultValue={String(feIterations)}
                title="Run the body N times per step (1 = ordinary subsystem)."
              />
            </div>
            <div className="param param-inline">
              <span className="param-name" data-i18n="Partition input ports">
                Partition input ports
              </span>
              <input
                type="text"
                className="input"
                data-inspector-foreach-partition
                defaultValue={fePartition}
                placeholder="e.g. 0, 2"
                title="0-based input port indices sliced across iterations; others are broadcast."
              />
            </div>
          </fieldset>
          <fieldset className="param-group">
            <legend data-i18n="Iteration">Iteration</legend>
            <div className="param param-inline">
              <span className="param-name" data-i18n="Mode">
                Mode
              </span>
              <select
                data-inspector-iterator-mode
                className="input"
                defaultValue={iterMode}
                title="Run the body several times sequentially within one step."
              >
                <option value="none" data-i18n="None">
                  None
                </option>
                <option value="for" data-i18n="For iterator">
                  For iterator
                </option>
                <option value="while" data-i18n="While iterator">
                  While iterator
                </option>
              </select>
            </div>
            <div className="param param-inline" data-inspector-iterator-source-row>
              <span className="param-name" data-i18n="Iteration source">
                Iteration source
              </span>
              <select
                data-inspector-iterator-source
                className="input"
                defaultValue={iterSource}
                title="Internal: fixed count. External: count read from the last input port."
              >
                <option value="internal" data-i18n="Internal (limit)">
                  Internal (limit)
                </option>
                <option value="external" data-i18n="External (input port)">
                  External (input port)
                </option>
              </select>
            </div>
            <div className="param param-inline" data-inspector-iterator-limit-row>
              <span className="param-name" data-i18n="Iteration limit">
                Iteration limit
              </span>
              <input
                type="number"
                min="0"
                step="1"
                className="input"
                data-inspector-iterator-limit
                defaultValue={String(iterLimit)}
                title="Number of times the body runs per step (For iterator, internal source)."
              />
            </div>
            <div className="param param-inline" data-inspector-iterator-max-row>
              <span className="param-name" data-i18n="Max iterations">
                Max iterations
              </span>
              <input
                type="number"
                min="1"
                step="1"
                className="input"
                data-inspector-iterator-max
                defaultValue={String(iterMax)}
                title="Safety cap: the While loop stops after this many iterations."
              />
            </div>
          </fieldset>
          <button
            type="button"
            className="secondary"
            data-action="open-subsystem"
            data-i18n="Enter Subsystem"
          >
            Enter Subsystem
          </button>
          <button
            type="button"
            className="secondary"
            data-action="save-subsystem"
            data-i18n="Save Subsystem"
          >
            Save Subsystem
          </button>
          <button
            type="button"
            className="secondary"
            data-action="generate-block-code"
            data-i18n="Generate Code"
          >
            Generate Code
          </button>
          <div className="param param-inline">
            <label className="checkbox-label">
              <input
                type="checkbox"
                data-inspector-codegen-build
                disabled={!hasCToolchain}
                title={hasCToolchain ? undefined : 'C toolchain not available'}
              />
              <span data-i18n="Build after generation">Build after generation</span>
            </label>
          </div>
          <div className="param param-inline">
            <span className="param-name" data-i18n="Configuration">
              Configuration
            </span>
            <select
              data-inspector-codegen-config
              className="input"
              disabled={!hasCToolchain}
              defaultValue="Debug"
            >
              <option value="Debug" data-i18n="Debug">
                Debug
              </option>
              <option value="Release" data-i18n="Release">
                Release
              </option>
            </select>
          </div>
          <pre
            className="debug-log"
            data-inspector-codegen-output
            data-i18n="Build output will appear here."
          >
            Build output will appear here.
          </pre>
        </>
      )}
    </>
  );
}
//=============================================================================
function updateMaskedIndicator(block: Block): void {
  try {
    const g = (block as any).group as Element | null;
    if (!g) return;
    const maskEl = g.querySelector('g.masked-indicator');
    if (maskEl) {
      maskEl.setAttribute('data-visible', block.params?.masked === true ? 'true' : 'false');
    }
  } catch (_e) {}
}
//=============================================================================
function bindSubsystemActions(block: Block, deps: BlockPanelDeps): void {
  const inspectorEl = deps.inspectorBody;
  if (!inspectorEl) return;

  const maskedInput = inspectorEl.querySelector<HTMLInputElement>(
    `input[data-edit="${FieldNames.kMasked}"]`
  );
  maskedInput?.addEventListener('change', () => {
    updateMaskedIndicator(block);
    deps.renderInspector(block);
  });

  // For-each: iterations + partitioned input ports -> params.forEach. Removing
  // the marker (iterations <= 1) reverts the block to an ordinary subsystem.
  const feIterInput = inspectorEl.querySelector<HTMLInputElement>(
    '[data-inspector-foreach-iterations]'
  );
  const fePartInput = inspectorEl.querySelector<HTMLInputElement>(
    '[data-inspector-foreach-partition]'
  );
  const applyForEach = (): void => {
    if (!block.params) block.params = {} as Record<string, unknown>;
    const n = Math.max(1, Math.floor(Number(feIterInput?.value) || 1));
    const partition = (fePartInput?.value || '')
      .split(',')
      .map((s) => parseInt(s.trim(), 10))
      .filter((x) => Number.isFinite(x) && x >= 0);
    if (n > 1) {
      (block.params as Record<string, unknown>).forEach = { numIterations: n, partition };
    } else {
      delete (block.params as Record<string, unknown>).forEach;
    }
    deps.callUpdate(block);
  };
  feIterInput?.addEventListener('input', applyForEach);
  fePartInput?.addEventListener('input', applyForEach);

  // Iterator: For / While iterator mode -> params.controlPort. For iterator runs
  // the body a fixed number of times (IterationLimit) or a count read from the
  // last input port (IterationSource=external); While iterator loops until an
  // inner iteratorCondition block reads zero, bounded by MaxIterations. Choosing
  // "None" only clears an iterator controlPort, leaving any other kind (enable,
  // trigger, ...) untouched.
  const iterModeSel = inspectorEl.querySelector<HTMLSelectElement>(
    '[data-inspector-iterator-mode]'
  );
  const iterSourceSel = inspectorEl.querySelector<HTMLSelectElement>(
    '[data-inspector-iterator-source]'
  );
  const iterLimitInput = inspectorEl.querySelector<HTMLInputElement>(
    '[data-inspector-iterator-limit]'
  );
  const iterMaxInput = inspectorEl.querySelector<HTMLInputElement>('[data-inspector-iterator-max]');
  const iterSourceRow = inspectorEl.querySelector<HTMLElement>(
    '[data-inspector-iterator-source-row]'
  );
  const iterLimitRow = inspectorEl.querySelector<HTMLElement>(
    '[data-inspector-iterator-limit-row]'
  );
  const iterMaxRow = inspectorEl.querySelector<HTMLElement>('[data-inspector-iterator-max-row]');
  const updateIterVisibility = (): void => {
    const mode = iterModeSel?.value ?? 'none';
    const src = iterSourceSel?.value ?? 'internal';
    if (iterSourceRow) iterSourceRow.style.display = mode === 'for' ? '' : 'none';
    if (iterLimitRow)
      iterLimitRow.style.display = mode === 'for' && src === 'internal' ? '' : 'none';
    if (iterMaxRow) iterMaxRow.style.display = mode === 'while' ? '' : 'none';
  };
  const applyIterator = (): void => {
    if (!block.params) block.params = {} as Record<string, unknown>;
    const params = block.params as Record<string, unknown>;
    const mode = iterModeSel?.value ?? 'none';
    if (mode === 'for') {
      const src = iterSourceSel?.value === 'external' ? 'external' : 'internal';
      const cp: Record<string, unknown> = { kind: 'forIterator', IterationSource: src };
      if (src === 'internal') {
        cp.IterationLimit = Math.max(0, Math.floor(Number(iterLimitInput?.value) || 1));
      }
      params.controlPort = cp;
    } else if (mode === 'while') {
      params.controlPort = {
        kind: 'whileIterator',
        MaxIterations: Math.max(1, Math.floor(Number(iterMaxInput?.value) || 100)),
      };
    } else {
      const cur = params.controlPort as { kind?: string } | undefined;
      if (cur && (cur.kind === 'forIterator' || cur.kind === 'whileIterator')) {
        delete params.controlPort;
      }
    }
    updateIterVisibility();
    deps.callUpdate(block);
  };
  iterModeSel?.addEventListener('change', applyIterator);
  iterSourceSel?.addEventListener('change', applyIterator);
  iterLimitInput?.addEventListener('input', applyIterator);
  iterMaxInput?.addEventListener('input', applyIterator);
  updateIterVisibility();

  // Mask parameters: a dynamic list of {name, value} rows -> params.maskParameters
  // (name -> numeric value). Each masked / library subsystem instance resolves
  // its inner block params against these. An empty list removes the marker.
  const maskRows = inspectorEl.querySelector<HTMLElement>('[data-inspector-mask-rows]');
  const maskAdd = inspectorEl.querySelector<HTMLButtonElement>('[data-inspector-mask-add]');
  const rebuildMask = (): void => {
    if (!block.params) block.params = {} as Record<string, unknown>;
    const rows = maskRows?.querySelectorAll<HTMLElement>('.mask-param-row') ?? [];
    const mp: Record<string, number> = {};
    rows.forEach((row) => {
      const name = row.querySelector<HTMLInputElement>('.mask-param-name')?.value.trim();
      const valStr = row.querySelector<HTMLInputElement>('.mask-param-value')?.value;
      if (name) {
        const v = Number(valStr);
        mp[name] = Number.isFinite(v) ? v : 0;
      }
    });
    if (Object.keys(mp).length > 0) {
      (block.params as Record<string, unknown>).maskParameters = mp;
    } else {
      delete (block.params as Record<string, unknown>).maskParameters;
    }
    deps.callUpdate(block);
  };
  const bindMaskRow = (row: HTMLElement): void => {
    row
      .querySelectorAll<HTMLInputElement>('input')
      .forEach((inp) => inp.addEventListener('input', rebuildMask));
    row.querySelector<HTMLButtonElement>('.mask-param-remove')?.addEventListener('click', () => {
      row.remove();
      rebuildMask();
    });
  };
  maskRows?.querySelectorAll<HTMLElement>('.mask-param-row').forEach(bindMaskRow);
  maskAdd?.addEventListener('click', () => {
    if (!maskRows) return;
    const row = document.createElement('div');
    row.className = 'param param-inline mask-param-row';
    const nameInput = document.createElement('input');
    nameInput.type = 'text';
    nameInput.className = 'input mask-param-name';
    nameInput.placeholder = 'name';
    const valueInput = document.createElement('input');
    valueInput.type = 'number';
    valueInput.step = 'any';
    valueInput.className = 'input mask-param-value';
    valueInput.placeholder = 'value';
    const removeBtn = document.createElement('button');
    removeBtn.type = 'button';
    removeBtn.className = 'secondary mask-param-remove';
    removeBtn.textContent = 'Remove';
    row.append(nameInput, valueInput, removeBtn);
    maskRows.appendChild(row);
    bindMaskRow(row);
  });

  inspectorEl
    .querySelector<HTMLButtonElement>("button[data-action='open-subsystem']")
    ?.addEventListener('click', () => {
      deps.onOpenSubsystem?.(block);
    });

  inspectorEl
    .querySelector<HTMLButtonElement>("button[data-action='save-subsystem']")
    ?.addEventListener('click', () => {
      deps.onSaveSubsystem?.(block);
    });

  inspectorEl
    .querySelector<HTMLButtonElement>("button[data-action='generate-block-code']")
    ?.addEventListener('click', () => {
      const buildChk = inspectorEl.querySelector<HTMLInputElement>(
        '[data-inspector-codegen-build]'
      );
      const buildCfgSel = inspectorEl.querySelector<HTMLSelectElement>(
        '[data-inspector-codegen-config]'
      );
      const logPre = inspectorEl.querySelector<HTMLElement>('[data-inspector-codegen-output]');
      (block as any)._codegenBuild = buildChk?.checked ?? false;
      (block as any)._codegenBuildConfig = buildCfgSel?.value ?? 'Debug';
      (block as any)._codegenLogEl = logPre ?? null;
      deps.onGenerateBlockCode?.(block);
    });
}
//=============================================================================
export function renderSubsystem(block: Block, deps: BlockPanelDeps): boolean {
  if (block.type !== FieldNames.kSubsystem) return false;

  const inputCount = Array.isArray(block.params?.externalInputs)
    ? block.params.externalInputs.length
    : 0;
  const outputCount = Array.isArray(block.params?.externalOutputs)
    ? block.params.externalOutputs.length
    : 0;
  const values: Record<string, unknown> = {
    [FieldNames.kName]: block.params?.name || 'Subsystem',
    description: block.params?.description || '',
    author: block.params?.author || '',
    [FieldNames.kMasked]: block.params?.masked === true,
  };

  renderGenericFieldsPanel(
    deps.inspectorBody,
    block,
    deps,
    SUBSYSTEM_FIELDS,
    values,
    <SubsystemActions block={block} inputCount={inputCount} outputCount={outputCount} />
  );
  bindSubsystemActions(block, deps);
  return true;
}
//=============================================================================
try {
  registerRenderer(renderSubsystem);
} catch (_e) {}
//=============================================================================
