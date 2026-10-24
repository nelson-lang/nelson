//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import { registerRenderer } from '../inspector-registry.js';
import { renderInspectorReact } from '../inspector-react-root.js';
import { state } from '../../app/state.js';
import { t } from '../../utils/i18n.js';
import dashboardContract from '../../../../../nflow_blocks/libraries/dashboard/contract.json';
//=============================================================================
export type DashboardBinding = {
  kind?: 'signal' | 'parameter' | 'variable';
  blockPath?: string[];
  outputPortIndex?: number;
  frameProcessingMode?: 'sample' | 'frame';
  paramName?: string;
  varName?: string;
  workspace?: string;
  element?: string;
};
//=============================================================================
type ContractProperty = {
  source: 'dialog' | 'configuration';
  type: string;
  default: unknown;
  enum: string[];
  invalidRejected: boolean | null;
};
//=============================================================================
type TargetOption = { block: Block; path: string[]; label: string };
type BindingStatus = { valid: boolean; message: string };
const COMMON_KEYS = new Set(['Binding', 'DashboardValue', '_visible']);
const OBSERVERS = new Set([
  'dashboardScope',
  'dashboardDisplay',
  'dashboardGauge',
  'dashboardHalfGauge',
  'dashboardQuarterGauge',
  'dashboardLinearGauge',
  'dashboardLamp',
  'dashboardMultiStateImage',
]);
//=============================================================================
function isDashboard(block: Block): boolean {
  return String(block.type || '').startsWith('dashboard');
}
//=============================================================================
export function dashboardBindingKinds(type: string): DashboardBinding['kind'][] {
  if (type === 'dashboardCallbackButton') return [];
  if (OBSERVERS.has(type)) return ['signal'];
  return ['parameter', 'variable'];
}
//=============================================================================
function pathText(binding: DashboardBinding): string {
  return Array.isArray(binding.blockPath) ? binding.blockPath.join('/') : '';
}
//=============================================================================
function targetName(block: Block): string {
  return String(block.params?.name || block.params?.label || block.type || block.id || '');
}
//=============================================================================
function nestedBlocks(block: Block): Block[] {
  const subsystem = block.params?.subsystem as { blocks?: Block[] } | undefined;
  return Array.isArray(subsystem?.blocks) ? subsystem.blocks : [];
}
//=============================================================================
export function collectDashboardTargets(blocks: Block[]): TargetOption[] {
  const result: TargetOption[] = [];
  const visit = (items: Block[], prefix: string[], labels: string[]) => {
    for (const block of items) {
      if (isDashboard(block) || block.id == null) continue;
      const path = [...prefix, String(block.id)];
      const nextLabels = [...labels, targetName(block)];
      result.push({ block, path, label: `${nextLabels.join(' / ')} (${path.join('/')})` });
      visit(nestedBlocks(block), path, nextLabels);
    }
  };
  visit(blocks, [], []);
  return result;
}
//=============================================================================
function parseElement(element?: string): number[] | null {
  if (!element) return [];
  const indices = element
    .replace(/[()\s]/g, '')
    .split(',')
    .map(Number);
  return indices.length && indices.every((index) => Number.isInteger(index) && index > 0)
    ? indices
    : null;
}
//=============================================================================
function elementValue(value: unknown, element?: string): unknown {
  const indices = parseElement(element);
  if (indices === null) return undefined;
  let current = value;
  for (const oneBased of indices) {
    if (!Array.isArray(current) || oneBased > current.length) return undefined;
    current = current[oneBased - 1];
  }
  return current;
}
//=============================================================================
function outputCount(block: Block): number | null {
  if (Array.isArray(block.dynamicOutputs)) return block.dynamicOutputs.length;
  if (Array.isArray(block.outputs)) return block.outputs.length;
  if (typeof block.outputs === 'number') return block.outputs;
  return null;
}
//=============================================================================
export function validateDashboardBinding(
  type: string,
  binding: DashboardBinding | null,
  targets: TargetOption[],
  variables: Record<string, unknown>
): BindingStatus {
  const kinds = dashboardBindingKinds(type);
  if (!kinds.length) return { valid: true, message: t('No target required') };
  if (!binding?.kind) return { valid: false, message: t('Not connected') };
  if (!kinds.includes(binding.kind)) return { valid: false, message: t('Unsupported target type') };
  if (binding.kind === 'variable') {
    if (!binding.varName) return { valid: false, message: t('Select a variable') };
    if (!(binding.varName in variables)) return { valid: false, message: t('Variable not found') };
    const value = elementValue(variables[binding.varName], binding.element);
    if (value === undefined) return { valid: false, message: t('Invalid element index') };
    if (typeof value !== 'number')
      return { valid: false, message: t('Target must be numeric and scalar') };
    return { valid: true, message: t('Valid binding') };
  }
  const path = pathText(binding);
  const target = targets.find((candidate) => candidate.path.join('/') === path)?.block;
  if (!target) return { valid: false, message: t('Target block not found') };
  if (binding.kind === 'signal') {
    const port = Number(binding.outputPortIndex);
    if (!Number.isInteger(port) || port < 1)
      return { valid: false, message: t('Invalid output port') };
    const count = outputCount(target);
    if (count !== null && port > count) return { valid: false, message: t('Invalid output port') };
    return { valid: true, message: t('Valid binding') };
  }
  if (!binding.paramName || !(binding.paramName in target.params))
    return { valid: false, message: t('Parameter not found') };
  const value = elementValue(target.params[binding.paramName], binding.element);
  if (value === undefined) return { valid: false, message: t('Invalid element index') };
  if (typeof value !== 'number')
    return { valid: false, message: t('Target must be numeric and scalar') };
  return { valid: true, message: t('Valid binding') };
}
//=============================================================================
function contractProperties(type: string): Record<string, ContractProperty> {
  const entry = dashboardContract.blocks.find((candidate) => candidate.type === type);
  return (entry?.properties || {}) as Record<string, ContractProperty>;
}
//=============================================================================
function PropertyEditor({
  name,
  schema,
  value,
  setValue,
}: {
  name: string;
  schema: ContractProperty | undefined;
  value: unknown;
  setValue: (value: unknown) => void;
}): JSX.Element {
  const enumValues = schema?.enum || (name === 'ButtonType' ? ['Momentary', 'Latch'] : []);
  if (enumValues.length) {
    return (
      <label className="param">
        {name}
        <select
          value={String(value ?? '')}
          onChange={(event) => setValue(event.currentTarget.value)}
        >
          {enumValues.map((option) => (
            <option key={option} value={option}>
              {option}
            </option>
          ))}
        </select>
      </label>
    );
  }
  if (schema?.type === 'boolean' || typeof value === 'boolean') {
    const stringBoolean = typeof value === 'string';
    const checked = stringBoolean ? value === 'on' : Boolean(value);
    return (
      <label className="param param-inline">
        <span className="param-name">{name}</span>
        <input
          type="checkbox"
          checked={checked}
          onChange={(event) =>
            setValue(
              stringBoolean
                ? event.currentTarget.checked
                  ? 'on'
                  : 'off'
                : event.currentTarget.checked
            )
          }
        />
      </label>
    );
  }
  if (Array.isArray(value) || (value && typeof value === 'object')) {
    return (
      <label className="param param-stack">
        <span className="param-name">{name}</span>
        <textarea
          defaultValue={JSON.stringify(value)}
          onBlur={(event) => {
            try {
              setValue(JSON.parse(event.currentTarget.value));
              event.currentTarget.setCustomValidity('');
            } catch {
              event.currentTarget.setCustomValidity(t('Invalid JSON value'));
              event.currentTarget.reportValidity();
            }
          }}
        />
      </label>
    );
  }
  const isNumber = schema?.type === 'number' || typeof value === 'number';
  return (
    <label className="param">
      {name}
      <input
        type={isNumber ? 'number' : 'text'}
        min={
          name === 'Opacity' || name === 'PressDelay' || name === 'RepeatInterval' ? 0 : undefined
        }
        max={name === 'Opacity' ? 1 : undefined}
        value={String(value ?? '')}
        onChange={(event) =>
          setValue(isNumber ? Number(event.currentTarget.value) : event.currentTarget.value)
        }
      />
    </label>
  );
}
//=============================================================================
function DashboardInspector({ block, deps }: { block: Block; deps: BlockPanelDeps }): JSX.Element {
  const type = String(block.type || '');
  const binding = ((block.params?.Binding as DashboardBinding | null) ||
    null) as DashboardBinding | null;
  const kinds = dashboardBindingKinds(type);
  const targets = collectDashboardTargets(Array.from(state.blocks.values()) as Block[]);
  const variables = state.variables || {};
  const variableOptions = Object.keys(variables).sort();
  const selectedTarget = targets.find(
    (candidate) => candidate.path.join('/') === pathText(binding || {})
  );
  const parameterOptions = selectedTarget
    ? Object.keys(selectedTarget.block.params)
        .filter((name) => !name.startsWith('_'))
        .sort()
    : [];
  const status = validateDashboardBinding(type, binding, targets, variables);
  const schemas = contractProperties(type);
  const propertyKeys = Object.keys(schemas).filter((key) => !COMMON_KEYS.has(key));

  const refresh = () => {
    deps.callUpdate(block);
    deps.signalDiagramChanged?.();
    deps.renderInspector(block);
  };
  const setParam = (key: string, value: unknown) => {
    block.params[key] = value;
    refresh();
  };
  const setBinding = (next: DashboardBinding | null) => {
    block.params.Binding = next;
    refresh();
  };

  return (
    <>
      {kinds.length > 0 && (
        <fieldset className="param-group dashboard-binding-editor">
          <legend>{t('Binding')}</legend>
          <label className="param">
            {t('Target type')}
            <select
              value={binding?.kind || ''}
              onChange={(event) => {
                const kind = event.currentTarget.value as DashboardBinding['kind'] | '';
                if (!kind) setBinding(null);
                else if (kind === 'signal')
                  setBinding({
                    kind,
                    blockPath: [],
                    outputPortIndex: 1,
                    frameProcessingMode: 'sample',
                  });
                else if (kind === 'parameter')
                  setBinding({ kind, blockPath: [], paramName: '', element: '' });
                else setBinding({ kind, varName: '', workspace: 'diagram', element: '' });
              }}
            >
              <option value="">{t('Not connected')}</option>
              {kinds.includes('signal') && <option value="signal">{t('Signal')}</option>}
              {kinds.includes('parameter') && <option value="parameter">{t('Parameter')}</option>}
              {kinds.includes('variable') && <option value="variable">{t('Variable')}</option>}
            </select>
          </label>
          {(binding?.kind === 'signal' || binding?.kind === 'parameter') && (
            <label className="param">
              {t('Block')}
              <select
                value={pathText(binding)}
                onChange={(event) =>
                  setBinding({
                    ...binding,
                    blockPath: event.currentTarget.value.split('/').filter(Boolean),
                  })
                }
              >
                <option value="">{t('Select a block')}</option>
                {targets.map((candidate) => (
                  <option key={candidate.path.join('/')} value={candidate.path.join('/')}>
                    {candidate.label}
                  </option>
                ))}
              </select>
            </label>
          )}
          {binding?.kind === 'signal' && (
            <>
              <label className="param">
                {t('Output port')}
                <input
                  type="number"
                  min="1"
                  step="1"
                  value={binding.outputPortIndex || 1}
                  onChange={(event) =>
                    setBinding({
                      ...binding,
                      outputPortIndex: Math.max(1, Number(event.currentTarget.value) || 1),
                    })
                  }
                />
              </label>
              <label className="param">
                {t('Processing')}
                <select
                  value={binding.frameProcessingMode || 'sample'}
                  onChange={(event) =>
                    setBinding({
                      ...binding,
                      frameProcessingMode: event.currentTarget.value as 'sample' | 'frame',
                    })
                  }
                >
                  <option value="sample">{t('Sample')}</option>
                  <option value="frame">{t('Frame')}</option>
                </select>
              </label>
            </>
          )}
          {binding?.kind === 'parameter' && (
            <>
              <label className="param">
                {t('Parameter')}
                <select
                  value={binding.paramName || ''}
                  onChange={(event) =>
                    setBinding({ ...binding, paramName: event.currentTarget.value, element: '' })
                  }
                >
                  <option value="">{t('Select a parameter')}</option>
                  {parameterOptions.map((name) => (
                    <option key={name} value={name}>
                      {name}
                    </option>
                  ))}
                </select>
              </label>
              <label className="param">
                {t('Element')}
                <input
                  type="text"
                  placeholder="(1)"
                  value={binding.element || ''}
                  onChange={(event) =>
                    setBinding({ ...binding, element: event.currentTarget.value })
                  }
                />
              </label>
            </>
          )}
          {binding?.kind === 'variable' && (
            <>
              <label className="param">
                {t('Workspace')}
                <select
                  value={binding.workspace || 'diagram'}
                  onChange={(event) =>
                    setBinding({ ...binding, workspace: event.currentTarget.value })
                  }
                >
                  <option value="diagram">{t('Diagram')}</option>
                  <option value="base">{t('Base')}</option>
                  <option value="global">{t('Global')}</option>
                </select>
              </label>
              <label className="param">
                {t('Variable')}
                <select
                  value={binding.varName || ''}
                  onChange={(event) =>
                    setBinding({ ...binding, varName: event.currentTarget.value, element: '' })
                  }
                >
                  <option value="">{t('Select a variable')}</option>
                  {variableOptions.map((name) => (
                    <option key={name} value={name}>
                      {name}
                    </option>
                  ))}
                </select>
              </label>
              <label className="param">
                {t('Element')}
                <input
                  type="text"
                  placeholder="(1)"
                  value={binding.element || ''}
                  onChange={(event) =>
                    setBinding({ ...binding, element: event.currentTarget.value })
                  }
                />
              </label>
            </>
          )}
          <output className={`dashboard-binding-status ${status.valid ? 'valid' : 'invalid'}`}>
            {status.message}
          </output>
        </fieldset>
      )}
      <fieldset className="param-group dashboard-property-editor">
        <legend>{t('Appearance and behavior')}</legend>
        {propertyKeys.map((key) => (
          <PropertyEditor
            key={key}
            name={key}
            schema={schemas[key]}
            value={block.params[key]}
            setValue={(value) => setParam(key, value)}
          />
        ))}
      </fieldset>
    </>
  );
}
//=============================================================================
export function renderDashboard(block: Block, deps: BlockPanelDeps): boolean {
  if (!isDashboard(block)) return false;
  renderInspectorReact(deps.inspectorBody, <DashboardInspector block={block} deps={deps} />);
  return true;
}
//=============================================================================
registerRenderer(renderDashboard);
//=============================================================================
