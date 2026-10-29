//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../../FieldNames.js';
import type { Block } from '../inspector.js';
import { renderInspectorReact } from '../inspector-react-root.js';
import {
  asDisplayValue,
  asDisplayValueWithFormatter,
  formatSignificant,
} from '../inspector-utils.js';
//=============================================================================
type SignalParams = {
  color?: string;
  dash?: string;
  label?: string;
  strokeWidth?: number;
};
//=============================================================================
const LEGEND_COLORS = ['#f5e642', '#ff49c9', '#39d7ff'];
function paramValue(block: Block, key: string): unknown {
  return block.params?.[key];
}
//=============================================================================
function triggerValue(block: Block, key: string): unknown {
  const trigger = block.params?.trigger as Record<string, unknown> | undefined;
  return trigger?.[key];
}
//=============================================================================
function ScopeTimeAxis({ block, runtime }: { block: Block; runtime: number | null }): JSX.Element {
  const limits = block.computedLimits || {};
  return (
    <fieldset className="param-group">
      <legend>Time Axis</legend>
      <label className="param">
        t min (s)
        <input
          type="text"
          data-edit={FieldNames.kTMin}
          defaultValue={asDisplayValue(paramValue(block, FieldNames.kTMin), limits.tMin ?? 0)}
        />
      </label>
      <label className="param">
        t max (s)
        <input
          type="text"
          data-edit={FieldNames.kTMax}
          defaultValue={asDisplayValue(
            paramValue(block, FieldNames.kTMax),
            limits.tMax ?? runtime ?? 1
          )}
        />
      </label>
      <label className="param important-param">
        Time window (s)
        <input
          type="text"
          data-edit={FieldNames.kTimeWindow}
          defaultValue={String(paramValue(block, FieldNames.kTimeWindow) ?? '')}
          placeholder="all (fixed)"
        />
      </label>
    </fieldset>
  );
}
//=============================================================================
function ScopeXAxis({ block }: { block: Block }): JSX.Element {
  const limits = block.computedLimits || {};
  return (
    <fieldset className="param-group">
      <legend>X Axis</legend>
      <label className="param">
        x min
        <input
          type="text"
          data-edit={FieldNames.kXMin}
          defaultValue={asDisplayValue(paramValue(block, FieldNames.kXMin), limits.xMin ?? -1.2)}
        />
      </label>
      <label className="param">
        x max
        <input
          type="text"
          data-edit={FieldNames.kXMax}
          defaultValue={asDisplayValue(paramValue(block, FieldNames.kXMax), limits.xMax ?? 1.2)}
        />
      </label>
    </fieldset>
  );
}
//=============================================================================
function ScopeYAxis({ block }: { block: Block }): JSX.Element {
  const limits = block.computedLimits || {};
  return (
    <fieldset className="param-group">
      <legend>Y Axis</legend>
      <label className="param">
        y min
        <input
          type="text"
          data-edit={FieldNames.kYMin}
          defaultValue={asDisplayValueWithFormatter(
            paramValue(block, FieldNames.kYMin),
            limits.yMin ?? -1.2,
            (value) => formatSignificant(Number(value), 2)
          )}
        />
      </label>
      <label className="param">
        y max
        <input
          type="text"
          data-edit={FieldNames.kYMax}
          defaultValue={asDisplayValueWithFormatter(
            paramValue(block, FieldNames.kYMax),
            limits.yMax ?? 1.2,
            (value) => formatSignificant(Number(value), 2)
          )}
        />
      </label>
    </fieldset>
  );
}
//=============================================================================
function ScopeZAxis({ block }: { block: Block }): JSX.Element {
  const limits = block.computedLimits || {};
  return (
    <fieldset className="param-group">
      <legend>Z Axis</legend>
      <label className="param">
        z min
        <input
          type="text"
          data-edit={FieldNames.kZMin}
          defaultValue={asDisplayValueWithFormatter(
            paramValue(block, FieldNames.kZMin),
            limits.zMin ?? -1.2,
            (value) => formatSignificant(Number(value), 2)
          )}
        />
      </label>
      <label className="param">
        z max
        <input
          type="text"
          data-edit={FieldNames.kZMax}
          defaultValue={asDisplayValueWithFormatter(
            paramValue(block, FieldNames.kZMax),
            limits.zMax ?? 1.2,
            (value) => formatSignificant(Number(value), 2)
          )}
        />
      </label>
    </fieldset>
  );
}
//=============================================================================
function ScopeView({ block }: { block: Block }): JSX.Element {
  return (
    <fieldset className="param-group">
      <legend>View</legend>
      <label className="param">
        Rotation X (deg)
        <input
          type="number"
          data-edit={FieldNames.kRotationX}
          defaultValue={String(block.params.RotationX ?? 30)}
          min="-180"
          max="180"
          step="5"
        />
      </label>
      <label className="param">
        Rotation Y (deg)
        <input
          type="number"
          data-edit={FieldNames.kRotationY}
          defaultValue={String(block.params.RotationY ?? 45)}
          min="-180"
          max="180"
          step="5"
        />
      </label>
      <label className="param">
        Width
        <input
          type="number"
          data-edit={FieldNames.kWidth}
          defaultValue={String(block.params.width ?? block.width ?? '')}
          min="160"
          step="10"
        />
      </label>
      <label className="param">
        Height
        <input
          type="number"
          data-edit={FieldNames.kHeight}
          defaultValue={String(block.params.height ?? block.height ?? '')}
          min="120"
          step="10"
        />
      </label>
    </fieldset>
  );
}
//=============================================================================
function ScopeDisplay({ block }: { block: Block }): JSX.Element {
  return (
    <fieldset className="param-group">
      <legend>Display</legend>
      <label className="param">
        Width
        <input
          type="number"
          data-edit={FieldNames.kWidth}
          defaultValue={String(block.params.width ?? block.width ?? '')}
          min="160"
          step="10"
        />
      </label>
      <label className="param">
        Height
        <input
          type="number"
          data-edit={FieldNames.kHeight}
          defaultValue={String(block.params.height ?? block.height ?? '')}
          min="120"
          step="10"
        />
      </label>
      <div className="param param-inline">
        <span className="param-name">Show tick labels</span>
        <input
          type="checkbox"
          data-edit={FieldNames.kShowTickLabels}
          defaultChecked={block.params.ShowTickLabels === true}
        />
      </div>
    </fieldset>
  );
}
//=============================================================================
function ScopeAppearance({ block }: { block: Block }): JSX.Element {
  const theme = block.params.theme ?? 'light';
  const backgroundColor =
    block.params.backgroundColor ?? (block.params.theme === 'light' ? '#ffffff' : '#101010');
  return (
    <fieldset className="param-group">
      <legend>Appearance</legend>
      <label className="param">
        Theme
        <select data-edit={FieldNames.kTheme} defaultValue={String(theme)}>
          <option value="dark">Dark</option>
          <option value="light">Light</option>
        </select>
      </label>
      <label className="param">
        Background
        <input type="color" data-edit="backgroundColor" defaultValue={String(backgroundColor)} />
      </label>
      <div className="param param-inline">
        <span className="param-name">Show grid</span>
        <input
          type="checkbox"
          data-edit={FieldNames.kShowGrid}
          defaultChecked={block.params.showGrid !== false}
        />
      </div>
      <div className="param param-inline">
        <span className="param-name">Show legend</span>
        <input
          type="checkbox"
          data-edit={FieldNames.kShowLegend}
          defaultChecked={block.params.showLegend !== false}
        />
      </div>
      <div className="param param-inline">
        <span className="param-name">Show title</span>
        <input
          type="checkbox"
          data-edit={FieldNames.kShowTitle}
          defaultChecked={block.params.showTitle !== false}
        />
      </div>
    </fieldset>
  );
}
//=============================================================================
function ScopeAdvanced({ block }: { block: Block }): JSX.Element {
  return (
    <fieldset className="param-group">
      <legend>Advanced</legend>
      <label className="param">
        Y scale
        <select
          data-edit={FieldNames.kYScale}
          defaultValue={String(block.params.yScale ?? 'linear')}
        >
          <option value="linear">Linear</option>
          <option value="log">Logarithmic</option>
        </select>
      </label>
      <div className="param param-inline">
        <span className="param-name">Multi-Y axis</span>
        <input
          type="checkbox"
          data-edit={FieldNames.kMultiYAxis}
          defaultChecked={block.params.multiYAxis === true}
        />
      </div>
      <fieldset className="param-group">
        <legend>Trigger</legend>
        <label className="param">
          Signal
          <input
            type="number"
            min="1"
            step="1"
            data-edit="triggerSource"
            defaultValue={String(
              triggerValue(block, 'source') != null ? Number(triggerValue(block, 'source')) + 1 : 1
            )}
          />
        </label>
        <label className="param">
          Level
          <input
            type="text"
            data-edit="triggerLevel"
            defaultValue={String(triggerValue(block, 'level') ?? '')}
            placeholder="off"
          />
        </label>
        <label className="param">
          Edge
          <select
            data-edit="triggerEdge"
            defaultValue={String(triggerValue(block, 'edge') ?? 'rising')}
          >
            <option value="rising">Rising ↑</option>
            <option value="falling">Falling ↓</option>
          </select>
        </label>
      </fieldset>
    </fieldset>
  );
}
//=============================================================================
function ScopeLabels({ block }: { block: Block }): JSX.Element {
  return (
    <fieldset className="param-group">
      <legend>Labels</legend>
      <label className="param important-param">
        X label
        <input
          type="text"
          data-edit={FieldNames.kXLabel}
          defaultValue={String(block.params.xLabel ?? '')}
        />
      </label>
      <label className="param important-param">
        Y label
        <input
          type="text"
          data-edit={FieldNames.kYLabel}
          defaultValue={String(block.params.yLabel ?? '')}
        />
      </label>
    </fieldset>
  );
}
//=============================================================================
function Scope3DLabels({ block }: { block: Block }): JSX.Element {
  return (
    <fieldset className="param-group">
      <legend>Labels</legend>
      <label className="param important-param">
        X label
        <input
          type="text"
          data-edit={FieldNames.kXLabel}
          defaultValue={String(block.params.xLabel ?? '')}
        />
      </label>
      <label className="param important-param">
        Y label
        <input
          type="text"
          data-edit={FieldNames.kYLabel}
          defaultValue={String(block.params.yLabel ?? '')}
        />
      </label>
      <label className="param important-param">
        Z label
        <input type="text" data-edit="zLabel" defaultValue={String(block.params.zLabel ?? '')} />
      </label>
    </fieldset>
  );
}
//=============================================================================
function ScopeSignalFieldset({
  signal,
  index,
}: {
  signal: SignalParams;
  index: number;
}): JSX.Element {
  const dash = signal.dash ?? 'solid';
  return (
    <fieldset className="param-group">
      <legend>{`Signal ${index + 1}`}</legend>
      <label className="param">
        Name
        <input
          type="text"
          data-signal-edit="label"
          data-signal-index={index}
          defaultValue={signal.label ?? `y${index + 1}`}
        />
      </label>
      <label className="param">
        Color
        <input
          type="color"
          data-signal-edit="color"
          data-signal-index={index}
          defaultValue={signal.color || LEGEND_COLORS[index] || '#ffffff'}
        />
      </label>
      <label className="param">
        Line width
        <input
          type="number"
          min="0.5"
          max="10"
          step="0.5"
          data-signal-edit="strokeWidth"
          data-signal-index={index}
          defaultValue={String(signal.strokeWidth ?? 2)}
        />
      </label>
      <label className="param">
        Line style
        <select data-signal-edit="dash" data-signal-index={index} defaultValue={dash}>
          <option value="solid">Solid</option>
          <option value="dashed">Dashed</option>
          <option value="dotted">Dotted</option>
        </select>
      </label>
    </fieldset>
  );
}
//=============================================================================
function ScopeSignals({ block }: { block: Block }): JSX.Element {
  const signals = block.params.signals as SignalParams[] | undefined;
  return (
    <fieldset className="param-group">
      <legend>
        <span className="collapse-chevron" aria-hidden="true" />
        Signals
      </legend>
      <div className="signals-wrapper">
        {Array.from({ length: Number(block.inputs ?? 1) }, (_, index) => (
          <ScopeSignalFieldset key={index} signal={signals?.[index] ?? {}} index={index} />
        ))}
      </div>
    </fieldset>
  );
}
//=============================================================================
function ScopeInspectorPanel({
  block,
  runtime,
}: {
  block: Block;
  runtime: number | null;
}): JSX.Element {
  return (
    <>
      <ScopeTimeAxis block={block} runtime={runtime} />
      <ScopeYAxis block={block} />
      <ScopeDisplay block={block} />
      <ScopeAppearance block={block} />
      <ScopeAdvanced block={block} />
      <ScopeLabels block={block} />
      <ScopeSignals block={block} />
    </>
  );
}
//=============================================================================
function XyScopeInspectorPanel({ block }: { block: Block }): JSX.Element {
  return (
    <>
      <ScopeXAxis block={block} />
      <ScopeYAxis block={block} />
      <ScopeDisplay block={block} />
      <ScopeAppearance block={block} />
      <ScopeAdvanced block={block} />
      <ScopeLabels block={block} />
      <ScopeSignals block={block} />
    </>
  );
}
//=============================================================================
function XyzScopeInspectorPanel({ block }: { block: Block }): JSX.Element {
  return (
    <>
      <ScopeXAxis block={block} />
      <ScopeYAxis block={block} />
      <ScopeZAxis block={block} />
      <ScopeView block={block} />
      <ScopeDisplay block={block} />
      <ScopeAppearance block={block} />
      <ScopeAdvanced block={block} />
      <Scope3DLabels block={block} />
      <ScopeSignals block={block} />
    </>
  );
}
//=============================================================================
export function renderScopeInspectorPanel(
  inspectorBody: HTMLElement | null,
  block: Block,
  runtime: number | null
): void {
  renderInspectorReact(inspectorBody, <ScopeInspectorPanel block={block} runtime={runtime} />);
}
//=============================================================================
export function renderXyScopeInspectorPanel(inspectorBody: HTMLElement | null, block: Block): void {
  renderInspectorReact(inspectorBody, <XyScopeInspectorPanel block={block} />);
}
//=============================================================================
export function renderXyzScopeInspectorPanel(
  inspectorBody: HTMLElement | null,
  block: Block
): void {
  renderInspectorReact(inspectorBody, <XyzScopeInspectorPanel block={block} />);
}
//=============================================================================
