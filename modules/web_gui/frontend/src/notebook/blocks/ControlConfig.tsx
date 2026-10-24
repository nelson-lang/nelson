//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
interface ControlConfigProps {
  block: { props: Record<string, unknown> };
  editor: { updateBlock: (block: unknown, update: unknown) => void };
  /** show the slider-only "Run on" option */
  slider?: boolean;
  /** show the drop-down-only "Items from variable" option */
  dropdown?: boolean;
}
//=============================================================================
/** The Live Editor "Configure Control" affordance: a per-control Run setting. */
export function ControlConfig({ block, editor, slider, dropdown }: ControlConfigProps) {
  const runMode = String(block.props.runMode ?? 'all');
  const runOn = String(block.props.runOn ?? 'change');
  const optionsVar = String(block.props.optionsVar ?? '');
  return (
    <details className="nls-cfg" contentEditable={false}>
      <summary className="nls-cfg-btn" title="Configure control">
        &#9881;
      </summary>
      <div className="nls-cfg-pop" onKeyDown={(e) => e.stopPropagation()}>
        <label className="nls-cfg-row">
          <span>Run</span>
          <select
            value={runMode}
            onChange={(e) => editor.updateBlock(block, { props: { runMode: e.target.value } })}
          >
            <option value="all">All sections</option>
            <option value="section">Current section</option>
            <option value="reactive">Dependent cells (reactive)</option>
            <option value="nothing">Nothing</option>
          </select>
        </label>
        {slider && (
          <label className="nls-cfg-row">
            <span>Run on</span>
            <select
              value={runOn}
              onChange={(e) => editor.updateBlock(block, { props: { runOn: e.target.value } })}
            >
              <option value="change">While moving</option>
              <option value="release">On release</option>
            </select>
          </label>
        )}
        {dropdown && (
          <label className="nls-cfg-row">
            <span>Items from</span>
            <input
              className="nls-cfg-input"
              defaultValue={optionsVar}
              placeholder="variable"
              onKeyDown={(e) => e.stopPropagation()}
              onBlur={(e) => editor.updateBlock(block, { props: { optionsVar: e.target.value } })}
            />
          </label>
        )}
      </div>
    </details>
  );
}
//=============================================================================
