//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { renderBlockPanel } from './inspector-block';
import * as FieldNames from '../FieldNames';
//=============================================================================
describe('inspector-block subsystem codegen build checkbox', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    try {
      sessionStorage.clear();
      localStorage.clear();
      try {
        (window as any).__nf_bootstrap = undefined;
      } catch (e) {}
    } catch (e) {}
  });

  function makeDeps() {
    return {
      inspectorBody: document.createElement('div'),
      callUpdate: () => {},
      callResize: () => {},
      renderInspector: () => {},
      signalDiagramChanged: () => {},
      onOpenSubsystem: () => {},
      onSaveSubsystem: () => {},
      onGenerateBlockCode: () => {},
    } as any;
  }

  const makeBlock = () =>
    ({
      type: FieldNames.kSubsystem,
      params: { externalInputs: [], externalOutputs: [], masked: false },
    } as any);

  it('disables build after generation when C toolchain missing', () => {
    const deps = makeDeps();
    const block = makeBlock();
    renderBlockPanel(block, deps);
    const chk = deps.inspectorBody.querySelector<HTMLInputElement>(
      '[data-inspector-codegen-build]'
    );
    expect(chk).toBeTruthy();
    expect(chk!.disabled).toBe(true);
  });

  it('enables build after generation when C toolchain present', () => {
    try {
      sessionStorage.setItem('nf-have-c-toolchain', '1');
    } catch (e) {}
    const deps = makeDeps();
    const block = makeBlock();
    renderBlockPanel(block, deps);
    const chk = deps.inspectorBody.querySelector<HTMLInputElement>(
      '[data-inspector-codegen-build]'
    );
    expect(chk).toBeTruthy();
    expect(chk!.disabled).toBe(false);
  });

  it('wires subsystem action buttons and codegen options', () => {
    try {
      sessionStorage.setItem('nf-have-c-toolchain', '1');
    } catch (e) {}
    const onOpenSubsystem = vi.fn();
    const onSaveSubsystem = vi.fn();
    const onGenerateBlockCode = vi.fn();
    const deps = {
      ...makeDeps(),
      onOpenSubsystem,
      onSaveSubsystem,
      onGenerateBlockCode,
    };
    const block = makeBlock();

    renderBlockPanel(block, deps);

    deps.inspectorBody
      .querySelector<HTMLButtonElement>("button[data-action='open-subsystem']")!
      .click();
    deps.inspectorBody
      .querySelector<HTMLButtonElement>("button[data-action='save-subsystem']")!
      .click();

    const build = deps.inspectorBody.querySelector<HTMLInputElement>(
      '[data-inspector-codegen-build]'
    );
    const config = deps.inspectorBody.querySelector<HTMLSelectElement>(
      '[data-inspector-codegen-config]'
    );
    build!.checked = true;
    config!.value = 'Release';
    deps.inspectorBody
      .querySelector<HTMLButtonElement>("button[data-action='generate-block-code']")!
      .click();

    expect(onOpenSubsystem).toHaveBeenCalledWith(block);
    expect(onSaveSubsystem).toHaveBeenCalledWith(block);
    expect(onGenerateBlockCode).toHaveBeenCalledWith(block);
    expect(block._codegenBuild).toBe(true);
    expect(block._codegenBuildConfig).toBe('Release');
    expect(block._codegenLogEl).toBeInstanceOf(HTMLElement);
  });

  it('renders an accessible block help button', () => {
    const deps = makeDeps();
    renderBlockPanel(makeBlock(), deps);

    const help = deps.inspectorBody.querySelector<HTMLButtonElement>('.inspector-help-button');
    expect(help).toBeTruthy();
    expect(help?.getAttribute('aria-label')).toBe('Block Help');
    expect(help?.title).toBe('Block Help');
  });
});
//=============================================================================
