//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let renderAny: any;
let libAny: any;
beforeAll(async () => {
  const createMod = await import('../../../dist/render/create-renderer.js');
  const templatesMod = await import('../../../dist/render/block/templates.js');
  renderAny = { ...createMod, ...templatesMod } as any;
  libAny = await import('../../../dist/app/library-loader.js');
});
//=============================================================================
beforeEach(() => {
  const map = libAny.getLoadedLibraries();
  map.clear();
});
//=============================================================================
test('refreshBlockTemplates picks up fake tf template and createBlock renders math', () => {
  const template: any = {
    width: 120,
    height: 80,
    inputs: [],
    outputs: [],
    defaultParams: { num: [1], den: [1] },
    render: (block: any, helpers: any) => {
      const g = block.group;
      const mathGroup = helpers.createSvgElement('g', { class: 'nf-tf-formula' });
      g.appendChild(mathGroup);
      const tex = helpers.buildTransferTeX(
        block.params.Numerator || [],
        block.params.Denominator || [],
        's'
      );
      helpers.renderTeXMath(mathGroup, tex, block.width, block.height);
    },
  };

  const map = libAny.getLoadedLibraries();
  map.set('fake.lib', {
    manifest: { id: 'fake.lib' },
    library: { id: 'fake.lib', title: 'Fake' },
    createTemplates: (helpers: any) => ({
      tf: {
        width: template.width,
        height: template.height,
        inputs: template.inputs,
        outputs: template.outputs,
        defaultParams: template.defaultParams,
        render: (block: any) => {
          const g = block.group;
          const mathGroup = helpers.createSvgElement('g', { class: 'nf-tf-formula' });
          g.appendChild(mathGroup);
          const tex = helpers.buildTransferTeX(
            block.params.Numerator || [],
            block.params.Denominator || [],
            's'
          );
          helpers.renderTeXMath(mathGroup, tex, block.width, block.height);
        },
      },
    }),
  });

  renderAny.refreshBlockTemplates();

  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyBlocks: new Set(),
    dirtyConnections: new Set(),
    selectedIds: new Set(),
    selectedConnections: new Set(),
    pendingPort: null,
    routingDirty: false,
    autoRoute: false,
  };

  const r = renderAny.createRenderer({
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  const blk = r.createBlock('tf', 10, 10, { id: 'b1', params: { num: [1, 2], den: [1, 3] } });
  expect(blk).not.toBeNull();
  const stored = state.blocks.get('b1');
  expect(stored).toBeTruthy();
  const mathGroup = stored.group.querySelector('.nf-tf-formula');
  expect(mathGroup).not.toBeNull();
  const fo = mathGroup.querySelector('foreignObject');
  expect(fo).not.toBeNull();

  const blk2 = r.createBlock('tf', 20, 20, {
    id: 'b2',
    params: { num: ['+x', '-2', '3'], den: ['0', '1'] },
  });
  expect(state.blocks.get('b2')).toBeTruthy();
  const mathGroup2 = state.blocks.get('b2').group.querySelector('.nf-tf-formula');
  expect(mathGroup2).not.toBeNull();
  const html = mathGroup2.innerHTML || '';
  expect(html.length).toBeGreaterThan(0);
  const blk3 = r.createBlock('tf', 30, 30, { id: 'b3', params: { num: [0, 0, 0], den: [] } });
  expect(state.blocks.get('b3')).toBeTruthy();
  const mathGroup3 = state.blocks.get('b3').group.querySelector('.nf-tf-formula');
  expect(mathGroup3).not.toBeNull();
});
//=============================================================================
