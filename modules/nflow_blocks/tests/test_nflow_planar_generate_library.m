%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% Unknown planar block errors with the documented identifier.
msg = '';
try
  NFlow.internal.planarGenerateIcon('NoSuchPlanarBlock');
catch e
  msg = e.identifier;
end
assert_istrue(strcmp(msg, 'Nelson:nflow:planarIconUnknown'));
%=============================================================================
% NFlow.internal.acausalWriteLibraries materialises the acausal palettes on disk.
dst = tempname();
dirs = NFlow.internal.acausalWriteLibraries(dst);
assert_istrue(~isempty(dirs));
assert_istrue(isfile([dst, '/acausal_electrical/library.json']));
assert_istrue(isfile([dst, '/acausal_electrical/exports/Resistor.svg']));
%=============================================================================
% NFlow.internal.planarGenerateLibrary: valid manifest, one icon per block, correct id.
plib = jsondecode(NFlow.internal.planarGenerateLibrary());
assert_istrue(strcmp(plib.id, 'builtin.acausal_planar'));
assert_istrue(plib.builtin == true);
assert_istrue(numel(plib.blocks) == numel(NFlow.internal.planarCatalog()));
for k = 1:numel(plib.blocks)
  assert_istrue(plib.blocks(k).acausal == true);
  assert_istrue(strcmp(plib.blocks(k).domain, 'planar'));
  assert_istrue(~isempty(plib.blocks(k).icon));
end
%=============================================================================
