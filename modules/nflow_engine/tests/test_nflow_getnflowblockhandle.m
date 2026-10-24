%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% getNFlowBlockHandle returns a block handle by path, or -1 when the block is
% not found (no error). A cell array of paths yields an array of handles. The
% handle equals get_param(path, 'Handle'). An optional load flag loads an
% unloaded model first.
%=============================================================================
bdclose('all');
new_system('nfd');
add_block('nflow/math/gain', 'nfd/G1');
add_block('nflow/math/gain', 'nfd/G2');
%=============================================================================
% A valid block path resolves to a positive numeric scalar handle.
h1 = getNFlowBlockHandle('nfd/G1');
assert_istrue(isa(h1, 'double'));
assert_istrue(isscalar(h1));
assert_istrue(h1 > 0);
%=============================================================================
% The handle equals get_param(path, 'Handle') and round-trips through get_param.
assert_isequal(get_param('nfd/G1', 'Handle'), h1);
set_param('nfd/G1', 'Gain', '7');
assert_isequal(get_param(h1, 'Gain'), '7');
%=============================================================================
% An unknown block yields -1 (no error). A model-only path is not a block: -1.
assert_isequal(getNFlowBlockHandle('nfd/Nope'), -1);
assert_isequal(getNFlowBlockHandle('nfd'), -1);
%=============================================================================
% A numeric argument is not a path and resolves to -1.
assert_isequal(getNFlowBlockHandle(h1), -1);
%=============================================================================
% A cell array of paths yields a double array of the same shape.
hc = getNFlowBlockHandle({'nfd/G1', 'nfd/Nope', 'nfd/G2'});
assert_istrue(isa(hc, 'double'));
assert_isequal(size(hc), [1 3]);
assert_istrue(hc(1) > 0);
assert_isequal(hc(2), -1);
assert_istrue(hc(3) > 0);
%=============================================================================
% An unloaded model yields -1 without the load flag, and loads with it.
old = pwd();
cd(tempdir());
save_system('nfd', 'nfd.nflow');
bdclose('nfd');
assert_isequal(getNFlowBlockHandle('nfd/G1'), -1);
assert_isfalse(bdIsLoaded('nfd'));
h2 = getNFlowBlockHandle('nfd/G1', true);
assert_istrue(h2 > 0);
assert_istrue(bdIsLoaded('nfd'));
% A missing model with the load flag still yields -1 (no error).
assert_isequal(getNFlowBlockHandle('no_such_model/X', true), -1);
bdclose('nfd');
delete('nfd.nflow');
cd(old);
%=============================================================================
bdclose('all');
%=============================================================================
