%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% gcbh returns a numeric handle for the current block. A block handle is also
% obtainable by path through get_param(path, 'Handle'); it round-trips through
% get_param/set_param and is invalidated when the block is deleted or its model
% is closed.
%=============================================================================
bdclose('all');
%=============================================================================
% With no editor active there is no current block, so gcbh returns [] (mirroring
% gcb, which returns the empty string).
assert_isequal(gcbh(), []);
assert_istrue(isempty(gcbh()));
%=============================================================================
new_system('demo');
add_block('nflow/math/gain', 'demo/Gain');
set_param('demo/Gain', 'Gain', '2');
%=============================================================================
% A block handle by path is a non-empty numeric scalar.
h = get_param('demo/Gain', 'Handle');
assert_istrue(isa(h, 'double'));
assert_istrue(isscalar(h));
assert_isfalse(isempty(h));
%=============================================================================
% The handle round-trips through get_param/set_param onto the block it names.
assert_isequal(get_param(h, 'Gain'), '2');
set_param(h, 'Gain', '5');
assert_isequal(get_param('demo/Gain', 'Gain'), '5');
%=============================================================================
% Typed geometry round-trips through the handle too.
set_param(h, 'Position', [10 20 60 50]);
assert_isequal(get_param(h, 'Position'), [10 20 60 50]);
%=============================================================================
% Requesting the handle again returns the same value (idempotent).
assert_isequal(get_param('demo/Gain', 'Handle'), h);
%=============================================================================
% A model handle is distinct from a block handle.
mh = get_param('demo', 'Handle');
assert_istrue(mh ~= h);
%=============================================================================
% Deleting the block invalidates its handle.
delete_block('demo/Gain');
assert_checkerror('get_param(h, ''Gain'')', _('Invalid nflow model handle.'));
%=============================================================================
% Closing the model invalidates handles belonging to it.
add_block('nflow/math/gain', 'demo/G2');
h2 = get_param('demo/G2', 'Handle');
assert_isfalse(isempty(h2));
bdclose('demo');
assert_checkerror('get_param(h2, ''Gain'')', _('Invalid nflow model handle.'));
%=============================================================================
bdclose('all');
%=============================================================================
