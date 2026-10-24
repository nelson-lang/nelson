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
% Reloading a model must reflect the file's current content and must not
% keep a stale copy from a previous load of a same-named model. Regression
% test for NFlowModelRegistry::add() silently discarding freshly-parsed
% content on a name collision.
%=============================================================================
bdclose('all');
%=============================================================================
% First version of model 'rl': Gain = 5.
new_system('rl');
add_block('nflow/math/gain', 'rl/Gain', 'gain', 5);
f1 = [tempname(), '.nflow'];
save_system('rl', f1);
bdclose('rl');
%=============================================================================
% Second version of the same-named model 'rl': Gain = 9.
new_system('rl');
add_block('nflow/math/gain', 'rl/Gain', 'gain', 9);
f2 = [tempname(), '.nflow'];
save_system('rl', f2);
bdclose('rl');
%=============================================================================
% Load v1, then reload v2 while 'rl' is still loaded (name collision).
h1 = load_system(f1);
asserts.istrue(bdIsLoaded('rl'));
asserts.isequal(get_param('rl/Gain', 'gain'), '5');
h2 = load_system(f2);
% The reload must refresh the content instead of keeping the stale v1.
asserts.isequal(get_param('rl/Gain', 'gain'), '9');
% The handle stays stable across the in-place refresh.
asserts.isequal(h1, h2);
%=============================================================================
bdclose('all');
rmfile(f1);
rmfile(f2);
%=============================================================================
