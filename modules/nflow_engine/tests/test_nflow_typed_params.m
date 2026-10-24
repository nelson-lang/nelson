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
% Typed parameters and geometry: vectors, matrices and Position
% round-trip through set_param/get_param and save/load without manual string
% serialization. Scalars keep the string convention.
%=============================================================================
bdclose('all');
new_system('m');
add_block('nflow/math/gain', 'm/G');
%=============================================================================
% Row vector.
set_param('m/G', 'Table', [1 2 3 4]);
t = get_param('m/G', 'Table');
assert_isequal(t, [1 2 3 4]);
assert_isequal(size(t), [1 4]);
%=============================================================================
% Matrix.
set_param('m/G', 'A', [1 2; 3 4]);
assert_isequal(get_param('m/G', 'A'), [1 2; 3 4]);
%=============================================================================
% Column vector shape is preserved.
set_param('m/G', 'col', [10; 20; 30]);
c = get_param('m/G', 'col');
assert_isequal(size(c), [3 1]);
assert_isequal(c, [10; 20; 30]);
%=============================================================================
% Scalars keep the string convention (back-compatible, reference-aligned).
set_param('m/G', 'gain', 2);
g = get_param('m/G', 'gain');
assert_istrue(ischar(g));
assert_isequal(g, '2');
%=============================================================================
% Geometry: Position is a real [x y x2 y2] vector.
set_param('m/G', 'Position', [100 200 180 260]);
assert_isequal(get_param('m/G', 'Position'), [100 200 180 260]);
%=============================================================================
% add_block accepts typed parameter values too.
add_block('nflow/math/gain', 'm/H', 'Coeffs', [5 6 7]);
assert_isequal(get_param('m/H', 'Coeffs'), [5 6 7]);
%=============================================================================
% A malformed Position is rejected and leaves the model unchanged (atomic).
assert_checkerror('set_param(''m/G'', ''Position'', [1 2 3])', _('Position must be a 1x4 [x y x2 y2] vector.'));
%=============================================================================
% Save / load preserves typed values and shapes.
f = [tempname(), '.nflow'];
save_system('m', f);
bdclose('m');
load_system(f);
assert_isequal(get_param('m/G', 'Table'), [1 2 3 4]);
assert_isequal(get_param('m/G', 'A'), [1 2; 3 4]);
assert_isequal(get_param('m/G', 'col'), [10; 20; 30]);
assert_isequal(get_param('m/G', 'Position'), [100 200 180 260]);
bdclose('all');
rmfile(f);
%=============================================================================
