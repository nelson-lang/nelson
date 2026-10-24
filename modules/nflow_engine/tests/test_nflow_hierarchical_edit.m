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
% Hierarchical editing by paths: add_block/set_param/get_param/
% add_line/delete_block operate on nested subsystems addressed as
% model/Sub/.../Block, without editing JSON by hand.
%=============================================================================
bdclose('all');
new_system('m');
add_block('subsystem', 'm/Sub');
%=============================================================================
% Add and configure a block inside the subsystem via its path.
p = add_block('nflow/math/gain', 'm/Sub/G', 'gain', 5);
assert_isequal(p, 'm/Sub/G');
assert_isequal(get_param('m/Sub/G', 'gain'), '5');
% The nested block is not visible at the root.
assert_isfalse(any(strcmp(get_param('m', 'Blocks'), 'G')));
%=============================================================================
% A line between two nested blocks in the same subsystem.
add_block('nflow/source/sine', 'm/Sub/S');
add_line('m', 'Sub/S/1', 'Sub/G/1');
%=============================================================================
% Geometry resolves through the path.
set_param('m/Sub/G', 'Position', [10 20 90 100]);
assert_isequal(get_param('m/Sub/G', 'Position'), [10 20 90 100]);
%=============================================================================
% A cross-subsystem line is rejected.
add_block('nflow/math/gain', 'm/Top');
assert_checkerror('add_line(''m'', ''Sub/S/1'', ''Top/1'')', ...
  _('A connection must be between two blocks of the same (sub)system.'));
%=============================================================================
% Save / load preserves the nested graph.
f = [tempname(), '.nflow'];
save_system('m', f);
bdclose('m');
load_system(f);
assert_isequal(get_param('m/Sub/G', 'gain'), '5');
assert_isequal(get_param('m/Sub/G', 'Position'), [10 20 90 100]);
%=============================================================================
% A nested delete removes only the nested block.
delete_block('m/Sub/G');
assert_checkerror('get_param(''m/Sub/G'', ''gain'')', _('No block named ''Sub/G''.'));
bdclose('all');
rmfile(f);
%=============================================================================
