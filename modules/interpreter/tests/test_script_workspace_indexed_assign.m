%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Indexed assignments in a script must extend variables inherited from the
% calling workspace, including when the script's first textual assignment to
% the variable is the indexed assignment itself.
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
%=============================================================================
% cell: first statement of the script indexes an inherited variable
filename = [path_test, '/ws_cell_extend.m'];
filewrite(filename, "wsCellVar{5} = 55;");
wsCellVar = {11, 22, 33};
run(filename);
asserts.isequal(size(wsCellVar), [1 5]);
asserts.isequal(wsCellVar{1}, 11);
asserts.isequal(wsCellVar{3}, 33);
asserts.isequal(wsCellVar{5}, 55);
%=============================================================================
% cell: an assignment in a dead branch must not hide the inherited value
filename = [path_test, '/ws_cell_dead_branch.m'];
filewrite(filename, ["if false"; "  wsDeadVar = 0;"; "end"; ...
  "wsDeadVar{5} = 55;"]);
wsDeadVar = {11, 22, 33};
run(filename);
asserts.isequal(wsDeadVar, {11, 22, 33, [], 55});
%=============================================================================
% vector growth loop over an inherited vector
filename = [path_test, '/ws_vec_grow.m'];
filewrite(filename, "for wsK = 4:6, wsVecVar(wsK) = wsK * 10; end");
wsVecVar = [1 2 3];
run(filename);
asserts.isequal(wsVecVar, [1 2 3 40 50 60]);
%=============================================================================
% struct array growth over an inherited scalar struct
filename = [path_test, '/ws_struct_grow.m'];
filewrite(filename, "for wsK = 2:3, wsStructVar(wsK).Id = wsK; end");
wsStructVar = struct('Id', 1, 'Name', 'a');
run(filename);
asserts.isequal([wsStructVar.Id], [1 2 3]);
asserts.isequal(wsStructVar(1).Name, 'a');
%=============================================================================
% struct field added in a script after a dead-branch assignment
filename = [path_test, '/ws_field_dead_branch.m'];
filewrite(filename, ["if false"; "  wsFieldVar = 0;"; "end"; ...
  "wsFieldVar.b = 2;"]);
wsFieldVar.a = 1;
run(filename);
asserts.isequal(wsFieldVar.a, 1);
asserts.isequal(wsFieldVar.b, 2);
%=============================================================================
% 2D resize of an inherited matrix
filename = [path_test, '/ws_matrix_resize.m'];
filewrite(filename, "wsMatVar(2, 3) = 7;");
wsMatVar = [1 2; 3 4];
run(filename);
asserts.isequal(wsMatVar, [1 2 0; 3 4 7]);
%=============================================================================
% end-based growth of an inherited cell
filename = [path_test, '/ws_cell_end_grow.m'];
filewrite(filename, "for wsK = 1:3, wsEndVar{end + 1} = wsK * 100; end");
wsEndVar = {11, 22, 33};
run(filename);
asserts.isequal(wsEndVar, {11, 22, 33, 100, 200, 300});
%=============================================================================
% a variable that does not exist yet is still created by the script
filename = [path_test, '/ws_cell_create.m'];
filewrite(filename, "wsNewVar{2} = 5;");
run(filename);
asserts.isequal(wsNewVar, {[], 5});
%=============================================================================
rmdir(path_test, 's');
%=============================================================================
