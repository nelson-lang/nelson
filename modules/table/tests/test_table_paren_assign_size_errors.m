%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a variable assigned with parentheses keeps its size: the value of a
% multicolumn variable must have its columns, only a scalar is expanded
rowMsg = _('To assign to or create a variable in a table, the number of rows must match the height of the table.');
varMsg = _('The number of table variables in an assignment must match.');
rhsMsg = _('Right hand side of an assignment into a table must be another table or a cell array.');
dimId = 'Nelson:table:AssignmentDimensionMismatch';
dimMsg = @(name, left, right) sprintf(_('Unable to perform assignment for table variable ''%s'' because its size on the left side is %s and the size of the corresponding variable on the right side is %s.'), name, left, right);
%=============================================================================
T = table([1; 2; 3]);
assert_checkerror('T([2 3], 1) = table([9 8]);', dimMsg('Var1', '2-by-1', '2-by-2'), dimId);
assert_checkerror('T([2 3], 1) = {[9 8]};', dimMsg('Var1', '2-by-1', '2-by-2'), dimId);
assert_checkerror('T(2, 1) = {[9 8]};', dimMsg('Var1', '1-by-1', '1-by-2'), dimId);
assert_checkerror('T([4 5], 1) = {[9 8]};', dimMsg('Var1', '2-by-1', '2-by-2'), dimId);
assert_checkerror('T([2 3], 1) = table(zeros(1, 0));', dimMsg('Var1', '2-by-1', '2-by-0'), dimId);
assert_isequal(T.Var1, [1; 2; 3]);
%=============================================================================
T = table([1; 2; 3], [4; 5; 6], 'VariableNames', {'A', 'Alpha'});
assert_checkerror('T([2 3], :) = table([9; 8], [1 2; 3 4]);', dimMsg('Alpha', '2-by-1', '2-by-2'), dimId);
%=============================================================================
T = table([1 2; 3 4; 5 6]);
assert_checkerror('T([2 3], 1) = {9};', dimMsg('Var1', '2-by-2', '2-by-1'), dimId);
assert_checkerror('T(2, 1) = {[9 8 7]};', dimMsg('Var1', '1-by-2', '1-by-3'), dimId);
T(2, 1) = {9};
assert_isequal(T.Var1, [1 2; 9 9; 5 6]);
%=============================================================================
T = table(zeros(3, 2, 2));
assert_checkerror('T([2 3], 1) = {ones(1, 3)};', dimMsg('Var1', '2-by-2-by-2', '2-by-3'), dimId);
T([2 3], 1) = {ones(1, 2, 2)};
assert_isequal(T.Var1(:, :, 2), [0 0; 1 1; 1 1]);
%=============================================================================
T = table([1; 2; 3]);
T([2 3], 'B') = {[9 8]};
assert_isequal(T.B, [0 0; 9 8; 9 8]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
assert_checkerror('TT([2 3], 1) = table([9 8]);', dimMsg('Var1', '2-by-1', '2-by-2'), dimId);
assert_checkerror('TT([2 3], 1) = {[9 8]};', dimMsg('Var1', '2-by-1', '2-by-2'), dimId);
%=============================================================================
% time values: a scalar is expanded, other sizes are checked
T = table(datetime(2020, 1, [1; 2; 3]));
T([2 3], 1) = {datetime(2021, 1, 1)};
assert_isequal(year(T.Var1), [2020; 2021; 2021]);
assert_checkerror('T(2, 1) = table(datetime(2021, 1, [1 2]));', dimMsg('Var1', '1-by-1', '1-by-2'), dimId);
T = table(seconds([1 2; 3 4; 5 6]));
T(2, 1) = {seconds(9)};
assert_isequal(seconds(T.Var1), [1 2; 9 9; 5 6]);
assert_checkerror('T([2 3], 1) = {seconds(9)};', dimMsg('Var1', '2-by-2', '2-by-1'), dimId);
%=============================================================================
% an array that is not a cell is sized like a table (rows, then columns),
% then refused; only a double [] deletes
T = table([1; 2; 3], [4; 5; 6]);
empties = {'''''', 'zeros(0, 0, ''single'')', 'string.empty', 'int8([])', 'false(0, 0)', 'zeros(0, 1)', 'zeros(0, 0, 2)', ...
  'categorical([])', 'datetime.empty', 'seconds([])', 'struct([])', 'sparse([])', 'table()'};
for k = 1:numel(empties)
  assert_checkerror(['T(2, :) = ', empties{k}, ';'], rowMsg, 'Nelson:table:RowDimensionMismatch');
  assert_checkerror(['T(:, 1) = ', empties{k}, ';'], rowMsg, 'Nelson:table:RowDimensionMismatch');
end
assert_isequal(size(T), [3 2]);
assert_checkerror('T(2, :) = zeros(1, 0);', varMsg, 'Nelson:table:VarDimensionMismatch');
assert_checkerror('T(:, 1) = zeros(1, 0);', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T(:, 1) = [1 2];', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T(2, 1) = [1 2];', varMsg, 'Nelson:table:VarDimensionMismatch');
assert_checkerror('T(2, :) = [1 2];', rhsMsg, 'Nelson:table:invalidRHS');
assert_checkerror('T(:, :) = 5;', rhsMsg, 'Nelson:table:invalidRHS');
assert_checkerror('T(2, :) = "";', rhsMsg, 'Nelson:table:invalidRHS');
assert_checkerror('T(2, :) = missing;', rhsMsg, 'Nelson:table:invalidRHS');
T(2, :) = [];
assert_isequal(T.Variables, [1 4; 3 6]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], [4; 5; 6]);
assert_checkerror('TT(2, :) = '''';', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('TT(2, :) = single([]);', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('TT(2, :) = sparse([]);', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('TT(2, :) = zeros(1, 0);', varMsg, 'Nelson:table:VarDimensionMismatch');
assert_checkerror('TT(2, :) = [1 2];', rhsMsg, 'Nelson:table:invalidRHS');
assert_isequal(TT.Variables, [1 4; 2 5; 3 6]);
TT(2, :) = [];
assert_isequal(seconds(TT.Properties.RowTimes), [1; 3]);
%=============================================================================
