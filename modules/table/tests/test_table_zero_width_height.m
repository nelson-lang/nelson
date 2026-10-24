%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a table without variables keeps its number of rows
%=============================================================================
T = cell2table(cell(2, 0));
assert_isequal(size(T), [2 0]);
assert_isequal(height(T), 2);
assert_isequal(width(T), 0);
assert_istrue(isempty(T));
assert_isequal(size(cell2table(cell(0, 0))), [0 0]);
assert_isequal(size(cell2table(cell(0, 2))), [0 2]);
assert_isequal(size(array2table(zeros(2, 0))), [2 0]);
assert_isequal(size(array2table(zeros(0, 0))), [0 0]);
assert_isequal(size(struct2table(repmat(struct(), 3, 1))), [3 0]);
%=============================================================================
T = table('Size', [2 0], 'VariableTypes', {});
assert_isequal(size(T), [2 0]);
assert_isequal(size(table('Size', [0 0], 'VariableTypes', {})), [0 0]);
%=============================================================================
T = table('RowNames', {'a'; 'b'});
assert_isequal(size(T), [2 0]);
assert_isequal(T.Properties.RowNames, {'a'; 'b'});
%=============================================================================
% deleting every variable
T = table([1; 2]);
T(:, 1) = [];
assert_isequal(size(T), [2 0]);
T = table([1; 2]);
T.Var1 = [];
assert_isequal(size(T), [2 0]);
T = table([1; 2], 'RowNames', {'a'; 'b'});
T(:, 1) = [];
assert_isequal(size(T), [2 0]);
assert_isequal(T.Properties.RowNames, {'a'; 'b'});
%=============================================================================
% indexing
T = table([1; 2]);
assert_isequal(size(T(:, [])), [2 0]);
T = cell2table(cell(3, 0));
assert_isequal(size(T(1, :)), [1 0]);
assert_isequal(size(T([1 3], [])), [2 0]);
T(2, :) = [];
assert_isequal(size(T), [2 0]);
%=============================================================================
% concatenation
T = cell2table(cell(2, 0));
R = [T, table([5; 6])];
assert_isequal(size(R), [2 1]);
assert_isequal(R.Var1, [5; 6]);
assert_isequal(size([table([5; 6]), T]), [2 1]);
assert_checkerror('[cell2table(cell(3, 0)), table([5; 6])]', 'All tables must have the same number of rows.');
assert_isequal(size([cell2table(cell(2, 0)); cell2table(cell(3, 0))]), [5 0]);
%=============================================================================
% adding a variable must match the height
T = cell2table(cell(2, 0));
T.a = [1; 2];
assert_isequal(size(T), [2 1]);
T = cell2table(cell(2, 0));
assert_checkerror('T.a = [1; 2; 3];', 'Value assignment must have the same number of rows as the table.');
T = addvars(cell2table(cell(2, 0)), [7; 8]);
assert_isequal(size(T), [2 1]);
T = table();
T.a = [1; 2; 3];
assert_isequal(size(T), [3 1]);
%=============================================================================
% comparison, arithmetic and conversions
assert_isfalse(isequal(cell2table(cell(2, 0)), cell2table(cell(3, 0))));
assert_istrue(isequal(cell2table(cell(2, 0)), cell2table(cell(2, 0))));
assert_isequal(size(cell2table(cell(2, 0)) + 1), [2 0]);
assert_isequal(size(table2cell(cell2table(cell(2, 0)))), [2 0]);
assert_isequal(table2array(cell2table(cell(2, 0))), zeros(2, 0));
%=============================================================================
% timetable without variables
TT = timetable(seconds([1; 2]));
assert_isequal(size(TT), [2 0]);
assert_istrue(isempty(TT));
TT = timetable(seconds([1; 2]), [3; 4]);
TT(:, 1) = [];
assert_isequal(size(TT), [2 0]);
%=============================================================================
