%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a 1x1 table is expanded to the indexed rows and variables, like a scalar
T = table([1; 2; 3], [4; 5; 6]);
T([2 3], :) = table(9);
assert_isequal(T.Variables, [1 4; 9 9; 9 9]);
assert_isequal(T.Properties.VariableNames, {'Var1', 'Var2'});
%=============================================================================
T = table([1; 2; 3], [4; 5; 6]);
T(2, :) = table(9);
assert_isequal(T.Variables, [1 4; 9 9; 3 6]);
%=============================================================================
% its variable name and row name are not assigned
T = table([1; 2; 3]);
T(:, 1) = table(9, 'VariableNames', {'Z'});
assert_isequal(T.Var1, [9; 9; 9]);
assert_isequal(T.Properties.VariableNames, {'Var1'});
%=============================================================================
T = table([1; 2; 3], [4; 5; 6], 'RowNames', {'a', 'b', 'c'});
T({'b', 'c'}, :) = table(9, 'RowNames', {'q'});
assert_isequal(T.Variables, [1 4; 9 9; 9 9]);
assert_isequal(T.Properties.RowNames, {'a'; 'b'; 'c'});
%=============================================================================
T = table({'a'; 'b'; 'c'});
T([2 3], 1) = table({'z'});
assert_isequal(T.Var1, {'a'; 'z'; 'z'});
%=============================================================================
% new rows and new variables
T = table([1; 2; 3]);
T([4 5], 1) = table(9);
assert_isequal(T.Var1, [1; 2; 3; 9; 9]);
%=============================================================================
T = table([1; 2; 3]);
T([2 3], {'B', 'C'}) = table(9);
assert_isequal(T.B, [0; 9; 9]);
assert_isequal(T.C, [0; 9; 9]);
%=============================================================================
% timetables: the row times of the target are kept
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], [4; 5; 6]);
TT([1 3], :) = table(0);
assert_isequal(TT.Variables, [0 0; 2 5; 0 0]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], [4; 5; 6]);
TT([1 3], :) = timetable(seconds(7), 0);
assert_isequal(TT.Variables, [0 0; 2 5; 0 0]);
assert_isequal(seconds(TT.Properties.RowTimes), [1; 2; 3]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], [4; 5; 6]);
TT([1 3], :) = timetable(seconds([7; 8]), [0; 0], [0; 0]);
assert_isequal(TT.Variables, [0 0; 2 5; 0 0]);
assert_isequal(seconds(TT.Properties.RowTimes), [1; 2; 3]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
TT(withtol(seconds(2), seconds(0.1)), 1) = table(9);
assert_isequal(TT.Var1, [1; 9; 3]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
TT(seconds([4 5]), 1) = table(7);
assert_isequal(TT.Var1, [1; 2; 3; 7; 7]);
assert_isequal(seconds(TT.Properties.RowTimes), [1; 2; 3; 4; 5]);
%=============================================================================
% other sizes, and an empty cell, do not match the indexed table
rowMsg = _('To assign to or create a variable in a table, the number of rows must match the height of the table.');
T = table([1; 2; 3], [4; 5; 6]);
assert_checkerror('T([2 3], :) = table(9, 8);', rowMsg, 'Nelson:table:RowDimensionMismatch');
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
assert_checkerror('TT(2, :) = {};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_isequal(TT.Var1, [1; 2; 3]);
%=============================================================================
