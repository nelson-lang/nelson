%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a 1x1 cell is expanded to the indexed rows, like a scalar
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], 'VariableNames', {'A'});
TT([2 3], 'A') = {9};
assert_isequal(TT.A, [1; 9; 9]);
assert_isequal(seconds(TT.Properties.RowTimes), [1; 2; 3]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], 'VariableNames', {'A'});
TT(withtol(seconds(2), seconds(0.1)), 'A') = {9};
assert_isequal(TT.A, [1; 9; 3]);
%=============================================================================
T = table([1; 2; 3]);
T([2 3], 1) = {9};
assert_isequal(T.Var1, [1; 9; 9]);
%=============================================================================
T = table([1; 2; 3]);
T(:, 1) = {9};
assert_isequal(T.Var1, [9; 9; 9]);
%=============================================================================
% ... and to every indexed variable
T = table([1; 2; 3], [4; 5; 6]);
T([2 3], :) = {9};
assert_isequal(T.Variables, [1 4; 9 9; 9 9]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], [4; 5; 6]);
TT([1 3], :) = {0};
assert_isequal(TT.Variables, [0 0; 2 5; 0 0]);
%=============================================================================
T = table({'a'; 'b'; 'c'});
T([2 3], 1) = {'z'};
assert_isequal(T.Var1, {'a'; 'z'; 'z'});
%=============================================================================
% new rows and new variables
T = table([1; 2; 3]);
T([4 5], 1) = {9};
assert_isequal(T.Var1, [1; 2; 3; 9; 9]);
%=============================================================================
T = table([1; 2; 3]);
T([2 3], 'B') = {9};
assert_isequal(T.B, [0; 9; 9]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
TT(seconds([4 5]), 1) = {7};
assert_isequal(TT.Var1, [1; 2; 3; 7; 7]);
assert_isequal(seconds(TT.Properties.RowTimes), [1; 2; 3; 4; 5]);
%=============================================================================
% one cell per row and variable still assigns element-wise
T = table([1; 2; 3], [4; 5; 6]);
T([2 3], :) = {9, 8; 7, 6};
assert_isequal(T.Variables, [1 4; 9 8; 7 6]);
%=============================================================================
% other sizes: rows are checked first, then variables
rowMsg = _('To assign to or create a variable in a table, the number of rows must match the height of the table.');
varMsg = _('The number of table variables in an assignment must match.');
T = table([1; 2; 3], [4; 5; 6]);
assert_checkerror('T([2 3], :) = {9, 8};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T([2 3], :) = {9, 8, 7};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T([2 3], :) = table(9, 8);', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T([2 3], :) = {9; 8};', varMsg, 'Nelson:table:VarDimensionMismatch');
assert_checkerror('T([2 3], :) = table([9; 8]);', varMsg, 'Nelson:table:VarDimensionMismatch');
assert_isequal(T.Variables, [1 4; 2 5; 3 6]);
%=============================================================================
T = table([1; 2; 3]);
assert_checkerror('T([2 3], 1) = {};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T([2 3], 1) = cell(0, 1);', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T(:, 1) = {};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T(:, ''NewVar'') = table([1; 2], [3; 4]);', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T(:, ''NewVar'') = table([1; 2; 3], [3; 4; 5]);', varMsg, 'Nelson:table:VarDimensionMismatch');
assert_checkerror('T([2 3], {''B'', ''C''}) = {9, 8};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('T([2 3], {''B'', ''C''}) = {9; 8};', varMsg, 'Nelson:table:VarDimensionMismatch');
assert_isequal(T.Properties.VariableNames, {'Var1'});
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], [4; 5; 6]);
assert_checkerror('TT([2 3], 1) = {9, 8};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('TT(2, :) = {9; 8};', rowMsg, 'Nelson:table:RowDimensionMismatch');
assert_checkerror('TT([2 3], :) = {9; 8};', varMsg, 'Nelson:table:VarDimensionMismatch');
%=============================================================================
