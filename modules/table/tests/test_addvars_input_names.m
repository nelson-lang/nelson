%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a variable passed by name keeps its name, an expression is named Var<N>
T = table([1; 2], [3; 4], [5; 6], 'VariableNames', {'A', 'B', 'C'});
x = [7; 8];
y = [9; 10];
R = addvars(T, x);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'x'});
assert_isequal(R.x, x);
R = addvars(T, x, y);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'x', 'y'});
R = addvars(T, x, x);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'x', 'x_1'});
R = addvars(T, x, [1; 2]);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'x', 'Var5'});
R = addvars(T, [1; 2], x);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Var4', 'x'});
R = addvars(T, T.A);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Var4'});
c = {x};
R = addvars(T, c{1});
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Var4'});
R = addvars(T, x, 'After', 1);
assert_isequal(R.Properties.VariableNames, {'A', 'x', 'B', 'C'});
R = addvars(T, x, 'NewVariableNames', 'Z');
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Z'});
%=============================================================================
% names already used are suffixed _1, _2, ... skipping every new name
A = x;
R = addvars(T, A);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'A_1'});
assert_isequal(R.A_1, x);
R = addvars(T, A, A);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'A_1', 'A_2'});
Var4 = x;
R = addvars(T, Var4, [1; 2]);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Var4', 'Var5'});
R = addvars(T, [1; 2], Var4);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Var4', 'Var4_1'});
T2 = table([1; 2], [3; 4], 'VariableNames', {'Var3', 'x'});
R = addvars(T2, x);
assert_isequal(R.Properties.VariableNames, {'Var3', 'x', 'x_1'});
R = addvars(T2, x, x);
assert_isequal(R.Properties.VariableNames, {'Var3', 'x', 'x_1', 'x_2'});
x_1 = x;
R = addvars(T2, x, x_1);
assert_isequal(R.Properties.VariableNames, {'Var3', 'x', 'x_2', 'x_1'});
R = addvars(T2, [1; 2]);
assert_isequal(R.Properties.VariableNames, {'Var3', 'x', 'Var3_1'});
%=============================================================================
% dimension names are used names too
Row = x;
R = addvars(T, Row);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Row_1'});
Variables = x;
R = addvars(T, Variables);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'Variables_1'});
%=============================================================================
% timetable and eventtable
TT = timetable(seconds([1; 2]), [3; 4], [5; 6], 'VariableNames', {'A', 'B'});
R = addvars(TT, x, 'After', 0);
assert_istrue(istimetable(R));
assert_isequal(R.Properties.VariableNames, {'x', 'A', 'B'});
assert_isequal(R.Properties.RowTimes, seconds([1; 2]));
Time = x;
R = addvars(TT, Time);
assert_isequal(R.Properties.VariableNames, {'A', 'B', 'Time_1'});
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
R = addvars(ET, x, 'After', 0);
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableNames, {'x', 'EventLabels'});
R = addvars(ET, x);
assert_isequal(R.Properties.VariableNames, {'EventLabels', 'x'});
%=============================================================================
