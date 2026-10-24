%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% splitvars(T) splits every variable with several columns or holding a table
R = splitvars(table([1 3; 2 4]));
asserts.isequal(R.Properties.VariableNames, {'Var1_1', 'Var1_2'});
asserts.isequal(R.Var1_1, [1; 2]);
asserts.isequal(R.Var1_2, [3; 4]);
%=============================================================================
T = table([1; 2], [1 3; 2 4], {'a'; 'b'}, 'VariableNames', {'x', 'M', 'c'});
R = splitvars(T);
asserts.isequal(R.Properties.VariableNames, {'x', 'M_1', 'M_2', 'c'});
asserts.isequal(R.c, {'a'; 'b'});
%=============================================================================
inner = table([5; 6], [7 8; 9 10], 'VariableNames', {'p', 'q'});
R = splitvars(table([1; 2], inner, 'VariableNames', {'x', 'N'}));
asserts.isequal(R.Properties.VariableNames, {'x', 'p', 'q'});
asserts.isequal(R.q, [7 8; 9 10]);
%=============================================================================
R = splitvars(table([1; 2], table([5; 6], 'VariableNames', {'x'}), 'VariableNames', {'x', 'N'}));
asserts.isequal(R.Properties.VariableNames, {'x', 'N_x'});
asserts.isequal(R.N_x, [5; 6]);
%=============================================================================
T = table([1; 2], [3; 4]);
asserts.isequal(splitvars(T), T);
%=============================================================================
R = splitvars(table({1 2; 3 4}));
asserts.isequal(R.Var1_2, {2; 4});
T = table([1; 2]);
T.Var1 = {[1 2]; [3 4]};
R = splitvars(T);
asserts.isequal(R.Properties.VariableNames, {'Var1'});
asserts.isequal(R.Var1, {[1 2]; [3 4]});
%=============================================================================
R = splitvars(table(reshape(1:8, 2, 2, 2)));
asserts.isequal(R.Properties.VariableNames, {'Var1_1', 'Var1_2', 'Var1_3', 'Var1_4'});
asserts.isequal(R.Var1_4, [7; 8]);
%=============================================================================
R = splitvars(table([1 2; 3 4], [5; 6], 'VariableNames', {'A', 'A_1'}));
asserts.isequal(R.Properties.VariableNames, {'A_1_1', 'A_2', 'A_1'});
R = splitvars(table([1 2; 3 4], [5 6; 7 8], 'VariableNames', {'A', 'A_1'}));
asserts.isequal(R.Properties.VariableNames, {'A_1', 'A_2', 'A_1_1', 'A_1_2'});
%=============================================================================
T = table([1 2; 3 4], [5; 6], 'VariableNames', {'A', 'B'});
T.Properties.VariableUnits = {'m', 's'};
T.Properties.VariableDescriptions = {'da', 'db'};
R = splitvars(T);
asserts.isequal(R.Properties.VariableUnits, {'m', 'm', 's'});
asserts.isequal(R.Properties.VariableDescriptions, {'da', 'da', 'db'});
%=============================================================================
% several variables, by name, number or logical mask
T = table([1 2; 3 4], [5 6; 7 8], [9; 10], 'VariableNames', {'A', 'B', 'C'});
expected = {'A_1', 'A_2', 'B_1', 'B_2', 'C'};
asserts.isequal(splitvars(T, {'A', 'B'}).Properties.VariableNames, expected);
asserts.isequal(splitvars(T, ["B", "A"]).Properties.VariableNames, expected);
asserts.isequal(splitvars(T, [1 2]).Properties.VariableNames, expected);
asserts.isequal(splitvars(T, [2 1]).Properties.VariableNames, expected);
R = splitvars(T, logical([1 0 1]));
asserts.isequal(R.Properties.VariableNames, {'A_1', 'A_2', 'B', 'C'});
asserts.isequal(R.B, [5 6; 7 8]);
asserts.isequal(splitvars(T, {}), T);
%=============================================================================
T = table([1 2; 3 4], [5 6; 7 8], 'VariableNames', {'A', 'B'});
R = splitvars(T, {'A', 'B'}, 'NewVariableNames', {{'a1', 'a2'}, ["b1", "b2"]});
asserts.isequal(R.Properties.VariableNames, {'a1', 'a2', 'b1', 'b2'});
asserts.isequal(R.b2, [6; 8]);
R = splitvars(table([1 2; 3 4], [5; 6], 'VariableNames', {'A', 'B'}), {'A', 'B'}, 'NewVariableNames', {{'a1', 'a2'}, {'bb'}});
asserts.isequal(R.Properties.VariableNames, {'a1', 'a2', 'bb'});
R = splitvars(table([1; 2], 'VariableNames', {'A'}), 'A', 'NewVariableNames', {'z'});
asserts.isequal(R.Properties.VariableNames, {'z'});
R = splitvars(table([1 2; 3 4], 'VariableNames', {'A'}), 'A', 'NewVariableNames', ["a1", "a2"]);
asserts.isequal(R.Properties.VariableNames, {'a1', 'a2'});
%=============================================================================
msgMulti = 'When splitting several variables, NewVariableNames must be a cell array with one cell of names per variable.';
assert_checkerror('splitvars(T, {''A'', ''B''}, ''NewVariableNames'', {''a1'', ''a2'', ''b1'', ''b2''})', msgMulti, 'Nelson:table:splitvars:incorrectNumVarnamesMultisplit');
assert_checkerror('splitvars(T, {''A'', ''B''}, ''NewVariableNames'', {{''a1'', ''a2''}})', msgMulti, 'Nelson:table:splitvars:incorrectNumVarnamesMultisplit');
assert_checkerror('splitvars(T, [1 1])', 'List of variables to split must not specify any variable more than once.', 'Nelson:table:splitvars:duplicateVars');
assert_checkerror('splitvars(T, 3)', 'Variable index exceeds table dimensions.', 'Nelson:table:varIndexOutOfRange');
assert_checkerror('splitvars(T, ''A'', ''NewVariableNames'', {''B'', ''C''})', 'Duplicate table variable name.', 'Nelson:table:duplicateVariableNames');
%=============================================================================
TT = timetable(seconds([1; 2]), [1 3; 2 4], [5 7; 6 8]);
R = splitvars(TT);
asserts.isequal(class(R), 'timetable');
asserts.isequal(R.Properties.VariableNames, {'Var1_1', 'Var1_2', 'Var2_1', 'Var2_2'});
asserts.isequal(R.Properties.RowTimes, seconds([1; 2]));
R = splitvars(TT, [1 2], 'NewVariableNames', {{'a', 'b'}, {'c', 'd'}});
asserts.isequal(R.Properties.VariableNames, {'a', 'b', 'c', 'd'});
asserts.isequal(R.d, [7; 8]);
%=============================================================================
E = eventtable(seconds([1; 2]), 'EventLabels', {'a'; 'b'});
E.Extra = [1 2; 3 4];
R = splitvars(E);
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Properties.VariableNames, {'EventLabels', 'Extra_1', 'Extra_2'});
asserts.isequal(R.Extra_2, [2; 4]);
%=============================================================================
