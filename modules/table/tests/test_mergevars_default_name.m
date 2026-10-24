%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The merged variable takes the place of the first variable listed and is
% named VarN (N = its position), made unique against the remaining names.
%=============================================================================
R = mergevars(table([1; 2], [3; 4]), [1 2]);
asserts.isequal(R.Properties.VariableNames, {'Var1'});
asserts.isequal(R.Var1, [1 3; 2 4]);
%=============================================================================
T3 = table([1; 2], [3; 4], [5; 6]);
R = mergevars(T3, [2 3]);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2'});
asserts.isequal(R.Var2, [3 5; 4 6]);
R = mergevars(T3, [3 2]);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2'});
asserts.isequal(R.Var2, [5 3; 6 4]);
R = mergevars(T3, [3 1]);
asserts.isequal(R.Properties.VariableNames, {'Var2', 'Var2_1'});
asserts.isequal(R.Var2_1, [5 1; 6 2]);
%=============================================================================
T4 = table([1; 2], [3; 4], [5; 6], [7; 8]);
R = mergevars(T4, [3 1]);
asserts.isequal(R.Properties.VariableNames, {'Var2', 'Var2_1', 'Var4'});
R = mergevars(T4, [4 2]);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var3', 'Var3_1'});
asserts.isequal(R.Var3_1, [7 3; 8 4]);
R = mergevars(T4, [4 1 2]);
asserts.isequal(R.Properties.VariableNames, {'Var3', 'Var2'});
asserts.isequal(R.Var2, [7 1 3; 8 2 4]);
R = mergevars(T4, logical([1 0 1 0]));
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2', 'Var4'});
asserts.isequal(R.Var1, [1 5; 2 6]);
R = mergevars(T4, {'Var4', 'Var1'});
asserts.isequal(R.Properties.VariableNames, {'Var2', 'Var3', 'Var3_1'});
%=============================================================================
A = [1; 2]; B = [3; 4]; C = [5; 6];
TN = table(A, B, C);
R = mergevars(TN, {'B', 'C'});
asserts.isequal(R.Properties.VariableNames, {'A', 'Var2'});
R = mergevars(TN, ["C", "A"]);
asserts.isequal(R.Properties.VariableNames, {'B', 'Var2'});
asserts.isequal(R.Var2, [5 1; 6 2]);
R = mergevars(TN, [2 2 3]);
asserts.isequal(R.Var2, [3 3 5; 4 4 6]);
R = mergevars(TN, []);
asserts.isequal(R, TN);
%=============================================================================
% name collisions: merged names are free again, remaining names are not
T = table([1; 2], [3; 4], [5; 6], 'VariableNames', {'X', 'Var1', 'Y'});
R = mergevars(T, [1 3]);
asserts.isequal(R.Properties.VariableNames, {'Var1_1', 'Var1'});
T = table([1; 2], [3; 4], [5; 6], 'VariableNames', {'X', 'Y', 'Var1'});
R = mergevars(T, [1 3]);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Y'});
T = table([1; 2], [3; 4], [5; 6], [7; 8], 'VariableNames', {'Var2', 'X', 'Y', 'Var2_1'});
R = mergevars(T, [2 3]);
asserts.isequal(R.Properties.VariableNames, {'Var2', 'Var2_2', 'Var2_1'});
%=============================================================================
% MergeAsTable
R = mergevars(T3, [2 3], 'MergeAsTable', true);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2'});
asserts.isequal(R.Var2.Properties.VariableNames, {'Var2', 'Var3'});
R = mergevars(TN, [1 3], 'MergeAsTable', true);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'B'});
asserts.isequal(R.Var1.Properties.VariableNames, {'A', 'C'});
R = mergevars(T4, [2 2], 'MergeAsTable', true);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2', 'Var3', 'Var4'});
asserts.isequal(R.Var2.Properties.VariableNames, {'Var2', 'Var2_1'});
%=============================================================================
% NewVariableName
R = mergevars(T4, [2 3], 'NewVariableName', "S");
asserts.isequal(R.Properties.VariableNames, {'Var1', 'S', 'Var4'});
R = mergevars(T4, [2 3], 'NewVariableName', 'Var3');
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var3', 'Var4'});
asserts.isequal(R.Var3, [3 5; 4 6]);
asserts.isequal(R.Var4, [7; 8]);
asserts.isequal(R.Var1, [1; 2]);
asserts.checkerror('mergevars(T3, [2 3], ''NewVariableName'', ''Var1'')', sprintf(_('Duplicate table variable name: ''%s''.'), 'Var1'), 'Nelson:table:duplicateVariableNames');
msg = 'Specify the new variable name as a string or a character vector.';
asserts.checkerror('mergevars(T4, [2 3], ''NewVariableName'', {''S''})', msg, 'Nelson:table:mergevars:InvalidNewVarName');
%=============================================================================
% timetable
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
R = mergevars(TT, [1 2]);
asserts.isequal(class(R), 'timetable');
asserts.isequal(R.Properties.VariableNames, {'Var1'});
TT = timetable(seconds([1; 2]), [1; 2], [3; 4], [5; 6]);
R = mergevars(TT, [2 3], 'MergeAsTable', true);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2'});
asserts.isequal(class(R.Var2), 'table');
asserts.isequal(R.Var2.Properties.VariableNames, {'Var2', 'Var3'});
%=============================================================================
