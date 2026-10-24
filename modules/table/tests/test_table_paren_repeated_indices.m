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
A = [1; 2]; B = [3; 4]; C = [5; 6];
T = table(A, B, C);
%=============================================================================
% a repeated variable keeps its data under a unique name
R = T(:, [2 2]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1'});
asserts.isequal(R.B, B);
asserts.isequal(R.B_1, B);
asserts.isequal(size(R), [2 2]);
R = T(:, {'B', 'B'});
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1'});
R = T(:, [2 2 2]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1', 'B_2'});
R = T(:, [2 1 2 1]);
asserts.isequal(R.Properties.VariableNames, {'B', 'A', 'B_1', 'A_1'});
asserts.isequal(R.A_1, A);
R = T(1, [2 2]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1'});
asserts.isequal(R.B_1, 3);
asserts.isequal(T{:, [2 2]}, [B, B]);
%=============================================================================
% the suffix skips a name already selected
B_1 = [7; 8];
T2 = table(A, B, B_1);
R = T2(:, [2 2 3]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_2', 'B_1'});
asserts.isequal(R.B_1, B_1);
asserts.isequal(R.B_2, B);
R = T2(:, [2 2]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1'});
asserts.isequal(R.B_1, B);
R = T2(:, [3 2 2]);
asserts.isequal(R.Properties.VariableNames, {'B_1', 'B', 'B_2'});
%=============================================================================
% units and descriptions follow the selected variables
TU = T;
TU.Properties.VariableUnits = {'ua', 'ub', 'uc'};
TU.Properties.VariableDescriptions = {'da', 'db', 'dc'};
R = TU(:, [3 2]);
asserts.isequal(R.Properties.VariableUnits, {'uc', 'ub'});
asserts.isequal(R.Properties.VariableDescriptions, {'dc', 'db'});
R = TU(:, [2 2]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1'});
asserts.isequal(R.Properties.VariableUnits, {'ub', 'ub'});
asserts.isequal(R.Properties.VariableDescriptions, {'db', 'db'});
%=============================================================================
% repeated rows get unique row names
T3 = table(A, B, 'RowNames', {'r1', 'r2'});
R = T3(:, [2 2]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1'});
asserts.isequal(R.Properties.RowNames, {'r1'; 'r2'});
R = T3([1 1], :);
asserts.isequal(R.Properties.RowNames, {'r1'; 'r1_1'});
asserts.isequal(R.A, [1; 1]);
R = T3([1 1 2 1], [1 1]);
asserts.isequal(R.Properties.RowNames, {'r1'; 'r1_1'; 'r2'; 'r1_2'});
asserts.isequal(R.Properties.VariableNames, {'A', 'A_1'});
%=============================================================================
% timetable and eventtable
TT = timetable(seconds([1; 2]), A, B);
R = TT(:, [2 2]);
asserts.isequal(R.Properties.VariableNames, {'B', 'B_1'});
asserts.isequal(R.B_1, B);
R = TT([1 1], :);
asserts.isequal(R.Time, seconds([1; 1]));
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
ET.X = [1; 2];
R = ET(:, [2 2]);
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Properties.VariableNames, {'X', 'X_1'});
%=============================================================================
M = mergevars(T, [2 2], 'MergeAsTable', true);
asserts.isequal(M.Properties.VariableNames, {'A', 'Var2', 'C'});
asserts.isequal(M.Var2.Properties.VariableNames, {'B', 'B_1'});
%=============================================================================
