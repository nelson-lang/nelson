%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Portable runtime table compatibility scenario triggers a runtime crash.');
A = [28; 34; 22; 30];
H = [175; 160; 180; 165];
W = [70; 55; 80; 60];
N = {'John'; 'Alice'; 'Bob'; 'Diana'};
%=============================================================================
T = table(N, A, H, W);
asserts.isequal(T.Properties.VariableNames, {'N', 'A', 'H', 'W'});
%=============================================================================
T = table(N, A, H, W, 'VariableNames', {'Name', 'Age', 'Height', 'Weight'}, ...
  'RowNames', {'r1', 'r2', 'r3', 'r4'});
asserts.isequal(class(T), 'table');
asserts.istrue(istable(T));
asserts.isequal(size(T), [4 4]);
asserts.isequal(width(T), 4);
asserts.isequal(height(T), 4);
asserts.isequal(T.Properties.VariableNames, {'Name', 'Age', 'Height', 'Weight'});
asserts.isequal(T.Age, A);
asserts.isequal(T{:, {'Age', 'Height'}}, [A H]);
%=============================================================================
S = T({'r2', 'r1'}, {'Weight', 'Age'});
asserts.istrue(istable(S));
asserts.isequal(S.Properties.VariableNames, {'Weight', 'Age'});
asserts.isequal(S.Age, [34; 28]);
%=============================================================================
L = T([true false true false], {'Name', 'Age'});
asserts.isequal(height(L), 2);
T.Score = [10; 20; 30; 40];
asserts.isequal(T.Properties.VariableNames, {'Name', 'Age', 'Height', 'Weight', 'Score'});
T{1, 'Age'} = 29;
asserts.isequal(T.Age(1), 29);
T(:, 'Score') = [];
asserts.isfalse(any(strcmp(T.Properties.VariableNames, 'Score')));
T.Properties.VariableNames = {'Name', 'Years', 'Height', 'Weight'};
asserts.isequal(T.Years, [29; 34; 22; 30]);
%=============================================================================
TN = table(A, H, W, 'VariableNames', {'Age', 'Height', 'Weight'}, ...
  'RowNames', {'r1', 'r2', 'r3', 'r4'});
asserts.isequal(TN.Row, {'r1'; 'r2'; 'r3'; 'r4'});
TV = TN.Variables;
asserts.isequal(TV, [A H W]);
TN.Properties.DimensionNames = {'Obs', 'Vars'};
asserts.isequal(TN.Obs, {'r1'; 'r2'; 'r3'; 'r4'});
TV = TN.Vars;
asserts.isequal(TV, [A H W]);
%=============================================================================
U = table(Size =[2 2], VariableTypes ={'double', 'string'}, ...
  VariableNames ={'X', 'Y'}, RowNames ={'u1', 'u2'});
asserts.isequal(size(U), [2 2]);
asserts.isequal(U.Properties.VariableNames, {'X', 'Y'});
%=============================================================================
V = table([1; 2], [5; 6], 'VariableNames', {'A', 'C'});
V = addvars(V, [3; 4], 'NewVariableNames', {'B'}, 'Before', 'C');
asserts.isequal(V.Properties.VariableNames, {'A', 'B', 'C'});
V = addvars(V, [7; 8], 'NewVariableNames', {'First'}, 'Before', 1);
asserts.isequal(V.Properties.VariableNames, {'First', 'A', 'B', 'C'});
V = movevars(V, 'C', 'Before', 1);
asserts.isequal(V.Properties.VariableNames, {'C', 'First', 'A', 'B'});
V = renamevars(V, {'C'}, {'D'});
V = removevars(V, {'A'});
asserts.isequal(V.Properties.VariableNames, {'D', 'First', 'B'});
V = addprop(V, 'Source', 'table');
V.Properties.CustomProperties.Source = 'nelson';
asserts.isequal(V.Properties.CustomProperties.Source, 'nelson');
V = rmprop(V, 'Source');
asserts.isfalse(isfield(V.Properties.CustomProperties, 'Source'));
%=============================================================================
X = table([1; NaN; 3], logical([true; false; true]), 'VariableNames', {'A', 'B'});
X.Properties.VariableDescriptions = {'descA', 'descB'};
X.Properties.VariableUnits = {'m', ''};
Q = summary(X);
asserts.isequal(Q.A.Size, [3 1]);
asserts.isequal(Q.A.Type, 'double');
asserts.isequal(Q.A.Description, 'descA');
asserts.isequal(Q.A.Units, 'm');
asserts.isequal(Q.A.NumMissing, 1);
asserts.isequal(Q.A.Min, 1);
asserts.isequal(Q.A.Median, 2);
asserts.isequal(Q.A.Max, 3);
asserts.isequal(Q.A.Mean, 2);
asserts.isapprox(Q.A.Std, sqrt(2), 1e-12);
asserts.isequal(Q.B.True, 2);
asserts.isequal(Q.B.False, 1);
%=============================================================================
C = table2cell(T);
asserts.istrue(iscell(C));
asserts.isequal(size(C), [4 4]);
A2 = table2array(TN);
asserts.isequal(A2, [A H W]);
ST = table2struct(TN);
asserts.isequal(length(ST), 4);
asserts.isequal(ST(1).Age, 28);
%=============================================================================
filename = [tempname(), '.csv'];
writetable(TN, filename);
TR = readtable(filename);
asserts.isequal(TR.Properties.VariableNames, {'Age', 'Height', 'Weight'});
%=============================================================================
