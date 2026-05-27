%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [28; 34; 22; 30];
H = [175; 160; 180; 165];
W = [70; 55; 80; 60];
N = {'John'; 'Alice'; 'Bob'; 'Diana'};
%=============================================================================
T = table(N, A, H, W);
assert_isequal(T.Properties.VariableNames, {'N', 'A', 'H', 'W'});
%=============================================================================
T = table(N, A, H, W, 'VariableNames', {'Name', 'Age', 'Height', 'Weight'}, ...
  'RowNames', {'r1', 'r2', 'r3', 'r4'});
assert_isequal(class(T), 'table');
assert_istrue(istable(T));
assert_isequal(size(T), [4 4]);
assert_isequal(width(T), 4);
assert_isequal(height(T), 4);
assert_isequal(T.Properties.VariableNames, {'Name', 'Age', 'Height', 'Weight'});
assert_isequal(T.Age, A);
assert_isequal(T{:, {'Age', 'Height'}}, [A H]);
%=============================================================================
S = T({'r2', 'r1'}, {'Weight', 'Age'});
assert_istrue(istable(S));
assert_isequal(S.Properties.VariableNames, {'Weight', 'Age'});
assert_isequal(S.Age, [34; 28]);
%=============================================================================
L = T([true false true false], {'Name', 'Age'});
assert_isequal(height(L), 2);
T.Score = [10; 20; 30; 40];
assert_isequal(T.Properties.VariableNames, {'Name', 'Age', 'Height', 'Weight', 'Score'});
T{1, 'Age'} = 29;
assert_isequal(T.Age(1), 29);
T(:, 'Score') = [];
assert_isfalse(any(strcmp(T.Properties.VariableNames, 'Score')));
T.Properties.VariableNames = {'Name', 'Years', 'Height', 'Weight'};
assert_isequal(T.Years, [29; 34; 22; 30]);
%=============================================================================
TN = table(A, H, W, 'VariableNames', {'Age', 'Height', 'Weight'}, ...
  'RowNames', {'r1', 'r2', 'r3', 'r4'});
assert_isequal(TN.Row, {'r1'; 'r2'; 'r3'; 'r4'});
TV = TN.Variables;
assert_isequal(TV, [A H W]);
TN.Properties.DimensionNames = {'Obs', 'Vars'};
assert_isequal(TN.Obs, {'r1'; 'r2'; 'r3'; 'r4'});
TV = TN.Vars;
assert_isequal(TV, [A H W]);
%=============================================================================
U = table(Size = [2 2], VariableTypes = {'double', 'string'}, ...
  VariableNames = {'X', 'Y'}, RowNames = {'u1', 'u2'});
assert_isequal(size(U), [2 2]);
assert_isequal(U.Properties.VariableNames, {'X', 'Y'});
%=============================================================================
V = table([1; 2], [5; 6], 'VariableNames', {'A', 'C'});
V = addvars(V, [3; 4], 'NewVariableNames', {'B'}, 'Before', 'C');
assert_isequal(V.Properties.VariableNames, {'A', 'B', 'C'});
V = addvars(V, [7; 8], 'NewVariableNames', {'First'}, 'Before', 1);
assert_isequal(V.Properties.VariableNames, {'First', 'A', 'B', 'C'});
V = movevars(V, 'C', 'Before', 1);
assert_isequal(V.Properties.VariableNames, {'C', 'First', 'A', 'B'});
V = renamevars(V, {'C'}, {'D'});
V = removevars(V, {'A'});
assert_isequal(V.Properties.VariableNames, {'D', 'First', 'B'});
V = addprop(V, 'Source', 'table');
V.Properties.CustomProperties.Source = 'nelson';
assert_isequal(V.Properties.CustomProperties.Source, 'nelson');
V = rmprop(V, 'Source');
assert_isfalse(isfield(V.Properties.CustomProperties, 'Source'));
%=============================================================================
X = table([1; NaN; 3], logical([true; false; true]), 'VariableNames', {'A', 'B'});
X.Properties.VariableDescriptions = {'descA', 'descB'};
X.Properties.VariableUnits = {'m', ''};
Q = summary(X);
assert_isequal(Q.A.Size, [3 1]);
assert_isequal(Q.A.Type, 'double');
assert_isequal(Q.A.Description, 'descA');
assert_isequal(Q.A.Units, 'm');
assert_isequal(Q.A.NumMissing, 1);
assert_isequal(Q.A.Min, 1);
assert_isequal(Q.A.Median, 2);
assert_isequal(Q.A.Max, 3);
assert_isequal(Q.A.Mean, 2);
assert_isapprox(Q.A.Std, sqrt(2), 1e-12);
assert_isequal(Q.B.True, 2);
assert_isequal(Q.B.False, 1);
%=============================================================================
C = table2cell(T);
assert_istrue(iscell(C));
assert_isequal(size(C), [4 4]);
A2 = table2array(TN);
assert_isequal(A2, [A H W]);
ST = table2struct(TN);
assert_isequal(length(ST), 4);
assert_isequal(ST(1).Age, 28);
%=============================================================================
filename = [tempname(), '.csv'];
writetable(TN, filename);
TR = readtable(filename);
assert_isequal(TR.Properties.VariableNames, {'Age', 'Height', 'Weight'});
%=============================================================================
