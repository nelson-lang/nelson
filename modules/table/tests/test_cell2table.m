%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
C = {5, 'cereal', 110, 'C+'; 12, 'pizza', 140, 'B'; 23, 'salmon', 367, 'A'; 2, 'cookies', 160, 'D'};
T = cell2table(C);
P = properties(T);
REF_P = {'C1'; 'C2'; 'C3'; 'C4'; 'Properties'; 'Row'; 'Variables' };
assert_isequal(P, REF_P);
assert_isequal(T.C1, [5; 12; 23; 2]);
assert_isequal(T.C2, {'cereal'; 'pizza'; 'salmon'; 'cookies'});
assert_isequal(T.C3, [110; 140; 367; 160]);
assert_isequal(T.C4, {'C+'; 'B'; 'A'; 'D'});
%=============================================================================
C = {5, 'cereal', 110, 'C+'; 12, 'pizza', 140, 'B'; 23, 'salmon', 367, 'A'; 2, 'cookies', 160, 'D'};
T = cell2table(C, 'VariableNames', {'Calories', 'Food', 'Price', 'NutritionGrade'});
P = properties(T);
REF_P = {'Calories'; 'Food'; 'Price'; 'NutritionGrade'; 'Properties'; 'Row'; 'Variables' };
assert_isequal(P, REF_P);
assert_isequal(T.Calories, [5; 12; 23; 2]);
assert_isequal(T.Food, {'cereal'; 'pizza'; 'salmon'; 'cookies'});
assert_isequal(T.Price, [110; 140; 367; 160]);
assert_isequal(T.NutritionGrade, {'C+'; 'B'; 'A'; 'D'});
%=============================================================================
D = {[1 2] 3; [4 5] 6};
T = cell2table(D);
P = properties(T);
REF_P = {'D1'; 'D2'; 'Properties'; 'Row'; 'Variables' };
assert_isequal(P, REF_P);
assert_isequal(T.D1, [1 2; 4 5]);
assert_isequal(T.D2, [3; 6]);
%=============================================================================
C = {[1 2] 3; 4 5};
T = cell2table(C);
P = properties(T);
REF_P = {'C1'; 'C2'; 'Properties'; 'Row'; 'Variables' };
assert_isequal(P, REF_P);
assert_isequal(T.C1, {[1 2]; 4});
assert_isequal(T.C2, [3; 5]);
%=============================================================================
ce = {"Chang", "Y", 124, 93;
"Brown", "N", 122, 80;
"Ruiz", "Y", 130, 92};
T = cell2table(ce);
assert_isequal(T.ce1, ["Chang";"Brown";"Ruiz"]);
assert_isequal(T.ce2, ["Y";"N";"Y"]);
assert_isequal(T.ce3, [124;122;130]);
assert_isequal(T.ce4, [93;80;92]);
%=============================================================================
ce = {'A', 'B', 'C'};
variableNames = {'Var1', 'Var2', 'Var3'};
T = cell2table(ce, 'VariableNames', variableNames);
assert_isequal(T{1, 1}, 'A');
assert_isequal(T{1, 2}, 'B');
assert_isequal(T{1, 3}, 'C');
assert_isequal(T.Var1, 'A');
assert_isequal(T.Var2, 'B');
assert_isequal(T.Var3, 'C');
%=============================================================================
C = {'John', 28, true; 'Alice', 35, false; 'Bob', 42, true};
T = cell2table(C);
assert_isequal(T.C3, [true; false; true]);
%=============================================================================
