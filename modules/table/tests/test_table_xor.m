%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(Age, Height, Weight, BloodPressure);
R = xor(T1,T1);
REF = table(xor(Age,Age),xor(Height,Height),xor(Weight,Weight),xor(BloodPressure,BloodPressure), 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R,REF);
%=============================================================================
A = table([true; false; true], [false; true; false], 'VariableNames', {'Var1', 'Var2'});
B = table([false; true; true], [true; false; true], 'VariableNames', {'Var1', 'Var2'});
R = xor(A,B);
REF = table(xor(A.Var1, B.Var1), xor(A.Var2, B.Var2), 'VariableNames', {'Var1', 'Var2'});
assert_isequal(R, REF);
%=============================================================================
A = table([true; false; true], [false; true; false], 'VariableNames', {'Var1', 'Var2'});
R = xor(A, 0);
REF = table(xor(A.Var1, 0), xor(A.Var2, 0), 'VariableNames', {'Var1', 'Var2'});
assert_isequal(R, REF);
%=============================================================================
