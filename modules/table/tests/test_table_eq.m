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
R = T1 == T1;
REF = table(Age == Age, Height == Height, Weight == Weight, BloodPressure == BloodPressure, 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R,REF);
%=============================================================================
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(Age, Height, Weight, BloodPressure);
R = T1 == 1;
REF = table(Age == 1, Height == 1, Weight == 1, BloodPressure == 1, 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R,REF);
%=============================================================================
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(Age, Height, Weight, BloodPressure);
R = 1 == T1;
REF = table(Age == 1, Height == 1, Weight == 1, BloodPressure == 1, 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R,REF);
%=============================================================================
