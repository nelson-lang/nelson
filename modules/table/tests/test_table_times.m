%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
R = T1 .* T1;
REF = table(Age .* Age, Height .* Height, Weight .* Weight, BloodPressure .* BloodPressure, 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R,REF);
%=============================================================================
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(Age, Height, Weight, BloodPressure);
R = T1 .* 2;
REF = table(Age .* 2, Height .* 2, Weight .* 2, BloodPressure .* 2, 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R,REF);
%=============================================================================
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(Age, Height, Weight, BloodPressure);
R = 2 .* T1;
REF = table(2 .* Age, 2 .* Height, 2 .* Weight, 2 .* BloodPressure, 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R,REF);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(LastName, Age, Height, Weight, BloodPressure);
msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), 'times', 'cell');
assert_checkerror('R = 1 .* T1;', msg);
%=============================================================================
