%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
T2 = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
R  = renamevars(T, 'LastName', 'Name');
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure, 'VariableNames', {'Name', 'Age', 'Smoker', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R, REF);
assert_isequal(T, T2);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure)
T.Properties.VariableNames = {'N1', 'A2', 'S3', 'H4', 'W5', 'B6'};
T_REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure, 'VariableNames', {'N1', 'A2', 'S3', 'H4', 'W5', 'B6'});
assert_isequal(T, T_REF);
%=============================================================================
