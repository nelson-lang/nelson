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
Smoker = [1;0;1;0;1];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2);
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_1.csv'];
R = readtable(csv_filename);
assert_isequal(R, REF)
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = {'1';'''0''';'''1''';'''0''';'1'};
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2);
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_2.csv'];
R = readtable(csv_filename);
assert_isequal(R, REF)
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = [1;NaN;NaN;0;1];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2);
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_3.csv'];
R = readtable(csv_filename);
assert_isequal(R, REF)
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = [1;NaN;NaN;0;1];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2, 'VariableNames', {'Last Name','Age','Smoker','Height','Weight','BloodPressure 1','BloodPressure 2'});
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_4.csv'];
R = readtable(csv_filename);
assert_isequal(R, REF)
%=============================================================================
