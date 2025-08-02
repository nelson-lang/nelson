%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table(["Y";"Y";"N";"N";"N"],[38;43;38;40;49], [124 93;109 77; 125 83; 117 75; 122 80], 'VariableNames', ["Smoker" "Age" "BloodPressure"], 'RowNames',["Chang" "Brown" "Ruiz" "Lee" "Garcia"]);
R1 = table2struct(T);
R2 = table2struct(T, 'toScalar', true);
R3 = table2struct(T, 'toScalar', false);
assert_isequal(R1, R3);
REF = struct();
REF(1, 1).Smoker = "Y";
REF(1, 1).Age = 38;
REF(1, 1).BloodPressure = [124 93];
REF(2, 1).Smoker = "Y";
REF(2, 1).Age = 43;
REF(2, 1).BloodPressure = [109 77];
REF(3, 1).Smoker = "N";
REF(3, 1).Age = 38;
REF(3, 1).BloodPressure = [125 83];
REF(4, 1).Smoker = "N";
REF(4, 1).Age = 40;
REF(4, 1).BloodPressure = [117 75];              
REF(5, 1).Smoker = "N";
REF(5, 1).Age = 49;
REF(5, 1).BloodPressure = [122 80];
assert_isequal(R1, REF);
%=============================================================================
T = table([10;20;30],{'M';'F';'F'},'VariableNames',{'Age','Gender'},'RowNames',{'P1','P2','P3'});
R1 = table2struct(T);
R2 = table2struct(T, 'toScalar', true);
R3 = table2struct(T, 'toScalar', false);
assert_isequal(R1, R3);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
R1 = table2struct(T);
R2 = table2struct(T, 'toScalar', true);
R3 = table2struct(T, 'toScalar', false);
assert_isequal(R1, R3);
REF = struct();
REF(1, 1).LastName = {'Sanchez'};
REF(1, 1).Age = 38;
REF(1, 1).Smoker = true;
REF(1, 1).Height = 71;
REF(1, 1).Weight = 176;
REF(1, 1).BloodPressure = [124 93];
REF(2, 1).LastName = {'Johnson'};
REF(2, 1).Age = 43;
REF(2, 1).Smoker = false;
REF(2, 1).Height = 69;
REF(2, 1).Weight = 163;
REF(2, 1).BloodPressure = [109 77];
REF(3, 1).LastName = {'Li'};
REF(3, 1).Age = 38;
REF(3, 1).Smoker = true;
REF(3, 1).Height = 64;
REF(3, 1).Weight = 131;
REF(3, 1).BloodPressure = [125 83];
REF(4, 1).LastName = {'Diaz'};
REF(4, 1).Age = 40;
REF(4, 1).Smoker = false;
REF(4, 1).Height = 67;
REF(4, 1).Weight = 133;
REF(4, 1).BloodPressure = [117 75];
REF(5, 1).LastName = {'Brown'};
REF(5, 1).Age = 49;
REF(5, 1).Smoker = true;
REF(5, 1).Height = 64;
REF(5, 1).Weight = 119;
REF(5, 1).BloodPressure = [122 80];
assert_isequal(R1, REF);
%=============================================================================
T = table();
R1 = table2struct(T);
R2 = table2struct(T, 'toScalar', true);
R3 = table2struct(T, 'toScalar', false);
assert_isequal(R1, R3);
assert_isequal(R2, struct());
REF = struct(zeros(0, 1));
assert_isequal(R1, REF);
%=============================================================================
LastName = ["Sanchez";"Johnson";"Li";"Diaz";"Brown"];
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
R1 = table2struct(T);
R2 = table2struct(T, 'toScalar', true);
R3 = table2struct(T, 'toScalar', false);
assert_isequal(R1, R3);
REF = struct();
REF(1, 1).LastName = "Sanchez";
REF(1, 1).Age = 38;
REF(1, 1).Smoker = true;
REF(1, 1).Height = 71;
REF(1, 1).Weight = 176;
REF(1, 1).BloodPressure = [124 93];
REF(2, 1).LastName = "Johnson";
REF(2, 1).Age = 43;
REF(2, 1).Smoker = false;
REF(2, 1).Height = 69;
REF(2, 1).Weight = 163;
REF(2, 1).BloodPressure = [109 77];
REF(3, 1).LastName = "Li";
REF(3, 1).Age = 38;
REF(3, 1).Smoker = true;
REF(3, 1).Height = 64;
REF(3, 1).Weight = 131;
REF(3, 1).BloodPressure = [125 83];
REF(4, 1).LastName = "Diaz";
REF(4, 1).Age = 40;
REF(4, 1).Smoker = false;
REF(4, 1).Height = 67;
REF(4, 1).Weight = 133;
REF(4, 1).BloodPressure = [117 75];
REF(5, 1).LastName = "Brown";
REF(5, 1).Age = 49;
REF(5, 1).Smoker = true;
REF(5, 1).Height = 64;
REF(5, 1).Weight = 119;
REF(5, 1).BloodPressure = [122 80];
assert_isequal(R1, REF);
%=============================================================================
