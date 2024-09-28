%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
LastName = {'Sanchez';'Johnson';'Lee';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
T = table(Age,Weight,Height,'RowNames',LastName);
%=============================================================================
% T.Age
assert_isequal(T.Age, [38;43;38;40;49]);
assert_isequal(T.Age(2:3), [43;38]);
%=============================================================================
% T brace indexing
assert_isequal(T{1, 1}, 38);
assert_isequal(T{1, 1:3}, [38   176    71]);
assert_isequal(T{1, 2}, 176);
assert_isequal(T{1, 3}, 71);
assert_isequal(T{2, 1}, 43);
assert_isequal(T{2, 2}, 163);
assert_isequal(T{2, 3}, 69);
REF = [38   176    71;
43   163    69;
38   131    64;
40   133    67;
49   119    64];
assert_isequal(T{:, :}, REF);
R = T{["Sanchez", "Lee"],2:3};
REF = [176    71;
131    64];
assert_isequal(R, REF);
%=============================================================================
% T parenthese indexing
LastName = {'Sanchez';'Johnson';'Lee';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
T = table(Age, Weight, Height, 'RowNames', LastName);
R = T('Lee', :);
assert_istrue(istable(R));
REF = table([38], [131], [64], 'VariableNames', {'Age', 'Weight', 'Height'}, 'RowNames', {'Lee'});
assert_isequal(R, REF);
%=============================================================================
R = T({'Lee', 'Diaz'}, :);
assert_istrue(istable(R));
REF = table([38;40], [131;133], [64;67], 'VariableNames', {'Age', 'Weight', 'Height'}, 'RowNames', {'Lee', 'Diaz'});
assert_isequal(R, REF);
%=============================================================================
R = T(1:2, {'Age', 'Weight'});
assert_istrue(istable(R));
assert_isequal(R.Row, {'Sanchez'; 'Johnson'});
assert_isequal(R.Age, [38;43]);
assert_isequal(R.Weight, [176;163]);
REF = table([38;43], [176;163], 'VariableNames', {'Age', 'Weight'}, 'RowNames', {'Sanchez', 'Johnson'});
assert_isequal(R, REF);
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
R = T.Properties.RowNames(2);
assert_isequal(R, {'B'});
%=============================================================================
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(Age,Smoker,Height,Weight,BloodPressure);
R = T{:,:};
REF = [ 38     1    71   176   124    93;
    43     0    69   163   109    77;
    38     1    64   131   125    83;
    40     0    67   133   117    75;
    49     1    64   119   122    80];
assert_isequal(R, REF);
%=============================================================================
Name = ["Alice"; "Bob"];
Age = [25; 30];
Height = [165; 180];
T = table(Name, Age, Height);
R = T{:,:};
REF = ["Alice"    "25"    "165";
      "Bob"    "30"    "180"];
assert_isequal(R, REF);
%=============================================================================
Name = ["Alice"; "Bob"];
Age = [25; 30];
Height = [165; 180];
T = table(Name, Age, Height);
R = T.Variables;
REF = ["Alice"    "25"    "165";
      "Bob"    "30"    "180"];
assert_isequal(R, REF);
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
R = T{:, 'name'};
REF = {'Harry';'Mark';'Steven'};
assert_isequal(R, REF);
%=============================================================================
