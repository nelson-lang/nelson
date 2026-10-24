%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
LastName = {'Sanchez'; 'Johnson'; 'Lee'; 'Diaz'; 'Brown'};
Age = [38; 43; 38; 40; 49];
Height = [71; 69; 64; 67; 64];
Weight = [176; 163; 131; 133; 119];
T = table(Age, Weight, Height, 'RowNames', LastName);
%=============================================================================
% T.Age
asserts.isequal(T.Age, [38; 43; 38; 40; 49]);
asserts.isequal(T.Age(2:3), [43; 38]);
%=============================================================================
% T brace indexing
asserts.isequal(T{1, 1}, 38);
asserts.isequal(T{1, 1:3}, [38 176 71]);
asserts.isequal(T{1, 2}, 176);
asserts.isequal(T{1, 3}, 71);
asserts.isequal(T{2, 1}, 43);
asserts.isequal(T{2, 2}, 163);
asserts.isequal(T{2, 3}, 69);
REF = [38 176 71;
  43 163 69;
  38 131 64;
  40 133 67;
  49 119 64];
asserts.isequal(T{:, :}, REF);
R = T{["Sanchez", "Lee"], 2:3};
REF = [176 71;
  131 64];
asserts.isequal(R, REF);
%=============================================================================
% T parentheses indexing
LastName = {'Sanchez'; 'Johnson'; 'Lee'; 'Diaz'; 'Brown'};
Age = [38; 43; 38; 40; 49];
Height = [71; 69; 64; 67; 64];
Weight = [176; 163; 131; 133; 119];
T = table(Age, Weight, Height, 'RowNames', LastName);
R = T('Lee', :);
asserts.istrue(istable(R));
REF = table([38], [131], [64], 'VariableNames', {'Age', 'Weight', 'Height'}, 'RowNames', {'Lee'});
asserts.isequal(R, REF);
%=============================================================================
R = T({'Lee', 'Diaz'}, :);
asserts.istrue(istable(R));
REF = table([38; 40], [131; 133], [64; 67], 'VariableNames', {'Age', 'Weight', 'Height'}, 'RowNames', {'Lee', 'Diaz'});
asserts.isequal(R, REF);
%=============================================================================
R = T(1:2, {'Age', 'Weight'});
asserts.istrue(istable(R));
asserts.isequal(R.Row, {'Sanchez'; 'Johnson'});
asserts.isequal(R.Age, [38; 43]);
asserts.isequal(R.Weight, [176; 163]);
REF = table([38; 43], [176; 163], 'VariableNames', {'Age', 'Weight'}, 'RowNames', {'Sanchez', 'Johnson'});
asserts.isequal(R, REF);
%=============================================================================
name = {'Harry'; 'Mark'; 'Steven'};
age = [23; 54; 13];
Employed = logical([1; 0; 1]);
T = table(name, age, Employed, 'RowNames', {'A'; 'B'; 'C'});
R = T.Properties.RowNames(2);
asserts.isequal(R, {'B'});
%=============================================================================
Age = [38; 43; 38; 40; 49];
Smoker = logical([1; 0; 1; 0; 1]);
Height = [71; 69; 64; 67; 64];
Weight = [176; 163; 131; 133; 119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(Age, Smoker, Height, Weight, BloodPressure);
R = T{:, :};
REF = [38 1 71 176 124 93;
    43 0 69 163 109 77;
    38 1 64 131 125 83;
    40 0 67 133 117 75;
    49 1 64 119 122 80];
asserts.isequal(R, REF);
%=============================================================================
Name = ["Alice"; "Bob"];
Age = [25; 30];
Height = [165; 180];
T = table(Name, Age, Height);
R = T{:, :};
REF = ["Alice" "25" "165";
      "Bob" "30" "180"];
asserts.isequal(R, REF);
%=============================================================================
Name = ["Alice"; "Bob"];
Age = [25; 30];
Height = [165; 180];
T = table(Name, Age, Height);
R = T.Variables;
REF = ["Alice" "25" "165";
      "Bob" "30" "180"];
asserts.isequal(R, REF);
%=============================================================================
name = {'Harry'; 'Mark'; 'Steven'};
age = [23; 54; 13];
Employed = logical([1; 0; 1]);
T = table(name, age, Employed, 'RowNames', {'A'; 'B'; 'C'});
R = T{:, 'name'};
REF = {'Harry'; 'Mark'; 'Steven'};
asserts.isequal(R, REF);
%=============================================================================
Names = {'John'; 'Alice'; 'Bob'; 'Diana'};
Age = [28; 34; 22; 30];
Height = [175; 160; 180; 165];
Weight = [70; 55; 80; 60];
T = table(Names, Age, Height, Weight);
R = T{'Alice', 'Age'};
asserts.isequal(R, 34);
R = T{'Alice', 2};
asserts.isequal(R, 34);
R = T({'Alice', 'Bob'}, {'Age', 'Height'});
REF = table([34; 22], [160; 180], 'VariableNames', {'Age', 'Height'});
asserts.isequal(R, REF);
%=============================================================================
Region = {'north'; 'south'; 'west'};
Loss = [10; 20; 30];
RestorationTime = datetime(2026, 1, [3; 1; 2]);
T = table(Region, Loss, RestorationTime);
R = T(1:3, :);
asserts.isequal(R.Region, Region);
asserts.isequal(R.Loss, Loss);
asserts.isequal(R.RestorationTime, RestorationTime);
%=============================================================================
