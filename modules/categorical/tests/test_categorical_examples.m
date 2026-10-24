%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Stations = ["S1" "S2" "S1" "S3" "S2"];
Stations = categorical(Stations);
asserts.isequal(categories(Stations), {'S1'; 'S2'; 'S3'});
TF = (Stations == "S2");
asserts.isequal(TF, [false true false false true]);
%=============================================================================
statesNE = ["MA" "ME" " CT" "VT" " ME " "NH" "VT" "MA" "NH" "CT" "RI "];
statesNE = categorical(statesNE);
asserts.isequal(categories(statesNE), {'CT'; 'MA'; 'ME'; 'NH'; 'RI'; 'VT'});
statesNE(12) = "ME";
asserts.isequal(cellstr(statesNE(12)), {'ME'});
statesNE(1:3) = categorical(["RI" "VT" "MA"]);
asserts.isequal(cellstr(statesNE(1:3)), {'RI', 'VT', 'MA'});
statesNE(2) = "";
asserts.istrue(isundefined(statesNE(2)));
%=============================================================================
A = ["red" "blue" "blue" "blue" "blue" "red"];
valueset = ["blue" "red" "green"];
C = categorical(A, valueset);
asserts.isequal(categories(C), {'blue'; 'red'; 'green'});
asserts.isequal(cellstr(C), {'red', 'blue', 'blue', 'blue', 'blue', 'red'});
%=============================================================================
A = [1 3 2; 2 1 3; 3 1 2];
C = categorical(A, [1 2 3], ["red" "green" "blue"]);
asserts.isequal(categories(C), {'red'; 'green'; 'blue'});
TF = (C == "red");
asserts.isequal(TF, [true false false; false true false; false true false]);
asserts.isequal(cellstr(C(TF)), {'red'; 'red'; 'red'});
%=============================================================================
A = ["hi" "lo" "" "lo" "lo" "hi"];
C = categorical(A);
asserts.isequal(categories(C), {'hi'; 'lo'});
asserts.isequal(isundefined(C), [false false true false false false]);
C = categorical(A, ["lo" "hi" ""], ["lo" "hi" "INDEF"]);
asserts.isequal(categories(C), {'lo'; 'hi'; 'INDEF'});
asserts.isequal(cellstr(C), {'hi', 'lo', 'INDEF', 'lo', 'lo', 'hi'});
%=============================================================================
A = [3 2; 3 3; 3 2; 2 1; 3 2];
valueset = [1 2 3];
catnames = ["child" "adult" "senior"];
C = categorical(A, valueset, catnames, 'Ordinal', true);
TF = C > "adult";
asserts.isequal(TF, [true false; true true; true false; false false; true false]);
asserts.isequal(cellstr(C(TF)), {'senior'; 'senior'; 'senior'; 'senior'; 'senior'});
AllSizes = ["medium" "large" "small" "small" "medium" "large" "medium" "small"];
valueset = ["small" "medium" "large"];
sizeOrd = categorical(AllSizes, valueset, Ordinal= true);
asserts.istrue(isordinal(sizeOrd));
asserts.isequal(categories(sizeOrd), {'small'; 'medium'; 'large'});
asserts.isequal(sizeOrd < "large", [true false true true true false true true]);
%=============================================================================
A = NaN(2, 4);
C = categorical(A);
asserts.isequal(categories(C), cell(0, 1));
C = addcats(C, ["small" "medium" "large"]);
asserts.isequal(categories(C), {'small'; 'medium'; 'large'});
C(1) = "medium";
C(8) = "small";
C(3:5) = "large";
asserts.isequal(cellstr(C), {'medium', 'large', 'large', '<undefined>'; ...
  '<undefined>', 'large', '<undefined>', 'small'});
%=============================================================================
bloodGroups = categorical(["A" "AB" "O" "O" "A" "A"], ["A" "B" "AB" "O"]);
Rhfactors = categorical(["+" "+" "-" "-" "+" "+"]);
bloodTypes = bloodGroups .* Rhfactors;
asserts.isequal(cellstr(bloodTypes), {'A +', 'AB +', 'O -', 'O -', 'A +', 'A +'});
asserts.isequal(categories(bloodTypes), {'A +'; 'A -'; 'B +'; 'B -'; ...
  'AB +'; 'AB -'; 'O +'; 'O -'});
%=============================================================================
