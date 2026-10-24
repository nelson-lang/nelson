%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical(["plane" "car" "train" "car" "plane"]);
C = categories(A);
asserts.isequal(C, {'car'; 'plane'; 'train'});
C = categories(A, OutputType= "categorical");
asserts.istrue(iscategorical(C));
asserts.isequal(cellstr(C), {'car'; 'plane'; 'train'});
C = categories(A, OutputType= "string");
asserts.isequal(cellstr(C), {'car'; 'plane'; 'train'});
%=============================================================================
A = categorical(["medium" "large"; "small" "xlarge"; "large" "medium"], ...
  ["small" "medium" "large" "xlarge"], Ordinal= true);
B = addcats(A, "xsmall", Before= "small");
asserts.isequal(categories(B), {'xsmall'; 'small'; 'medium'; 'large'; 'xlarge'});
asserts.isequal(cellstr(B), {'medium', 'large'; 'small', 'xlarge'; 'large', 'medium'});
%=============================================================================
A = categorical(["red" "blue" "green" "blue" "red"], ["red" "green" "blue"]);
B = countcats(A);
asserts.isequal(B, [2 1 2]);
sizes = categorical(["medium", "large", "small", missing, "medium"]);
asserts.isequal(categories(sizes), {'large'; 'medium'; 'small'});
asserts.isequal(isundefined(sizes), [false false false true false]);
asserts.isequal(countcats(sizes), [1 2 1]);
age = [4 12 18 32 48 NaN];
ageGroup = discretize(age, [0 13 20 65], 'categorical', {'child', 'teen', 'adult'});
asserts.isequal(categories(ageGroup), {'child'; 'teen'; 'adult'});
asserts.isequal(cellstr(ageGroup), {'child', 'child', 'teen', 'adult', 'adult', '<undefined>'});
valueset = 1:3;
catnames = ["red" "green" "blue"];
A = categorical([1 3; 2 1; 3 1], valueset, catnames);
B = countcats(A);
asserts.isequal(B, [1 2; 1 0; 1 1]);
B = countcats(A, 2);
asserts.isequal(B, [1 0 1; 1 1 0; 1 0 1]);
N = reshape(categorical({'a', 'b', 'a', ''}), [1 2 2]);
asserts.checkerror('countcats(N, 3)', _('Dimension argument is not supported for categorical countcats.'));
asserts.checkerror('countcats(N, 3)', _('Dimension argument is not supported for categorical countcats.'), 'Nelson:countcats:dimensionNotSupported');
A = categorical([1; 3; 2; 1; 3; 1], valueset, catnames);
A = removecats(A, "blue");
B = countcats(A);
asserts.isequal(B, [3; 1]);
asserts.isequal(sum(isundefined(A)), 2);
%=============================================================================
A = categorical(["MA"; "ME"; "CT"; "VT"; "ME"; "NH"; "VT"; "MA"; "NH"; "CT"; "RI"]);
newnames = ["Connecticut" "Massachusetts" "Maine" "New Hampshire" "Rhode Island" "Vermont"];
B = renamecats(A, newnames);
asserts.isequal(categories(B), {'Connecticut'; 'Massachusetts'; 'Maine'; ...
  'New Hampshire'; 'Rhode Island'; 'Vermont'});
A = categorical(["purple" "blue" "purple" "red" "red" "blue"]);
B = renamecats(A, "purple", "violet");
asserts.isequal(categories(B), {'blue'; 'violet'; 'red'});
asserts.isequal(cellstr(B), {'violet', 'blue', 'violet', 'red', 'red', 'blue'});
%=============================================================================
A = categorical(["red" "blue" "blue" "blue" "red" "blue"], ...
  ["black" "blue" "red" "green"]);
B = removecats(A);
asserts.isequal(categories(B), {'blue'; 'red'});
asserts.isequal(cellstr(B), {'red', 'blue', 'blue', 'blue', 'red', 'blue'});
A = categorical(["plane" "car" "train" "car" "plane" "car"]);
B = removecats(A, "car");
asserts.isequal(categories(B), {'plane'; 'train'});
asserts.isequal(cellstr(B), {'plane', '<undefined>', 'train', '<undefined>', 'plane', '<undefined>'});
%=============================================================================
A = categorical(["pink" "blue" "pink" "red" "blue" "red"]);
B = mergecats(A, ["red" "pink"]);
asserts.isequal(categories(B), {'blue'; 'red'});
asserts.isequal(cellstr(B), {'red', 'blue', 'red', 'red', 'blue', 'red'});
A = categorical(["shirt" "pants" "shoes" "shirt" "dress" "belt"]);
B = mergecats(A, ["belt" "shoes"], "other");
asserts.isequal(categories(B), {'other'; 'dress'; 'pants'; 'shirt'});
asserts.isequal(cellstr(B), {'shirt', 'pants', 'other', 'shirt', 'dress', 'other'});
A = categorical([1 2 3 2 1], 1:3, ["poor" "fair" "good"], Ordinal= true);
B = mergecats(A, ["fair" "poor"], "bad");
asserts.isequal(categories(B), {'bad'; 'good'});
asserts.isequal(cellstr(B), {'bad', 'bad', 'good', 'bad', 'bad'});
%=============================================================================
X = categorical(["Frog" "Cat" "Cat" "Ant" "Frog"]);
Y = categorical(["Deer" "Bear" "Eagle" "Deer"]);
A = [X Y];
B = reordercats(A);
asserts.isequal(categories(B), {'Ant'; 'Bear'; 'Cat'; 'Deer'; 'Eagle'; 'Frog'});
A = categorical(["red" "green" "blue" "red" "green" "red" "blue" "blue"]);
B = reordercats(A, ["red" "green" "blue"]);
asserts.isequal(categories(B), {'red'; 'green'; 'blue'});
A = categorical(["plane" "car" "train" "car" "plane" "car"], ...
  ["car" "train" "plane"], Ordinal= true);
B = reordercats(A, ["train" "car" "plane"]);
asserts.isequal(cellstr(B(B > "car")), {'plane', 'plane'});
A = categorical(["plane" "car" "train" "car" "car" "plane" "car"]);
B = countcats(A);
[~, neworder] = sort(B);
D = reordercats(A, neworder);
asserts.isequal(categories(D), {'train'; 'plane'; 'car'});
%=============================================================================
A = categorical(["blue" "black" "red" "red" "blue" "black" "black" "red" "blue"]);
B = setcats(A, ["red" "black"]);
asserts.isequal(cellstr(B), {'<undefined>', 'black', 'red', 'red', '<undefined>', ...
  'black', 'black', 'red', '<undefined>'});
B = setcats(A, ["red" "pink" "blue"]);
B(1) = "pink";
asserts.isequal(cellstr(B(1:4)), {'pink', '<undefined>', 'red', 'red'});
%=============================================================================
A = categorical(["shirt" "pants"; "pants" "hat"; "shirt" "pants"]);
catnames = ["shirt" "pants" "socks" "shoes"];
tf = iscategory(A, catnames);
asserts.isequal(tf, [true true false false]);
data = ["plane" "car" "train" "car" "plane"];
categoriesOfData = ["boat" "car" "plane" "train"];
A = categorical(data, categoriesOfData);
tf = iscategory(A, "boat");
asserts.istrue(tf);
%=============================================================================
