%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% text before or after a categorical array becomes categorical; the categories
% of the first categorical operand come first, then new values in order
a = categorical({'a'});
C = ["z" a];
assert_isequal(class(C), 'categorical');
assert_isequal(size(C), [1 2]);
assert_isequal(cellstr(C), {'z', 'a'});
assert_isequal(categories(C), {'a'; 'z'});
C = [a "z"];
assert_isequal(cellstr(C), {'a', 'z'});
assert_isequal(categories(C), {'a'; 'z'});
C = ["z"; a];
assert_isequal(size(C), [2 1]);
assert_isequal(categories(C), {'a'; 'z'});
C = horzcat("z", a);
assert_isequal(cellstr(C), {'z', 'a'});
C = cat(1, "z", a);
assert_isequal(size(C), [2 1]);
C = cat(3, "z", a);
assert_isequal(size(C), [1 1 2]);
C = ["z" "y" a];
assert_isequal(categories(C), {'a'; 'z'; 'y'});
C = ["a" a];
assert_isequal(categories(C), {'a'});
C = [categorical({'b'}) "z" a];
assert_isequal(categories(C), {'b'; 'z'; 'a'});
%=============================================================================
% character vectors and cell arrays of character vectors
C = ['z' a];
assert_isequal(categories(C), {'a'; 'z'});
C = ['zz'; 'yy'; a];
assert_isequal(size(C), [3 1]);
assert_isequal(categories(C), {'a'; 'zz'; 'yy'});
C = [{'z'} a];
assert_isequal(categories(C), {'a'; 'z'});
C = [{'x'; 'y'}; a];
assert_isequal(size(C), [3 1]);
assert_isequal(categories(C), {'a'; 'x'; 'y'});
C = ['a b' a];
assert_isequal(categories(C), {'a'; 'a b'});
%=============================================================================
% empty text and missing values are undefined; 0-by-0 empties are skipped
C = ["" a];
assert_isequal(isundefined(C), [true false]);
C = [{''} a];
assert_isequal(isundefined(C), [true false]);
C = [a string(missing)];
assert_isequal(isundefined(C), [false true]);
assert_isequal(categories(C), {'a'});
C = ["z" missing a];
assert_isequal(isundefined(C), [false true false]);
C = [[] a];
assert_isequal(size(C), [1 1]);
C = [{} a];
assert_isequal(size(C), [1 1]);
%=============================================================================
% ordinal and protected arrays accept only their own categories
o = categorical({'lo'; 'hi'}, {'lo', 'hi'}, 'Ordinal', true);
C = ["lo"; o];
assert_isequal(size(C), [3 1]);
assert_istrue(isordinal(C));
C = [missing; o];
assert_isequal(isundefined(C), [true; false; false]);
asserts.checkerror('["mid"; o]', 'Ordinal arrays must have the same categories, including their order.');
p = categorical({'a'}, {'a'}, 'Protected', true);
C = [p "a"];
assert_isequal(size(C), [1 2]);
msg = 'One or more of the arrays is categorical and either ordinal or protected, and the arrays do not all have the same sets of categories.  Use ADDCATS to add new categories.';
asserts.checkerror('["z" p]', msg);
%=============================================================================
% other classes are rejected
msg = 'Unable to concatenate a string array and a categorical array. Multiple categorical values represented as a string array are not converted automatically. Convert using the categorical function.';
asserts.checkerror('[a ["x" "y"]]', msg);
asserts.checkerror('[strings(0) a]', msg);
asserts.checkerror('[{1} a]', 'Unable to concatenate cell array and a categorical array unless the cell array contains only character vectors.');
asserts.checkerror('[1 a]', 'Unable to concatenate a double array and a categorical array.');
asserts.checkerror('[zeros(1, 0) a]', 'Unable to concatenate a double array and a categorical array.');
asserts.checkerror('[true a]', 'Unable to concatenate a logical array and a categorical array.');
asserts.checkerror('[a int8(1)]', 'Unable to concatenate a int8 array and a categorical array.');
asserts.checkerror('[a seconds(1)]', 'Unable to concatenate a duration array and a categorical array.');
%=============================================================================
