%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% numeric variable is sorted numerically even with a cellstr variable
T = table([10; 9; 2], {'a'; 'b'; 'c'});
[R, I] = sortrows(T);
assert_isequal(R.Var1, [2; 9; 10]);
assert_isequal(R.Var2, {'c'; 'b'; 'a'});
assert_isequal(I, [3; 2; 1]);
%=============================================================================
% lexicographic across variables of different types, stable ties
T = table([2; 1; 2; 1; 2], {'b'; 'z'; 'a'; 'z'; 'b'}, [5; 4; 3; 2; 1]);
[R, I] = sortrows(T, {'Var1', 'Var2'});
assert_isequal(I, [2; 4; 3; 1; 5]);
assert_isequal(R.Var3, [4; 2; 3; 5; 1]);
%=============================================================================
% 'descend' keeps ties stable
[~, I] = sortrows(T, {'Var1', 'Var2'}, 'descend');
assert_isequal(I, [1; 5; 3; 2; 4]);
%=============================================================================
% one direction per variable
[~, I] = sortrows(T, {'Var1', 'Var2'}, {'ascend', 'descend'});
assert_isequal(I, [2; 4; 1; 5; 3]);
[~, I] = sortrows(T, [1 2], ["descend", "ascend"]);
assert_isequal(I, [3; 1; 5; 2; 4]);
%=============================================================================
% numeric with NaN: missing values last in both directions, stable ties
T = table([3; NaN; 1; 20; NaN], 'VariableNames', {'x'});
[~, I] = sortrows(T);
assert_isequal(I, [3; 1; 4; 2; 5]);
[~, I] = sortrows(T, 'x', 'descend');
assert_isequal(I, [4; 1; 3; 2; 5]);
%=============================================================================
% string variable with <missing>
T = table(["b"; missing; "a"; "c"], [1; 2; 3; 4]);
[~, I] = sortrows(T);
assert_isequal(I, [3; 1; 4; 2]);
[~, I] = sortrows(T, 1, 'descend');
assert_isequal(I, [4; 1; 3; 2]);
%=============================================================================
% missing last per variable with mixed directions
T = table([1; 1; 2; 2], [NaN; 5; NaN; 7]);
[~, I] = sortrows(T, [1 2], {'descend', 'descend'});
assert_isequal(I, [4; 3; 2; 1]);
%=============================================================================
% categorical variable follows the category order, undefined last
c = categorical({'low'; 'high'; ''; 'mid'}, {'low', 'mid', 'high'});
T = table(c, [1; 2; 3; 4]);
[~, I] = sortrows(T);
assert_isequal(I, [1; 4; 2; 3]);
[~, I] = sortrows(T, 1, 'descend');
assert_isequal(I, [2; 4; 1; 3]);
%=============================================================================
% datetime and duration variables
T = table([datetime(2020, 1, 3); NaT; datetime(2019, 5, 1)], [hours(2); hours(10); hours(1)]);
[~, I] = sortrows(T, 1);
assert_isequal(I, [3; 1; 2]);
[~, I] = sortrows(T, 1, 'descend');
assert_isequal(I, [1; 3; 2]);
[~, I] = sortrows(T, 2);
assert_isequal(I, [3; 1; 2]);
%=============================================================================
% logical variable
T = table([true; false; true], {'x'; 'y'; 'a'});
[~, I] = sortrows(T);
assert_isequal(I, [2; 3; 1]);
%=============================================================================
% multicolumn variable is sorted column by column
T = table([1 20; 1 3; 0 100], 'VariableNames', {'M'});
[R, I] = sortrows(T);
assert_isequal(I, [3; 2; 1]);
assert_isequal(R.M, [0 100; 1 3; 1 20]);
%=============================================================================
% integer variable with a string variable
T = table(int8([-1; 5; -7]), ["b"; "a"; "c"]);
[~, I] = sortrows(T);
assert_isequal(I, [3; 1; 2]);
%=============================================================================
% no rows
T = table(zeros(0, 1), cell(0, 1));
[R, I] = sortrows(T);
assert_isequal(height(R), 0);
assert_isequal(size(I), [0 1]);
%=============================================================================
% invalid direction
assert_checkerror('sortrows(table([1; 2]), 1, ''up'')', 'Sorting direction must be ''ascend'' or ''descend'', or a cell array with one direction per sorting variable.');
assert_checkerror('sortrows(table([1; 2], [3; 4]), [1 2], {''ascend'', ''descend'', ''ascend''})', 'Sorting direction must be ''ascend'' or ''descend'', or a cell array with one direction per sorting variable.');
%=============================================================================
