%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fillmissing with the 'knn' fill method
%=============================================================================
A = [1 3 9 3; -5 1 7 2; -1 1 7 NaN; 12 1 9 1];
[F, TF] = fillmissing(A, 'knn');
assert_isequal(F, [1 3 9 3; -5 1 7 2; -1 1 7 3; 12 1 9 1]);
assert_isequal(TF, isnan(A));
assert_isequal(fillmissing(A, 'knn', 2), [1 3 9 3; -5 1 7 2; -1 1 7 2.5; 12 1 9 1]);
assert_isequal(fillmissing(A, 'knn', 3), [1 3 9 3; -5 1 7 2; -1 1 7 2; 12 1 9 1]);
% k larger than the number of neighbors: all of them are used
assert_isequal(fillmissing(A, 'knn', 9), [1 3 9 3; -5 1 7 2; -1 1 7 2; 12 1 9 1]);
%=============================================================================
% a row missing one of the compared coordinates, or the value, is no neighbor
B = [1 2 NaN; 1 NaN 5; 2 2 7; 10 10 10; NaN NaN 1];
[F, TF] = fillmissing(B, 'knn');
assert_isequal(F, [1 2 7; 1 2 5; 2 2 7; 10 10 10; 1 2 1]);
assert_isequal(TF, [false false true; false true false; false false false; false false false; true true false]);
assert_isequal(fillmissing(B, 'knn', 2), [1 2 8.5; 1 6 5; 2 2 7; 10 10 10; 1.5 6 1]);
assert_isequal(fillmissing([0 NaN 0; NaN 7 0; 1 8 0], 'knn'), [0 8 0; 1 7 0; 1 8 0]);
assert_isequal(fillmissing([0 NaN; 0.1 NaN; 1 4; 5 9], 'knn', 2), [0 6.5; 0.1 6.5; 1 4; 5 9]);
assert_isequal(fillmissing([0 NaN 0 0; 3 1 0 0; 0 5 2.9 0], 'knn'), [0 5 0 0; 3 1 0 0; 0 5 2.9 0]);
assert_isequal(fillmissing([0 NaN; Inf 3; 5 8], 'knn'), [0 8; Inf 3; 5 8]);
%=============================================================================
% ties keep the row order, a row without nonmissing values takes the first rows
assert_isequal(fillmissing([0 NaN; 1 10; -1 20], 'knn'), [0 10; 1 10; -1 20]);
assert_isequal(fillmissing([0 NaN; -1 20; 1 10], 'knn'), [0 20; -1 20; 1 10]);
assert_isequal(fillmissing([NaN NaN; 1 2; 3 4], 'knn'), [1 2; 1 2; 3 4]);
[F, TF] = fillmissing([NaN NaN; NaN 1], 'knn');
assert_isequal(F, [NaN 1; NaN 1]);
assert_isequal(TF, [false true; false false]);
%=============================================================================
% dim: with dim 2 the columns are compared
assert_isequal(fillmissing(B', 'knn', 1, 2), [1 1 2 10 1; 2 2 2 10 2; 7 5 7 10 1]);
assert_isequal(fillmissing(B, 'knn', 2, 2), [1 2 NaN; 1 1 5; 2 2 7; 10 10 10; NaN NaN 1]);
assert_isequal(fillmissing([1 NaN 3; 2 5 4], 'knn', 1, 1), [1 5 3; 2 5 4]);
assert_isequal(fillmissing([1; NaN; 3], 'knn'), [1; 1; 3]);
assert_isequal(fillmissing([1 NaN 3], 'knn'), [1 1 3]);
%=============================================================================
% Distance
S = [0 NaN 0; 10 1 1; 0 2 3];
assert_isequal(fillmissing(S, 'knn', 'Distance', 'euclidean'), [0 2 0; 10 1 1; 0 2 3]);
assert_isequal(fillmissing(S, 'knn', 'Distance', "seuclidean"), [0 1 0; 10 1 1; 0 2 3]);
assert_isequal(fillmissing([0 NaN 1; 0 3 2; 0 5 1], 'knn', 'Distance', 'seuclidean'), [0 NaN 1; 0 3 2; 0 5 1]);
d1 = @(x, m) sum(abs(x(1, :) - x(2, :)), 'omitnan');
assert_isequal(fillmissing(A, 'knn', 'Distance', d1), [1 3 9 3; -5 1 7 2; -1 1 7 2; 12 1 9 1]);
assert_isequal(fillmissing([1 NaN; 2 3; 4 5], 'knn', 'Distance', @(x, m) NaN), [1 NaN; 2 3; 4 5]);
assert_isequal(fillmissing([1 NaN; 2 3; 4 5], 'knn', 'Distance', @(x, m) 5 - x(2, 1)), [1 5; 2 3; 4 5]);
R = fillmissing(int8([1 2; 2 3; 5 9]), 'knn', 'Distance', d1, 'MissingLocations', [false true; false false; false false]);
assert_isequal(R, int8([1 3; 2 3; 5 9]));
%=============================================================================
% neighbors are ranked on the unrounded squared distance (no false ties),
% 'seuclidean' divides each difference by the standard deviation of its coordinate
idx = (1:1200)';
X = reshape(mod(idx * 7919, 101) / 10, 200, 6);
X(mod(idx * 31, 17) == 0) = NaN;
R = fillmissing(X, 'knn', 3);
assert_isapprox(R(84, 5), 8.5333333333333332, 1e-12);
R = fillmissing(X, 'knn', 5, 'Distance', 'seuclidean');
assert_isapprox([R(51, 1), R(55, 2), R(12, 4), R(152, 5), R(156, 6)], [7.06 5.16 4.34 4.56 2.66], 1e-12);
%=============================================================================
% single, MissingLocations, empty
R = fillmissing(single([1 NaN; 2 3]), 'knn');
assert_isequal(R, single([1 3; 2 3]));
R = fillmissing([1 -99; 2 3; 2.1 8], 'knn', 'MissingLocations', [false true; false false; false false]);
assert_isequal(R, [1 3; 2 3; 2.1 8]);
assert_isequal(size(fillmissing(zeros(0, 3), 'knn')), [0 3]);
%=============================================================================
% tables: the rows of the data variables are compared
T = table([1; 2; NaN], [4; 5; 6], [7; 8; 100]);
R = fillmissing(T, 'knn');
assert_isequal(R{:, :}, [1 4 7; 2 5 8; 2 6 100]);
R = fillmissing(T, 'knn', 'DataVariables', [1 2]);
assert_isequal(R{:, :}, [1 4 7; 2 5 8; 2 6 100]);
R = fillmissing(T, 'knn', 'ReplaceValues', false);
assert_isequal(R.Properties.VariableNames, {'Var1', 'Var2', 'Var3', 'Var1_filled', 'Var2_filled', 'Var3_filled'});
assert_isequal(R{:, :}, [1 4 7 1 4 7; 2 5 8 2 5 8; NaN 6 100 2 6 100]);
R = fillmissing(table(single([1; 2; NaN]), [4; 5; 6]), 'knn');
assert_isequal(R.Var1, single([1; 2; 2]));
%=============================================================================
assert_checkerror('fillmissing([1 NaN; 2 3], ''knn'', ''EndValues'', ''none'')', 'The EndValues argument is not supported for the ''knn'' method.', 'Nelson:data_analysis:knnUnsupportedArgument');
assert_checkerror('fillmissing([1 NaN; 2 3], ''knn'', ''MaxGap'', 1)', 'The MaxGap argument is not supported for the ''knn'' method.');
assert_checkerror('fillmissing([1 NaN; 2 3], ''knn'', ''SamplePoints'', [1 2])', 'The SamplePoints argument is not supported for the ''knn'' method.');
assert_checkerror('fillmissing([1 NaN; 2 3], ''knn'', 0)', '''k'' value must be a real, positive integer.', 'Nelson:data_analysis:knnInvalidK');
assert_checkerror('fillmissing([1 NaN; 2 3], ''knn'', 1.5)', '''k'' value must be a real, positive integer.');
assert_checkerror('fillmissing({''a''; ''''}, ''knn'')', 'The ''euclidean'' and ''seuclidean'' distance metrics support only double and single data.', 'Nelson:data_analysis:knnFloatExpected');
assert_checkerror('fillmissing(int8([1 2; 2 3]), ''knn'')', 'The ''euclidean'' and ''seuclidean'' distance metrics support only double and single data.');
assert_checkerror('fillmissing([1i NaN; 2 3], ''knn'')', 'First argument must be real when using the ''euclidean'' and ''seuclidean'' distance metrics.', 'Nelson:data_analysis:knnRealExpected');
assert_checkerror('fillmissing([1 NaN; 2 3], ''knn'', ''Distance'', ''cityblock'')', '''Distance'' value must be ''euclidean'', ''seuclidean'', or a function handle.', 'Nelson:data_analysis:knnInvalidDistance');
assert_checkerror('fillmissing([1 NaN; 2 3], ''previous'', ''Distance'', ''euclidean'')', 'The ''Distance'' argument is supported only for the ''knn'' method.', 'Nelson:data_analysis:distanceOnlyForKnn');
assert_checkerror('fillmissing(cat(3, [1 NaN; 2 3], [4 5; 6 7]), ''knn'')', 'First argument must be a matrix or a table for the ''knn'' method.', 'Nelson:data_analysis:knnMatrixExpected');
assert_checkerror('fillmissing(table([1; NaN; 3], {''a''; ''b''; ''c''}), ''knn'')', 'The ''euclidean'' and ''seuclidean'' distance metrics support only double and single data.');
%=============================================================================
