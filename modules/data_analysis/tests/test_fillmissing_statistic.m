%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fillmissing with the 'mean', 'median' and 'mode' fill methods
%=============================================================================
A = [NaN 1 NaN NaN 4 NaN NaN];
[F, TF] = fillmissing(A, 'mean');
assert_isequal(F, [2.5 1 2.5 2.5 4 2.5 2.5]);
assert_isequal(TF, isnan(A));
assert_isequal(fillmissing(A, "median"), [2.5 1 2.5 2.5 4 2.5 2.5]);
%=============================================================================
assert_isequal(fillmissing([NaN 1 1 2 2 NaN 3], 'mode'), [1 1 1 2 2 1 3]);
assert_isequal(fillmissing([5; NaN; 3; 3; NaN; 8], 'mode'), [5; 3; 3; 3; 3; 8]);
assert_isequal(fillmissing([4 NaN 1 7], 'median'), [4 4 1 7]);
%=============================================================================
% EndValues
assert_isequal(fillmissing(A, 'mean', 'EndValues', 'none'), [NaN 1 2.5 2.5 4 NaN NaN]);
assert_isequal(fillmissing(A, 'mean', 'EndValues', 'previous'), [NaN 1 2.5 2.5 4 4 4]);
assert_isequal(fillmissing(A, 'mean', 'EndValues', 'next'), [1 1 2.5 2.5 4 NaN NaN]);
assert_isequal(fillmissing(A, 'mean', 'EndValues', 'nearest'), [1 1 2.5 2.5 4 4 4]);
assert_isequal(fillmissing(A, 'mean', 'EndValues', 9), [9 1 2.5 2.5 4 9 9]);
%=============================================================================
% MaxGap (end gaps are measured to the first or last sample point)
assert_isequal(fillmissing(A, 'mean', 'MaxGap', 2), [2.5 1 NaN NaN 4 2.5 2.5]);
assert_isequal(fillmissing(A, 'mean', 'MaxGap', 1), [2.5 1 NaN NaN 4 NaN NaN]);
R = fillmissing([1 NaN 3 NaN 10], 'mean', 'SamplePoints', [1 2 3 10 11], 'MaxGap', 5);
assert_isapprox(R, [1 14 / 3 3 NaN 10], 1e-12);
%=============================================================================
% matrix, all-missing column stays missing, dim
M = [NaN 2 NaN; 3 NaN NaN; NaN NaN NaN; 5 6 NaN];
[F, TF] = fillmissing(M, 'median');
assert_isequal(F, [4 2 NaN; 3 4 NaN; 4 4 NaN; 5 6 NaN]);
assert_isequal(TF, [true false false; false true false; true true false; false false false]);
assert_isequal(fillmissing(M, 'mean', 2), [2 2 2; 3 3 3; NaN NaN NaN; 5 6 5.5]);
assert_isequal(fillmissing(M, 'mean', 'EndValues', [7 8 9]), [7 2 9; 3 4 9; 4 4 9; 5 6 9]);
%=============================================================================
% integer, single, MissingLocations
R = fillmissing(int8([1 2 4]), 'mean', 'MissingLocations', [false true false]);
assert_isequal(R, int8([1 3 4]));
assert_isequal(fillmissing(single([1 NaN 4]), 'mean'), single([1 2.5 4]));
R = fillmissing([1 -99 3 5], 'median', 'MissingLocations', [false true false false]);
assert_isequal(R, [1 3 3 5]);
%=============================================================================
% table
T = table([1; NaN; 3], [NaN; 4; 8]);
R = fillmissing(T, 'mean');
assert_isequal(R.Var1, [1; 2; 3]);
assert_isequal(R.Var2, [6; 4; 8]);
R = fillmissing(T, 'mode', 'DataVariables', 'Var2');
assert_isequal(R.Var1, [1; NaN; 3]);
assert_isequal(R.Var2, [4; 4; 8]);
%=============================================================================
assert_isequal(size(fillmissing(zeros(0, 3), 'mean')), [0 3]);
%=============================================================================
assert_checkerror('fillmissing(["a", missing], ''mean'')', 'Unsupported "mean" option.');
assert_checkerror('fillmissing(["a", missing], ''mean'')', 'Unsupported "mean" option.', 'Nelson:data_analysis:unsupportedFillMethod');
%=============================================================================
