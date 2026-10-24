%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fillmissing: every method operates along dim (default: first non-singleton)
%=============================================================================
A = [1 NaN 3; NaN 5 NaN; 7 8 NaN];
assert_isequal(fillmissing(A, 'previous'), [1 NaN 3; 1 5 3; 7 8 3]);
assert_isequal(fillmissing(A, 'previous', 2), [1 1 3; NaN 5 5; 7 8 8]);
assert_isequal(fillmissing(A, 'next', 2), [1 3 3; 5 5 NaN; 7 8 NaN]);
assert_isequal(fillmissing(A, 'previous', 3), A);
assert_isequal(fillmissing([1 NaN 3], 'previous'), [1 1 3]);
assert_isequal(fillmissing([1 NaN 3; NaN 2 NaN], 'linear', 2), [1 2 3; NaN 2 NaN]);
assert_isequal(fillmissing([1 NaN 3; NaN 5 NaN], 'movmean', 3, 2), [1 2 3; 5 5 5]);
assert_isequal(fillmissing([1 NaN 3; NaN 5 NaN], 'constant', [10 20], 2), [1 10 3; 20 5 20]);
assert_isequal(fillmissing([1 NaN 3; NaN 5 NaN], 'constant', [10 20 30]), [1 20 3; 10 5 30]);
assert_isequal(fillmissing([1 NaN], 'constant', 0, 3), [1 0]);
assert_isequal(fillmissing([NaN 1 NaN; NaN 2 3], 'previous', 2, 'EndValues', [7 8]), [7 1 7; 8 2 3]);
%=============================================================================
% N-D arrays: TF has the size of A
B = reshape(1:24, 2, 3, 4);
B([3 8 9 14 20]) = NaN;
[F, TF] = fillmissing(B, 'previous', 3);
R = B;
R([8 14 20]) = 2;
assert_isequal(F, R);
assert_isequal(size(TF), [2 3 4]);
assert_isequal(find(TF)', [8 14 20]);
R = fillmissing(B, 'linear', 3);
assert_isequal(R(:)', [1 2 3 4 5 6 7 NaN 9 10 11 12 13 NaN 15 16 17 18 19 NaN 21 22 23 24]);
R = fillmissing(B, 'nearest', 2);
assert_isequal(R(:)', [1 2 5 4 5 6 7 10 11 10 11 12 13 16 15 16 17 18 19 22 21 22 23 24]);
[F, TF] = fillmissing(B, 'next', 3);
assert_isequal(size(TF), [2 3 4]);
assert_isequal(find(TF)', [3 9]);
assert_isequal(F([3 9]), [15 15]);
R = fillmissing(B, 'movmean', 3, 3);
assert_isequal(R(:)', [1 2 NaN 4 5 6 7 2 15 10 11 12 13 NaN 15 16 17 18 19 NaN 21 22 23 24]);
R = fillmissing(B, 'constant', [1 2 3 4 5 6], 3);
assert_isequal(R(:)', [1 2 3 4 5 6 7 2 3 10 11 12 13 2 15 16 17 18 19 2 21 22 23 24]);
R = fillmissing(B, 'previous', 3, 'EndValues', 'nearest');
assert_isequal(R(:)', [1 2 15 4 5 6 7 2 15 10 11 12 13 2 15 16 17 18 19 2 21 22 23 24]);
assert_isequal(fillmissing(B, 'previous', 4), B);
%=============================================================================
% nearest: ties go to the next value, distances follow SamplePoints
assert_isequal(fillmissing([1 NaN 3], 'nearest'), [1 3 3]);
assert_isequal(fillmissing([1 NaN NaN NaN 5], 'nearest'), [1 1 5 5 5]);
assert_isequal(fillmissing([1 NaN 3], 'nearest', 'SamplePoints', [0 2 3]), [1 3 3]);
assert_isequal(fillmissing([1 NaN NaN NaN 5 NaN 7], 'nearest', 'MaxGap', 2), [1 NaN NaN NaN 5 7 7]);
%=============================================================================
% linear: extrapolation from the two nearest values, at least two values needed
assert_isequal(fillmissing([NaN 1 2 NaN NaN], 'linear'), [0 1 2 3 4]);
assert_isequal(fillmissing([NaN 1 NaN], 'linear'), [NaN 1 NaN]);
assert_isequal(fillmissing([1 NaN 3 NaN], 'linear', 'SamplePoints', [0 1 4 5]), [1 1.5 3 3.5]);
assert_isequal(fillmissing([1 NaN NaN 4 NaN 6], 'linear', 'MaxGap', 2), [1 NaN NaN 4 5 6]);
assert_isequal(fillmissing(single([1 NaN 4]), 'linear'), single([1 2.5 4]));
[F, TF] = fillmissing([NaN 2 NaN NaN], 'linear', 'EndValues', 5, 'MaxGap', 1);
assert_isequal(F, [5 2 NaN NaN]);
assert_isequal(TF, [true false false false]);
%=============================================================================
% spline, pchip, makima
assert_isequal(fillmissing([NaN 1 NaN], 'spline'), [NaN 1 NaN]);
assert_isapprox(fillmissing([NaN 1 4 NaN 16 NaN], 'spline'), [0 1 4 9 16 25], 1e-12);
assert_isapprox(fillmissing([NaN 1 4 NaN 16 NaN], 'pchip'), [0.28571428571428648 1 4 8.9642857142857153 16 24.892857142857142], 1e-12);
assert_isapprox(fillmissing([NaN 1 4 NaN 16 25 NaN], 'makima'), [3.7000000000000011 1 4 9.2071428571428555 16 25 34.80952380952381], 1e-12);
assert_isapprox(fillmissing([NaN 1 NaN 3 NaN 7 NaN], 'pchip'), [0.875 1 1.7916666666666667 3 4.708333333333333 7 9.625], 1e-12);
assert_isapprox(fillmissing([NaN 1 NaN 3 NaN], 'spline'), [0 1 2 3 4], 1e-12);
assert_isapprox(fillmissing([NaN 1 NaN 3 NaN], 'makima'), [0 1 2 3 4], 1e-12);
M = magic(4);
M([2 7 12]) = NaN;
assert_isapprox(fillmissing(M, 'makima', 2), [16 2 3 13; 10.1 11 10 8; 9 6.09375 6 12; 4 14 10.59375 1], 1e-12);
assert_isapprox(fillmissing(M, 'spline', 2), [16 2 3 13; 11 11 10 8; 9 5 6 12; 4 14 13 1], 1e-12);
assert_isapprox(fillmissing(M, 'pchip', 2), [16 2 3 13; 32 / 3 11 10 8; 9 6.375 6 12; 4 14 11.875 1], 1e-12);
assert_isapprox(fillmissing([1 NaN 3 NaN 5; 2 4 NaN 8 NaN], 'spline', 2), [1 2 3 4 5; 2 4 6 8 10], 1e-12);
%=============================================================================
% movmean, movmedian: window centered on the missing entry
X = [1 NaN 3 NaN NaN NaN 10 12];
assert_isequal(fillmissing(X, 'movmean', 3), [1 2 3 3 NaN 10 10 12]);
assert_isequal(fillmissing(X, 'movmean', 4), [1 2 3 3 3 10 10 12]);
assert_isequal(fillmissing(X, 'movmean', [2 0]), [1 1 3 3 3 NaN 10 12]);
assert_isequal(fillmissing(X', 'movmean', 3), [1 2 3 3 NaN 10 10 12]');
assert_isequal(fillmissing([1 NaN 3 NaN 5 7 NaN 10 12], 'movmedian', 4), [1 2 3 4 5 7 7 10 12]);
assert_isequal(fillmissing([NaN 1 NaN 3 NaN], 'movmean', 3, 'EndValues', 'none'), [NaN 1 2 3 NaN]);
assert_isequal(fillmissing([1 NaN 3 NaN 5], 'movmean', 3, 'SamplePoints', [1 2 4 7 8]), [1 1 3 5 5]);
assert_isequal(fillmissing([10 20 NaN 40 50], 'movmean', 2, 'SamplePoints', 1:5), [10 20 20 40 50]);
assert_isequal(fillmissing([10 20 NaN 40 50], 'movmean', [1 1], 'SamplePoints', 1:5), [10 20 30 40 50]);
assert_isequal(fillmissing([10 20 NaN 40 50], 'movmean', 2), [10 20 20 40 50]);
M = magic(4);
M([2 7 12]) = NaN;
assert_isequal(fillmissing(M, 'movmedian', 3, 2), [16 2 3 13; 11 11 10 8; 9 7.5 6 12; 4 14 7.5 1]);
%=============================================================================
% function handle: fun(xs, ts, tq) per gap, window [t1 - b, t2 + a]
f = @(xs, ts, tq) repmat(100 * numel(ts) + sum(ts), size(tq));
assert_isequal(fillmissing([1 NaN NaN 4 5 NaN 7], f, 2), [1 205 205 4 5 212 7]);
assert_isequal(fillmissing([1 NaN NaN 4 5 NaN 7], f, 4), [1 310 310 4 5 316 7]);
assert_isequal(fillmissing([NaN 1 NaN NaN 4 5 NaN 7], f, [1 2]), [205 1 313 313 4 5 214 7]);
assert_isequal(fillmissing([NaN 1 NaN NaN 4 5 NaN 7]', f, [1 2]), [205 1 313 313 4 5 214 7]');
assert_isequal(fillmissing([1 NaN 4 5], f, 3, 'SamplePoints', [0 1.5 3 4]), [1 203 4 5]);
g = @(xs, ts, tq) repmat(sum(xs), size(tq));
assert_isequal(fillmissing([1 NaN 3; NaN 5 NaN], g, 2, 2), [1 4 3; 5 5 5]);
assert_isequal(fillmissing([NaN 1 NaN 3 NaN], @(xs, ts, tq) repmat(9, size(tq)), 3, 'EndValues', 'none'), [NaN 1 9 3 NaN]);
%=============================================================================
% EndValues and MaxGap apply to every method
assert_isequal(fillmissing([NaN 1 NaN 2 NaN], 'constant', 0, 'EndValues', 'none'), [NaN 1 0 2 NaN]);
assert_isequal(fillmissing([NaN 1 NaN 2 NaN], 'previous', 'EndValues', 'next'), [1 1 1 2 NaN]);
assert_isequal(fillmissing([NaN 1 NaN 2 NaN], 'next', 'EndValues', 9), [9 1 2 2 9]);
assert_isequal(fillmissing([1 NaN 2 NaN NaN 5 NaN], 'previous', 'MaxGap', 2), [1 1 2 NaN NaN 5 5]);
assert_isequal(fillmissing([1 NaN 2 NaN NaN 5 NaN], 'constant', 0, 'MaxGap', 2), [1 0 2 NaN NaN 5 0]);
[F, TF] = fillmissing([1 NaN NaN 1], 'previous', 'EndValues', 'nearest', 'MaxGap', 1);
assert_isequal(F, [1 NaN NaN 1]);
assert_isequal(TF, false(1, 4));
%=============================================================================
% an entry filled with a missing value is not reported as filled
[F, TF] = fillmissing([1 NaN 3], 'constant', NaN);
assert_isequal(TF, false(1, 3));
[F, TF] = fillmissing([1 NaN 3], @(xs, ts, tq) NaN(size(tq)), 3);
assert_isequal(TF, false(1, 3));
%=============================================================================
% text and integer data
assert_isequal(fillmissing({'a', ''; '', 'b'}, 'previous', 2), {'a', 'a'; '', 'b'});
assert_isequal(fillmissing(["a" missing; missing "b"], 'next', 2), ["a" missing; "b" "b"]);
assert_isequal(fillmissing(int16([1 2 3]), 'previous', 'MissingLocations', [false true false]), int16([1 1 3]));
assert_isequal(fillmissing([true false true], 'previous', 'MissingLocations', [false true false]), [true true true]);
assert_isequal(size(fillmissing(zeros(0, 3), 'linear')), [0 3]);
%=============================================================================
assert_checkerror('fillmissing([1 NaN], ''previous'', 0)', 'Dimension argument must be a positive integer scalar.', 'Nelson:data_analysis:dimensionArgumentPositiveIntegerScalar');
assert_checkerror('fillmissing([1 NaN], ''previous'', 1.5)', 'Dimension argument must be a positive integer scalar.');
assert_checkerror('fillmissing({''a'', ''''}, ''linear'')', 'Unsupported "linear" option.', 'Nelson:data_analysis:unsupportedFillMethod');
assert_checkerror('fillmissing([1 NaN], ''cubic'')', 'Unsupported "cubic" option.');
assert_checkerror('fillmissing([1 NaN], ''movmean'')', 'Window size expected.');
%=============================================================================
