%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
asserts.isequal(nargin('isoutlier'), -2);
asserts.isequal(nargout('isoutlier'), 4);
%=============================================================================
A = [57 59 60 100 59 58 57 58 300 61 62 60 62 58 57];
TF = isoutlier(A);
asserts.isequal(TF, logical([0 0 0 1 0 0 0 0 1 0 0 0 0 0 0]));
TF = isoutlier(A, 'mean');
asserts.isequal(TF, logical([0 0 0 0 0 0 0 0 1 0 0 0 0 0 0]));
%=============================================================================
[TF, L, U, C] = isoutlier([60 59 49 49 58 100 61 57 48 58]);
asserts.isequal(TF, logical([0 0 0 0 0 1 0 0 0 0]));
asserts.isapprox(L, 46.880483361207986, 1e-12);
asserts.isapprox(U, 69.119516638792021, 1e-12);
asserts.isequal(C, 58);
%=============================================================================
B = [217 24 1 8 15; 23 205 7 14 16; 4 6 213 20 22; 10 12 19 221 3; 11 18 25 2 209];
TF = isoutlier(B, 2);
asserts.isequal(TF, logical(eye(5)));
%=============================================================================
C = [1 2 3 4 100; 10 11 12 13 14];
TF = isoutlier(C, 'quartiles', 2);
asserts.isequal(TF, logical([0 0 0 0 1; 0 0 0 0 0]));
[TF, L, U, center] = isoutlier(C, 'percentiles', [20 80], 2);
asserts.isequal(TF, logical([1 0 0 0 1; 1 0 0 0 1]));
asserts.isapprox(L, [1.5; 10.5], 1e-12);
asserts.isapprox(U, [52; 13.5], 1e-12);
asserts.isequal(center, [3; 12]);
%=============================================================================
D = reshape([1 2 100 4 5 6 7 8 9 10 11 12], [3 2 2]);
TF = isoutlier(D, 'median', 1, 'ThresholdFactor', 2);
asserts.isequal(size(TF), [3 2 2]);
asserts.istrue(TF(3, 1, 1));
asserts.isfalse(any(TF(:) & ((1:numel(TF))' ~= 3)));
%=============================================================================
E = [1 1 1 1 10 1 1 1];
[TF, L, U, C] = isoutlier(E, 'movmedian', 3);
asserts.isequal(TF, logical([0 0 0 0 1 0 0 0]));
asserts.isequal(C(5), 1);
asserts.isequal(L(5), 1);
asserts.isequal(U(5), 1);
TF = isoutlier(E, 'movmean', [1 1], 'ThresholdFactor', 1);
asserts.istrue(TF(5));
H = [1 1 100 1 1];
TF = isoutlier(H, 'movmedian', 3, 'SamplePoints', [1 2 100 101 102]);
asserts.isfalse(TF(3));
TF = isoutlier(H, 'movmedian', 3);
asserts.istrue(TF(3));
%=============================================================================
F = [10 10.1 9.9 10.2 9.8 35];
TF = isoutlier(F, 'grubbs');
asserts.isequal(TF, logical([0 0 0 0 0 1]));
TF = isoutlier([10 10.1 9.9 10.2 9.8 35 38], 'gesd', 'MaxNumOutliers', 2);
asserts.isequal(TF, logical([0 0 0 0 0 1 1]));
%=============================================================================
G = single([1 NaN 1 50]);
TF = isoutlier(G);
asserts.isequal(TF, logical([0 0 0 1]));
%=============================================================================
Z = zeros(0, 3);
[TF, L, U, C] = isoutlier(Z);
asserts.isequal(size(TF), [0 3]);
asserts.isequal(size(L), [1 3]);
asserts.istrue(all(isnan(L(:))));
asserts.istrue(all(isnan(U(:))));
asserts.istrue(all(isnan(C(:))));
%=============================================================================
asserts.checkerror('isoutlier()', _('Not enough input arguments.'));
asserts.checkerror('isoutlier({1})', sprintf(_('%s must be a real numeric array.'), 'A'));
asserts.checkerror('isoutlier([1 2], ''bad'')', sprintf(_('%s has an unsupported value.'), 'METHOD'));
asserts.checkerror('isoutlier([1 2], ''percentiles'')', sprintf(_('%s must be specified.'), 'Percentile thresholds'));
asserts.checkerror('isoutlier([1 2], ''percentiles'', [90 10])', _('Percentile thresholds must be a two-element row vector in increasing order.'));
asserts.checkerror('isoutlier([1 2], ''movmedian'', 0)', _('Window length must be a positive numeric scalar or two-element vector.'));
asserts.checkerror('isoutlier([1 2], ''mean'', ''ThresholdFactor'', -1)', sprintf(_('%s must be a nonnegative scalar.'), 'ThresholdFactor'));
asserts.checkerror('isoutlier([1 2 3], ''gesd'', ''ThresholdFactor'', 2)', _('ThresholdFactor must be a scalar in the range (0,1) for this method.'));
asserts.checkerror('isoutlier([1 2 3], ''gesd'', ''MaxNumOutliers'', 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'MaxNumOutliers'));
asserts.checkerror('isoutlier([1 2 3], ''movmean'', 2.5)', _('Window length must be a positive integer scalar or two-element vector.'));
asserts.checkerror('isoutlier([1 2 3], ''movmean'', 2, ''SamplePoints'', [1 1 2])', _('SamplePoints must be sorted and contain unique values.'));
asserts.checkerror('isoutlier([1 2 3], ''movmean'', 2, ''SamplePoints'', [1 2])', _('SamplePoints must have one element for each input element along the operating dimension.'));
%=============================================================================
