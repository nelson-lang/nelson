%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% mean over a vector of dimensions is sum / count over those dimensions
A = int8(reshape([100 100 -10 -100 50 50 -100 -100 1 2 3 4 90 90 -90 -90 5 6 7 8 120 -120 60 60], 2, 3, 4));
asserts.isapprox(mean(A, [1 3]), [24.375 -35.875 29.75], 1e-14);
asserts.isapprox(mean(A, [3 1]), [24.375 -35.875 29.75], 1e-14);
asserts.isequal(mean(A, [2 3]), [59 / 3; -7.5]);
asserts.isapprox(mean(A, [1 2 3]), 6.083333333333333, 1e-14);
asserts.isapprox(mean(A, [3 5]), [24.25 5.25 29.5; 24.5 -77 30], 1e-14);
%=============================================================================
% 'native' integers are rounded once
asserts.isequal(mean(A, [1 3], 'native'), int8([24 -36 30]));
asserts.isequal(mean(A, [2 3], 'native'), int8([20; -8]));
asserts.isequal(mean(A, 'all', 'native'), int8(6));
asserts.isequal(mean(uint8(A), [1 3], 'native'), uint8([49 15 30]));
asserts.isequal(mean(single(A), [1 3], 'native'), single([24.375 -35.875 29.75]));
asserts.isequal(mean(A, [1 3], 'double'), [24.375 -35.875 29.75]);
asserts.isequal(mean(int8([]), [1 2], 'native'), int8(0));
asserts.isequal(mean(zeros(0, 3), [1 2]), NaN);
%=============================================================================
% omitted NaN with a vector of dimensions
D = double(A) / 7;
D([1 5 9 24]) = NaN;
D(:, 2, :) = NaN;
asserts.isequal(mean(D, [1 3]), [NaN NaN NaN]);
asserts.isapprox(mean(D, [1 3], 'omitnan'), [1.93877551020408 NaN 3.04761904761905], 1e-12);
asserts.isapprox(mean(D, [2 3], 'omitnan'), [1.54761904761905; 3.22448979591837], 1e-12);
asserts.isapprox(mean(D, [1 2 3], 'omitnan'), 2.45054945054945, 1e-12);
asserts.isapprox(mean(D, [1 3], 'omitmissing'), [1.93877551020408 NaN 3.04761904761905], 1e-12);
R = mean(single(D), [1 3], 'omitnan');
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [1.93877551020408 NaN 3.04761904761905], 1e-6);
C = D + 1i * fliplr(reshape(1:24, 2, 3, 4));
R = mean(C, [1 3], 'omitnan');
asserts.isapprox(real(R), [1.93877551020408 NaN 3.04761904761905], 1e-12);
asserts.isapprox(imag(R), [15.8571428571429 0 10.5], 1e-12);
asserts.isequal(mean(A, [1 3], 'omitnan', 'native'), int8([24 -36 30]));
asserts.isequal(mean(A, [1 3], 'native', 'omitnan'), int8([24 -36 30]));
%=============================================================================
% options without a dimension, in any order
asserts.isequal(mean(int8([100 100 -10]), 'omitnan', 'native'), int8(63));
asserts.isequal(mean(int8([100 100 -10]), 'native', 'omitnan'), int8(63));
asserts.isequal(mean([1 NaN 3], 'omitnan', 'double'), 2);
asserts.checkerror('mean([1 2 3], ''omitnan'', ''includenan'')', sprintf(_('Invalid %s argument.'), 'third'), ...
  'Nelson:statistics:invalidValue:invalidArgument');
asserts.checkerror('mean([1 2 3], ''native'', ''double'')', sprintf(_('Invalid %s argument.'), 'third'), ...
  'Nelson:statistics:invalidValue:invalidArgument');
asserts.checkerror('mean([1 2 3], ''omitnan'', 1)', sprintf(_('Invalid %s argument.'), 'third'), ...
  'Nelson:statistics:invalidValue:invalidArgument');
%=============================================================================
