%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[y, idx] = hampel([1 1 10 1 1], 1, 2);
asserts.isequal(y, [1 1 1 1 1]);
asserts.isequal(idx, [false false true false false]);
%=============================================================================
[y, idx, xmedian, xsigma] = hampel([1 2 100 4 5 6], 1, 2);
asserts.isequal(y, [1 2 4 4 5 6]);
asserts.isequal(idx, [false false true false false false]);
asserts.isapprox(xmedian, [1.5 2 4 5 5 5.5], 1e-12);
asserts.isapprox(xsigma, [0.741301109252801 1.4826022185056 2.9652044370112 ...
  1.4826022185056 1.4826022185056 0.741301109252801], 1e-12);
%=============================================================================
[y, idx] = hampel([1; 1; 10; 1; 1], 1, 2);
asserts.isequal(y, [1; 1; 1; 1; 1]);
asserts.isequal(idx, [false; false; true; false; false]);
%=============================================================================
[y, idx] = hampel([1 1 1 20 1 1 1]);
asserts.isequal(y, [1 1 1 1 1 1 1]);
asserts.isequal(idx, [false false false true false false false]);
asserts.checkerror('hampel([1 2], -1)', _('Number of neighbors must be a positive integer.'));
asserts.checkerror('hampel([1 2], 1.5)', _('Number of neighbors must be a positive integer.'));
asserts.checkerror('hampel([1 2], 1, -1)', _('Number of standard deviations must be nonnegative.'));
[y, idx] = hampel([1 2], 1, NaN);
asserts.isequal(y, [1 2]);
asserts.isequal(idx, [false false]);
%=============================================================================
