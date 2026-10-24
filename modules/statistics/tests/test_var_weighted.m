%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% var(X, w) and std(X, w) with a weight vector compute weighted variance,
% normalized by sum(w). Expected values are listed explicitly below.
%=============================================================================
assert_isapprox(var([1 2 3], [1 1 2]), 0.6875, 1e-10);
assert_isapprox(std([1 2 3], [1 1 2]), sqrt(0.6875), 1e-10);
%=============================================================================
% equal weights match the population variance var(X, 1)
assert_isapprox(var([1 2 3 4], [1 1 1 1]), var([1 2 3 4], 1), 1e-12);
assert_isapprox(var([1 2 3], [0.5 0.5 0.5]), var([1 2 3], 1), 1e-12);
%=============================================================================
% weighting along the default (first non-singleton) dimension of a matrix
assert_isapprox(var([1 2; 3 4; 5 6], [1 2 3]), [20 / 9 20 / 9], 1e-10);
%=============================================================================
% weighting along an explicit dimension
assert_isapprox(var([1 2 3; 4 5 6], [1 1 1], 2), [2 / 3; 2 / 3], 1e-10);
%=============================================================================
% the scalar normalization flags are unchanged
assert_isapprox(var([1 2 3], 0), 1, 1e-12);
assert_isapprox(var([1 2 3], 1), 2 / 3, 1e-12);
%=============================================================================
% a weight length that does not match the operating dimension errors
assert_checkerror('var([1 2 3], [1 2])', 'The length of W must be compatible with X.');
%=============================================================================
