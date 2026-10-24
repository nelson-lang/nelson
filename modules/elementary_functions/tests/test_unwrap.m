%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected values are listed explicitly below.
%=============================================================================
assert_isapprox(unwrap([0 3 * pi / 2 3 * pi]), [0 -pi / 2 -pi], 1e-10);
%=============================================================================
% differences below pi are left untouched
assert_isapprox(unwrap([0 0.1 0.2]), [0 0.1 0.2], 1e-12);
%=============================================================================
% a column vector stays a column vector
assert_isapprox(unwrap([0; 3 * pi / 2; 3 * pi]), [0; -pi / 2; -pi], 1e-10);
%=============================================================================
% each column of a matrix is unwrapped independently
assert_isapprox(unwrap([0 0; 3 * pi / 2 pi; 3 * pi 2 * pi]), [0 0; -pi / 2 pi; -pi 2 * pi], 1e-10);
%=============================================================================
% unwrapping along the second dimension
assert_isapprox(unwrap([0 3 * pi / 2 3 * pi; 0 pi / 2 pi], [], 2), [0 -pi / 2 -pi; 0 pi / 2 pi], 1e-10);
%=============================================================================
% the tolerance only affects which jumps are corrected, in multiples of 2*pi
assert_isapprox(unwrap([0 2 4 6 8], 1), [0 2 4 6 8], 1e-12);
%=============================================================================
% negative jumps
assert_isapprox(unwrap([0 -3 * pi / 2 -3 * pi]), [0 pi / 2 pi], 1e-10);
%=============================================================================
% a scalar and a single-element dimension are returned unchanged
assert_isequal(unwrap(5), 5);
%=============================================================================
