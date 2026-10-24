%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% expm1 must be accurate for small values (Kahan's algorithm)
R = expm1(1e-15);
assert(abs(R - 1e-15) / 1e-15 < 1e-10);
R = expm1(-1e-15);
assert(abs(R + 1e-15) / 1e-15 < 1e-10);
R = expm1(1e-10);
assert(abs(R - 1.000000000050000e-10) / 1e-10 < 1e-10);
% standard values
asserts.isequal(expm1(0), 0);
asserts.isapprox(expm1(1), exp(1) - 1, 1e-15);
asserts.isequal(expm1(-50), -1);
asserts.isequal(expm1(Inf), Inf);
asserts.isequal(expm1(-Inf), -1);
assert(isnan(expm1(NaN)));
% vector with mixed magnitudes
R = expm1([1e-15, 1, -50, 0]);
assert(abs(R(1) - 1e-15) / 1e-15 < 1e-10);
asserts.isapprox(R(2), exp(1) - 1, 1e-15);
asserts.isequal(R(3), -1);
asserts.isequal(R(4), 0);
% complex falls back to exp(x) - 1
R = expm1(1 + 2i);
asserts.isapprox(R, exp(1 + 2i) - 1, 1e-12);
%=============================================================================
