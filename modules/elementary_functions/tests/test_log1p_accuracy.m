%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% log1p must be accurate for small values
R = log1p(1e-15);
assert(abs(R - 1e-15) / 1e-15 < 1e-10);
R = log1p(-1e-15);
assert(abs(R - (-1e-15)) / 1e-15 < 1e-10);
R = log1p(1e-10);
assert(abs(R - 9.999999999500000e-11) / 1e-10 < 1e-10);
% standard values
asserts.isapprox(log1p(1), log(2), 1e-15);
asserts.isequal(log1p(0), 0);
asserts.isequal(log1p(-1), -Inf);
assert(isnan(log1p(NaN)));
% below -1: complex result
R = log1p(-2);
asserts.isapprox(real(R), 0, 1e-15);
asserts.isapprox(imag(R), pi, 1e-15);
% single
R = log1p(single(1e-7));
asserts.isequal(class(R), 'single');
assert(abs(double(R) - 1e-7) / 1e-7 < 1e-6);
%=============================================================================
