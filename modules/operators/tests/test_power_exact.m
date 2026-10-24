%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% negative base with integer exponent: exact real result
asserts.isequal((-2) ^ 3, -8);
asserts.isequal((-2) .^ 3, -8);
asserts.isequal((-2) ^ 2, 4);
asserts.isequal((-3) ^ 3, -27);
asserts.isequal((-2) ^ 10, 1024);
asserts.isequal([-2 -3] .^ 3, [-8 -27]);
asserts.isequal((-2) ^ -2, 0.25);
asserts.isequal((-2) ^ -3, -0.125);
assert(isreal((-2) ^ 3));
asserts.isequal(realpow(-2, 3), -8);
asserts.isapprox(nthroot(-8, 3), -2, 1e-15);
% negative base with non-integer exponent stays complex
R = (-2) ^ 0.5;
assert(~isreal(R));
asserts.isapprox(imag(R), sqrt(2), 1e-15);
%=============================================================================
% no absolute epsilon clean-up: small legitimate values are kept
R = complex(1e-4, 0) ^ 2;
asserts.isequal(real(R), 1e-8);
asserts.isequal(imag(R), 0);
R = complex(0, 1e-4) ^ 2;
asserts.isequal(real(R), -1e-8);
asserts.isequal(imag(R), 0);
R = (2 + 0i) ^ 2;
asserts.isequal(real(R), 4);
asserts.isequal(imag(R), 0);
% (-4)^0.5 == 2i (real part cleaned relative to magnitude)
R = (-4) ^ 0.5;
asserts.isequal(real(R), 0);
asserts.isapprox(imag(R), 2, 1e-15);
%=============================================================================
