%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The expected branch convention for real inputs outside [-1, 1]
R = asin(2);
asserts.isapprox(real(R), 1.5707963267948966, 1e-15);
asserts.isapprox(imag(R), -1.3169578969248166, 1e-15);
R = asin(-2);
asserts.isapprox(real(R), -1.5707963267948966, 1e-15);
asserts.isapprox(imag(R), 1.3169578969248166, 1e-15);
R = acos(2);
asserts.isapprox(real(R), 0, 1e-15);
asserts.isapprox(imag(R), 1.3169578969248166, 1e-15);
R = acos(-2);
asserts.isapprox(real(R), pi, 1e-15);
asserts.isapprox(imag(R), -1.3169578969248166, 1e-15);
% single
R = asin(single(2));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(real(R)), 1.5707964, 1e-6);
asserts.isapprox(double(imag(R)), -1.316958, 1e-6);
R = acos(single(2));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(imag(R)), 1.316958, 1e-6);
% vector mixing in-range and out-of-range values
R = asin([0.5 2]);
asserts.isapprox(real(R(1)), asin(0.5), 1e-15);
asserts.isequal(imag(R(1)), 0);
asserts.isapprox(imag(R(2)), -1.3169578969248166, 1e-15);
% in-range values stay real
assert(isreal(asin(0.5)));
assert(isreal(acos(-0.5)));
assert(isreal(asin(NaN)));
%=============================================================================
