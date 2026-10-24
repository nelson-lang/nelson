%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% bandpower returns a power, which is a double quantity: an integer input must
% be promoted. Squaring in place saturated, and the sampling-frequency and psd
% forms raised an integer class mismatch.
%=============================================================================
asserts.isapprox(bandpower(uint8([10 40 70 20 50 80])), 2650, 1e-12);
asserts.isapprox(bandpower(int16([100 200 300])), 140000 / 3, 1e-12);
asserts.isapprox(bandpower(int32([100000 200000])), 25000000000, 1e-12);
asserts.isapprox(bandpower(int64([4000000000 5000000000])), 20500000000000000000, 1e-12);
asserts.isapprox(bandpower(uint16(300)), 90000, 1e-12);
asserts.isequal(class(bandpower(uint8([1 2 3]))), 'double');
%=============================================================================
% abs(int8(-128)) clamps to 127 on its own, before the square.
asserts.isapprox(bandpower(int8([100 -128 50])), 9628, 1e-12);
asserts.isapprox(bandpower(int8(-128)), 16384, 1e-12);
%=============================================================================
n = 64;
t = (0:n - 1) / 100;
xd = round(100 * cos(2 * pi * 10 * t));
asserts.isapprox(bandpower(int16(xd), 100, [5 15]), bandpower(xd, 100, [5 15]), 1e-12);
asserts.isapprox(bandpower(int32(xd), 100, [5 15]), bandpower(xd, 100, [5 15]), 1e-12);
asserts.isapprox(bandpower(uint8(mod(xd, 200)), 50, [5 15]), ...
  bandpower(double(uint8(mod(xd, 200))), 50, [5 15]), 1e-12);
%=============================================================================
pxx = [1 2; 3 4; 5 6];
f = [0; 1; 2];
asserts.isapprox(bandpower(int16(pxx), f, 'psd'), [9 12], 1e-12);
asserts.isapprox(bandpower(uint8(pxx), f, [0 1], 'psd'), [4 6], 1e-12);
asserts.isapprox(bandpower(pxx, int16(f), 'psd'), [9 12], 1e-12);
asserts.isapprox(bandpower(int16(pxx), int16(f), 'psd'), [9 12], 1e-12);
%=============================================================================
% The other classes are unchanged.
asserts.isapprox(bandpower([1 2 3 4]), 7.5, 1e-12);
asserts.isequal(class(bandpower([1 2 3 4])), 'double');
asserts.isequal(class(bandpower(single([1 2 3 4]))), 'single');
asserts.isapprox(bandpower(single([1 2 3 4])), single(7.5), 1e-6);
asserts.istrue(isnan(bandpower([1 NaN 3])));
asserts.isapprox(bandpower(logical([1 0 1])), 2 / 3, 1e-12);
asserts.isapprox(bandpower([3 + 4i, 0]), 12.5, 1e-12);
%=============================================================================
