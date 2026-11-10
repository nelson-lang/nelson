%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
% implicit expansion: a dimension of length 1 is repeated
h = single([1 2 3; 4 5 6]);
g = gpuArray(h);
assert_isequal(gather(g + g(1, :)), h + h(1, :));
assert_isequal(gather(g .* g(:, 1)), h .* h(:, 1));
assert_isequal(gather(g(1, :) + g(:, 1)), h(1, :) + h(:, 1));
assert_isequal(gather(g + [1 2 3]), h + single([1 2 3]));
assert_isequal(gather(g > g(1, :)), h > h(1, :));
assert_isequal(gather(max(g, g(:, 1))), max(h, h(:, 1)));
assert_isapprox(gather(atan2(g, g(1, :))), atan2(h, h(1, :)), 1e-4);
assert_isequal(size(zeros(0, 3, 'gpuArray') + g(1, :)), [0 3]);
assert_isequal(size(zeros(2, 0, 'gpuArray') + zeros(1, 0, 'gpuArray')), [2 0]);
assert_checkerror('g + [1 2]', _('Arrays have incompatible sizes for this operation.'));
%=============================================================================
% N-D, complex and integer operands
A = rand(2, 3, 4, 'single');
B = rand(1, 3, 1, 'single');
C = rand(2, 1, 4, 'single');
D = rand(1, 1, 1, 5, 'single');
assert_isequal(gather(gpuArray(A) + gpuArray(B)), A + B);
assert_isequal(gather(gpuArray(A) .* gpuArray(C)), A .* C);
assert_isequal(gather(gpuArray(A) - gpuArray(D)), A - D);
assert_isequal(gather(gpuArray(B) + gpuArray(C)), B + C);
z = single([1 2; 3 4]) * 1i;
assert_isequal(gather(gpuArray(z) .* gpuArray(single([1 2]))), z .* single([1 2]));
assert_isequal(gather(gpuArray(z) == gpuArray(single([1i 2i]))), z == single([1i 2i]));
s8 = gather(gpuArray(int8([100 -100])) + gpuArray(int8([100; 50])));
assert_isequal(s8, int8([127 0; 127 -50]));
s32 = gather(gpuArray(int32([1 2])) - gpuArray(int32([10; 20])));
assert_isequal(s32, int32([-9 -8; -19 -18]));
x = rand(2000, 1, 'single');
y = rand(1, 2000, 'single');
assert_isequal(gather(gpuArray(x) + gpuArray(y)), x + y);
%=============================================================================
% class conversions stay on the device
r = int8(gpuArray([1.5 -2.5 NaN 300 -300]));
assert_istrue(isgpuarray(r));
assert_isequal(gather(r), int8([2 -3 0 127 -128]));
assert_isequal(gather(uint8(gpuArray(single([-1 255.5 NaN])))), uint8([0 255 0]));
assert_isequal(gather(int16(gpuArray(single([40000 -40000 7.5])))), int16([32767 -32768 8]));
assert_isequal(gather(uint16(gpuArray(single([-3 70000 2.5])))), uint16([0 65535 3]));
r32 = gather(int32(gpuArray(single([2.5 -2.5 NaN 3e9 -3e9]))));
assert_isequal(r32, int32([3 -3 0 2147483647 -2147483648]));
assert_isequal(gather(uint32(gpuArray(single([-1 5e9 7.5])))), uint32([0 4294967295 8]));
assert_isequal(gather(int16(gpuArray(int32([100000 -100000 7])))), int16([32767 -32768 7]));
assert_isequal(gather(uint32(gpuArray(int32([-5 5])))), uint32([0 5]));
assert_isequal(gather(int32(gpuArray(uint32([4294967295 5])))), int32([2147483647 5]));
assert_isequal(gather(single(gpuArray(int8(5)))), single(5));
assert_isequal(gather(single(gpuArray(int32([7 -8])))), single([7 -8]));
assert_isequal(gather(logical(gpuArray([0 2 -1]))), [false true true]);
assert_isequal(gather(logical(gpuArray(int8([0 3])))), [false true]);
assert_isequal(gather(logical(gpuArray(int32([0 -8])))), [false true]);
assert_isequal(gather(single(gpuArray(complex(single(1), single(2))))), complex(single(1), single(2)));
%=============================================================================
% the device has no double storage: double() keeps single precision
d = double(g);
assert_istrue(isgpuarray(d));
assert_isequal(underlyingType(d), 'single');
assert_isequal(gather(d), h);
assert_isequal(underlyingType(double(gpuArray(true))), 'single');
%=============================================================================
% cast
assert_isequal(underlyingType(cast(g, 'int16')), 'int16');
assert_isequal(gather(cast(g, 'int16')), int16(h));
c = cast([1 2], 'like', g);
assert_istrue(isgpuarray(c));
assert_isequal(gather(c), single([1 2]));
assert_isequal(underlyingType(cast(g, 'like', int8(1))), 'int8');
assert_isequal(underlyingType(cast(gpuArray(int8(3)), 'like', g)), 'single');
%=============================================================================
% conversions the device cannot represent
assert_checkerror('logical(gpuArray(NaN))', _('NaN values cannot be converted to logical.'));
msg = _('Complex values cannot be converted to logical.');
assert_checkerror('logical(gpuArray(complex(1, 1)))', msg);
msg = _('Complex integer gpuArray values are not supported.');
assert_checkerror('int8(gpuArray(complex(1, 2)))', msg);
msg = sprintf(_('Conversion of a gpuArray to %s is not supported.'), 'int64');
assert_checkerror('int64(gpuArray(5))', msg);
%=============================================================================
