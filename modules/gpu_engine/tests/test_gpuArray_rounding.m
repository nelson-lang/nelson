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
% floor / ceil / round / fix / sign on a real gpuArray match the host result.
a = single([-2.5 -1.4 1.4 2.5 -0.5 3.7 0 -3.5]);
A = gpuArray(a);
assert_isequal(gather(floor(A)), floor(a));
assert_isequal(gather(ceil(A)), ceil(a));
assert_isequal(gather(round(A)), round(a)); % round half away from zero
assert_isequal(gather(fix(A)), fix(a));
assert_isequal(gather(sign(A)), sign(a));
%=============================================================================
% results stay device resident
assert_istrue(isgpuarray(floor(A)));
assert_istrue(isgpuarray(sign(A)));
%=============================================================================
% an integer input is returned unchanged and keeps its class (floor/ceil/round/
% fix of an integer is itself); sign keeps the integer class too
assert_isequal(gather(floor(gpuArray(int8([-5 3 -2])))), int8([-5 3 -2]));
assert_isequal(gather(ceil(gpuArray(int16([-5 3 -2])))), int16([-5 3 -2]));
assert_isequal(gather(round(gpuArray(int32([-5 3 -2])))), int32([-5 3 -2]));
assert_isequal(gather(fix(gpuArray(uint8([5 3 2])))), uint8([5 3 2]));
assert_isequal(class(gather(floor(gpuArray(int8([1 2 3]))))), 'int8');
%=============================================================================
% sign keeps the integer class (int8/16 and int32/uint32)
assert_isequal(gather(sign(gpuArray(int8([-7 0 12])))), int8([-1 0 1]));
assert_isequal(gather(sign(gpuArray(int32([-7 0 12])))), int32([-1 0 1]));
assert_isequal(gather(sign(gpuArray(uint32([0 5 12])))), uint32([0 1 1]));
assert_isequal(class(gather(sign(gpuArray(int32([1 2 3]))))), 'int32');
%=============================================================================
% mod follows the sign of the divisor (mod(x,0) == x); rem follows the sign of
% the dividend (rem(x,0) == NaN); both match the host result
x = single([5 -5 5 -5 7 5]);
y = single([3 3 -3 -3 0 2.5]);
assert_isequal(gather(mod(gpuArray(x), gpuArray(y))), mod(x, y));
assert_isequal(gather(rem(gpuArray(x), gpuArray(y))), rem(x, y));
% integer mod/rem keep the class
assert_isequal(gather(mod(gpuArray(int8([-5 5 7])), int8(3))), int8([1 2 1]));
assert_isequal(gather(rem(gpuArray(int8([-5 5 7])), int8(3))), int8([-2 2 1]));
assert_isequal(class(gather(mod(gpuArray(int16([5 3])), int16(3)))), 'int16');
%=============================================================================
