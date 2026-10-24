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
% matrix product stays on the device
a = single(rand(64, 32));
b = single(rand(32, 48));
A = gpuArray(a);
B = gpuArray(b);
C = A * B;
assert_istrue(isgpuarray(C));
assert_isequal(size(C), [64 48]);
% accumulation order differs from BLAS: use a relative tolerance
assert_isapprox(gather(C), a * b, 1e-4);
%=============================================================================
% non-tile-aligned sizes
a2 = single(rand(17, 5));
b2 = single(rand(5, 23));
C2 = gpuArray(a2) * gpuArray(b2);
assert_isapprox(gather(C2), a2 * b2, 1e-4);
%=============================================================================
% scalar * matrix routes to elementwise
S = gpuArray(single(3)) * A;
assert_isapprox(gather(S), 3 * a, 1e-6);
%=============================================================================
% incompatible dimensions
G = gpuArray(single(rand(3, 4)));
H = gpuArray(single(rand(3, 4)));
assert_checkerror('G * H', _('Incorrect dimensions for matrix multiplication.'));
%=============================================================================
% integer * : a scalar operand gives a saturating scalar product keeping the
% class; two integer matrices are not allowed (as on the host)
assert_isequal(gather(gpuArray(int8(2)) * gpuArray(int8([1 2; 3 4]))), int8([2 4; 6 8]));
assert_isequal(gather(gpuArray(int8([50 100; 3 4])) * gpuArray(int8(2))), int8([100 127; 6 8]));
assert_isequal(underlyingType(gpuArray(int8(2)) * gpuArray(int8([1 2; 3 4]))), 'int8');
assert_isequal(gather(gpuArray(int32([1 2; 3 4])) * gpuArray(int32(3))), int32([3 6; 9 12]));
assert_checkerror('gpuArray(int8([1 2; 3 4])) * gpuArray(int8([1 0; 0 1]))', ...
  _('Integer matrix multiplication requires at least one scalar operand.'));
assert_checkerror('gpuArray(int32([1 2; 3 4])) * gpuArray(int32([1 0; 0 1]))', ...
  _('Integer matrix multiplication requires at least one scalar operand.'));
%=============================================================================
