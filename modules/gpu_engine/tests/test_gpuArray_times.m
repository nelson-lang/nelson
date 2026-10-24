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
% elementwise .* stays on the device
a = single(rand(100, 80));
b = single(rand(100, 80));
A = gpuArray(a);
B = gpuArray(b);
C = A .* B;
assert_istrue(isgpuarray(C));
assert_isapprox(gather(C), a .* b, 1e-6);
%=============================================================================
% scalar broadcast
D = A .* gpuArray(single(2));
assert_isapprox(gather(D), a .* 2, 1e-6);
%=============================================================================
% mixed host/device operand
E = A .* 2;
assert_istrue(isgpuarray(E));
assert_isapprox(gather(E), a .* 2, 1e-6);
F = 2 .* A;
assert_istrue(isgpuarray(F));
assert_isapprox(gather(F), a .* 2, 1e-6);
%=============================================================================
% incompatible sizes
G = gpuArray(single(rand(3, 3)));
H = gpuArray(single(rand(4, 4)));
assert_checkerror('G .* H', _('Arrays have incompatible sizes for this operation.'));
%=============================================================================
