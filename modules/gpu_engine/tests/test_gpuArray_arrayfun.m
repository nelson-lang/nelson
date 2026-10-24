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
a = single(rand(4, 5)) + 0.5;
b = single(rand(4, 5)) + 0.5;
c = single(rand(4, 5)) + 0.5;
A = gpuArray(a); B = gpuArray(b); C = gpuArray(c);
tol = 1e-5;
%=============================================================================
% fused elementwise expression (2 inputs), result device resident
R = arrayfun(@(x, y) x .* y + sin(x), A, B);
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), a .* b + sin(a), 1e-4);
%=============================================================================
% single input, several functions
assert_isapprox(gather(arrayfun(@(x) sqrt(x) + exp(-x), A)), sqrt(a) + exp(-a), 1e-4);
assert_isapprox(gather(arrayfun(@(x) abs(x - 1), A)), abs(a - 1), tol);
%=============================================================================
% three inputs
assert_isapprox(gather(arrayfun(@(x, y, z) x .* y - z, A, B, C)), a .* b - c, 1e-4);
%=============================================================================
% arithmetic subset: /, \, ^
assert_isapprox(gather(arrayfun(@(x, y) x ./ y, A, B)), a ./ b, 1e-4);
assert_isapprox(gather(arrayfun(@(x, y) x .^ y, A, B)), a .^ b, 1e-3);
%=============================================================================
% comparison / logical -> logical gpuArray
M = arrayfun(@(x, y) x > y, A, B);
assert_isequal(underlyingType(M), 'logical');
assert_isequal(gather(M), a > b);
assert_isequal(gather(arrayfun(@(x, y)(x > 0.7) & (y < 0.9), A, B)), (a > 0.7) & (b < 0.9));
%=============================================================================
% captured scalar constant is inlined
k = single(3.25);
assert_isapprox(gather(arrayfun(@(x) x .* k + 1, A)), a .* k + 1, 1e-4);
%=============================================================================
% mixed host/device inputs
assert_isapprox(gather(arrayfun(@(x, y) x .* y, A, b)), a .* b, 1e-4);
%=============================================================================
% min / max / mod scalar functions
assert_isapprox(gather(arrayfun(@(x, y) max(x, y), A, B)), max(a, b), tol);
assert_isapprox(gather(arrayfun(@(x, y) min(x, y), A, B)), min(a, b), tol);
%=============================================================================
% non-translatable body -> CPU fallback, still correct
F = arrayfun(@(x) x(1), A); % indexing -> not translatable
assert_isequal(gather(F), a);
%=============================================================================
% UniformOutput,false -> CPU fallback (returns a cell)
G = arrayfun(@(x) x * 2, A, 'UniformOutput', false);
assert_istrue(iscell(G));
%=============================================================================
% small integer classes (int8/uint8/int16) run on the device with saturating
% i32 semantics; the result keeps the class.
r = arrayfun(@(x) x + 1, gpuArray(int8([10 100 127])));
assert_isequal(underlyingType(r), 'int8');
assert_isequal(gather(r), int8([11 101 127])); % 127+1 saturates
ru = arrayfun(@(x) x * 2, gpuArray(uint8([100 200])));
assert_isequal(underlyingType(ru), 'uint8');
assert_isequal(gather(ru), uint8([200 255])); % 400 saturates to 255
%=============================================================================
% saturation is applied at every step (not only at the end): (x + x) saturates
% before the subtraction, so x + x - x is not x for a saturating class.
assert_isequal(gather(arrayfun(@(x) x + x - x, gpuArray(int8(100)))), int8(27));
% multiply saturates to the class range
assert_isequal(gather(arrayfun(@(x, y) x .* y, gpuArray(int8(50)), gpuArray(int8(50)))), int8(127));
assert_isequal(gather(arrayfun(@(x, y) x .* y, gpuArray(int16(200)), gpuArray(int16(200)))), ...
  int16(32767));
% unary minus, abs and min/max
assert_isequal(gather(arrayfun(@(x) - x, gpuArray(int8([-128 50])))), int8([127 -50]));
assert_isequal(gather(arrayfun(@(x) - x, gpuArray(uint8(5)))), uint8(0)); % unsigned saturates to 0
assert_isequal(gather(arrayfun(@(x) abs(x), gpuArray(int8(-128)))), int8(127));
assert_isequal(gather(arrayfun(@(x, y) max(x, y), gpuArray(int8([1 9])), gpuArray(int8([4 4])))), ...
  int8([4 9]));
% a comparison root yields a logical result
cmp = arrayfun(@(x, y) x > y, gpuArray(int8([5 1])), gpuArray(int8([3 4])));
assert_isequal(underlyingType(cmp), 'logical');
assert_isequal(gather(cmp), logical([1 0]));
% a captured integer scalar is inlined and saturates with the expression
assert_isequal(gather(arrayfun(@(x) x + int8(100), gpuArray(int8([50 -50])))), int8([127 50]));
% unsupported ops in an integer expression fall back to the host (still correct)
assert_isequal(gather(arrayfun(@(x, y) x ./ y, gpuArray(int8([10 7])), gpuArray(int8([3 2])))), ...
  int8([10 7]) ./ int8([3 2]));
% uint16 / int32 are not accelerated but still work through the fallback
assert_isequal(gather(arrayfun(@(x, y) x + y, gpuArray(uint16(60000)), gpuArray(uint16(10000)))), ...
  uint16(65535));
%=============================================================================
