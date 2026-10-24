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
% The small integer classes (int8/16, uint8/16) round-trip through the device
% and reproduce saturating integer arithmetic. Expected values are written as
% explicit literals matching the saturating-integer semantics, on purpose: the
% device saturates every result, whereas the host currently wraps integer
% element-wise multiplication and promotes integer sum() to double.
%=============================================================================
% round-trip preserves class and values
classes = {'int8', 'uint8', 'int16', 'uint16'};
for k = 1:numel(classes)
  cls = classes{k};
  h = cast([-3 0 5; 10 -128 127], cls);
  G = gpuArray(h);
  assert_istrue(isgpuarray(G));
  assert_isequal(underlyingType(G), cls);
  r = gather(G);
  assert_isequal(class(r), cls);
  assert_isequal(r, h);
end
%=============================================================================
% element-wise arithmetic keeps the integer class and saturates
a = int8([100 -100 40 -40]);
b = int8([50 -50 3 3]);
A = gpuArray(a);
B = gpuArray(b);
assert_isequal(gather(A + B), int8([127 -128 43 -37]));
assert_isequal(gather(A - B), int8([50 -50 37 -43]));
assert_isequal(gather(A .* B), int8([127 127 120 -120]));
assert_isequal(class(gather(A + B)), 'int8');
%=============================================================================
% unsigned saturates at zero and at the upper bound
U = gpuArray(uint8([10 200 5]));
V = gpuArray(uint8([20 100 5]));
assert_isequal(gather(U - V), uint8([0 100 0]));
assert_isequal(gather(U + V), uint8([30 255 10]));
%=============================================================================
% unary abs / uminus saturate against the asymmetric range
assert_isequal(gather(abs(gpuArray(int8(-128)))), int8(127));
assert_isequal(gather(-gpuArray(uint8(5))), uint8(0));
assert_isequal(gather(-gpuArray(int8(-128))), int8(127));
%=============================================================================
% division rounds half away from zero
D1 = gpuArray(int8([7 -7 5 -5 9]));
D2 = gpuArray(int8([2 2 2 2 4]));
assert_isequal(gather(D1 ./ D2), int8([4 -4 3 -3 2]));
%=============================================================================
% scalar broadcast keeps the integer class (a real scalar promotes to it)
assert_isequal(gather(gpuArray(int16([100 200 300])) + 50), int16([150 250 350]));
assert_isequal(gather(gpuArray(int8([10 20 30])) .* 2), int8([20 40 60]));
%=============================================================================
% reductions: sum/prod promote to a floating result (no saturation), like the
% host default; max/min reduce and keep the integer class
assert_isequal(gather(sum(gpuArray(int8([100 100 100])))), single(300));
assert_isequal(class(gather(sum(gpuArray(int8([1 2 3]))))), 'single');
assert_isequal(gather(prod(gpuArray(int8([10 10 3])))), single(300));
M = gpuArray(int16([-5 40 12 -80 3]));
assert_isequal(gather(max(M)), int16(40));
assert_isequal(gather(min(M)), int16(-80));
assert_isequal(class(gather(max(M))), 'int16');
%=============================================================================
% scans (cumsum/cumprod) saturate at each step and keep the integer class
assert_isequal(gather(cumsum(gpuArray(int8([100 100 -100])))), int8([100 127 27]));
assert_isequal(gather(cumsum(gpuArray(int8([100 100 100])))), int8([100 127 127]));
assert_isequal(gather(cumprod(gpuArray(int8([10 10 3])))), int8([10 100 127]));
assert_isequal(class(gather(cumsum(gpuArray(int8([1 2 3]))))), 'int8');
%=============================================================================
% sort keeps the integer class
assert_isequal(gather(sort(gpuArray(int8([5 -3 2 -1 0])))), int8([-3 -1 0 2 5]));
assert_isequal(class(gather(sort(gpuArray(int16([3 1 2]))))), 'int16');
%=============================================================================
% find returns the 1-based positions of the non-zero elements
assert_isequal(gather(find(gpuArray(int8([0 5 0 -3 0])))), single([2 4]));
%=============================================================================
% power keeps the class and saturates; negative base with odd exponent stays
% negative; a negative exponent is an error
assert_isequal(gather(gpuArray(int8([10 -2 2])) .^ int8([3 7 3])), int8([127 -128 8]));
assert_isequal(gather(gpuArray(uint8(3)) .^ uint8(6)), uint8(255));
assert_checkerror('gather(gpuArray(int8(2)) .^ int8(-1))', ...
  _('Integers can only be raised to positive integral powers.'));
%=============================================================================
% relational operators compare exactly (result is logical)
assert_isequal(gather(A > B), a > b);
assert_isequal(gather(A == B), a == b);
assert_isequal(class(gather(A > B)), 'logical');
%=============================================================================
% two integer gpuArrays of different classes cannot be combined
assert_checkerror('gather(gpuArray(int8(1)) + gpuArray(int16(1)))', ...
  _('Integer types must match.'));
%=============================================================================
