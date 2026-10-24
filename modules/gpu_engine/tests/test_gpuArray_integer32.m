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
% int32/uint32 gpuArray: unlike int8/16, 32-bit integers cannot be represented
% exactly in single precision, so the device stores the raw integer bits and
% dedicated kernels operate on them with exact saturating arithmetic. Expected
% values are explicit literals matching saturating-integer semantics.
%=============================================================================
% round-trip preserves class and the exact 32-bit values (including the bounds)
classes = {'int32', 'uint32'};
for k = 1:numel(classes)
  cls = classes{k};
  if strcmp(cls, 'int32')
    h = int32([-2147483648, -3, 0, 5, 2147483647]);
  else
    h = uint32([0, 5, 100, 4294967295]);
  end
  G = gpuArray(h);
  assert_istrue(isgpuarray(G));
  assert_isequal(underlyingType(G), cls);
  r = gather(G);
  assert_isequal(class(r), cls);
  assert_isequal(r, h);
end
%=============================================================================
% signed arithmetic saturates at both bounds and keeps the class
assert_isequal(gather(gpuArray(int32(2000000000)) + int32(2000000000)), int32(2147483647));
assert_isequal(gather(gpuArray(int32(-2000000000)) - int32(2000000000)), int32(-2147483648));
assert_isequal(gather(gpuArray(int32(2000000000)) .* int32(2)), int32(2147483647));
assert_isequal(gather(gpuArray(int32(-2000000000)) .* int32(2)), int32(-2147483648));
assert_isequal(gather(gpuArray(int32(30000)) .* int32(4)), int32(120000)); % exact
assert_isequal(class(gather(gpuArray(int32(1)) + int32(1))), 'int32');
%=============================================================================
% unsigned saturates at 0 and at 2^32-1
assert_isequal(gather(gpuArray(uint32(3)) - uint32(5)), uint32(0));
assert_isequal(gather(gpuArray(uint32(4000000000)) + uint32(1000000000)), uint32(4294967295));
assert_isequal(gather(gpuArray(uint32(100000)) .* uint32(100000)), uint32(4294967295));
%=============================================================================
% unary uminus / abs saturate against the asymmetric signed range
assert_isequal(gather(-gpuArray(int32(-2147483648))), int32(2147483647));
assert_isequal(gather(abs(gpuArray(int32(-2147483648)))), int32(2147483647));
assert_isequal(gather(-gpuArray(uint32(7))), uint32(0));
%=============================================================================
% scalar broadcast: a real scalar promotes to the integer class
assert_isequal(gather(gpuArray(int32([10 20 30])) + 5), int32([15 25 35]));
assert_isequal(gather(gpuArray(int32([10 20 30])) .* 2), int32([20 40 60]));
%=============================================================================
% division rounds half away from zero and keeps the class; x/0 saturates by
% sign; INT32_MIN/-1 saturates to the maximum
d1 = gpuArray(int32([7 5 -7 -5 8 1 -1 0 -2147483648]));
d2 = gpuArray(int32([2 2 2 2 3 0 0 0 -1]));
assert_isequal(gather(d1 ./ d2), int32([4 3 -4 -3 3 2147483647 -2147483648 0 2147483647]));
assert_isequal(gather(gpuArray(uint32([7 5])) ./ uint32([2 0])), uint32([4 4294967295]));
assert_isequal(gather(gpuArray(int32(2)) .\ int32(7)), int32(4));
assert_isequal(gather(gpuArray(int32([10 20 30])) ./ 3), int32([3 7 10]));
assert_isequal(class(gather(gpuArray(int32(7)) ./ int32(2))), 'int32');
%=============================================================================
% binary max/min keep the class
assert_isequal(gather(max(gpuArray(int32([-5 40 12])), int32(0))), int32([0 40 12]));
assert_isequal(gather(min(gpuArray(uint32([5 40 12])), uint32(10))), uint32([5 10 10]));
%=============================================================================
% reductions: sum/prod promote to a floating result (no saturation), like the
% host default; max/min reduce and keep the integer class
assert_isequal(gather(sum(gpuArray(int32([2000000000 2000000000])))), single(4000000000));
assert_isequal(gather(prod(gpuArray(int32([100000 100000])))), single(10000000000));
assert_isequal(gather(sum(gpuArray(uint32([4000000000 1000000000])))), single(5000000000));
assert_isequal(gather(sum(gpuArray(int32([1 2; 3 4])))), single([4 6]));
assert_isequal(gather(sum(gpuArray(int32([1 2; 3 4])), 2)), single([3; 7]));
assert_isequal(gather(max(gpuArray(int32([-5 40 12 -80])))), int32(40));
assert_isequal(gather(min(gpuArray(int32([-5 40 12 -80])))), int32(-80));
assert_isequal(class(gather(sum(gpuArray(int32([1 2 3]))))), 'single');
assert_isequal(class(gather(max(gpuArray(int32([1 2 3]))))), 'int32');
assert_isequal(gather(mean(gpuArray(int32([1 2 3 4])))), single(2.5));
assert_isequal(class(gather(mean(gpuArray(int32([1 2 3 4]))))), 'single');
%=============================================================================
% scans (cumsum/cumprod) saturate at each step and keep the integer class
assert_isequal(gather(cumsum(gpuArray(int32([2000000000 2000000000])))), ...
  int32([2000000000 2147483647]));
assert_isequal(gather(cumsum(gpuArray(int32([1 2; 3 4])))), int32([1 2; 4 6]));
assert_isequal(gather(cumprod(gpuArray(uint32([100000 100000])))), uint32([100000 4294967295]));
assert_isequal(class(gather(cumsum(gpuArray(int32([1 2 3]))))), 'int32');
%=============================================================================
% relational operators compare exactly (logical result)
a = int32([1 5 3 -8]);
b = int32([2 2 2 2]);
A = gpuArray(a);
B = gpuArray(b);
assert_isequal(gather(A > B), a > b);
assert_isequal(gather(A == B), a == b);
assert_isequal(gather(A <= B), a <= b);
assert_isequal(class(gather(A > B)), 'logical');
%=============================================================================
% byte-moving operations work on the raw storage (class preserved)
m = int32([1 2 3; 4 5 6]);
M = gpuArray(m);
assert_isequal(gather(M'), m');
assert_isequal(gather(reshape(M, 3, 2)), reshape(m, 3, 2));
assert_isequal(underlyingType(M'), 'int32');
%=============================================================================
% two integer gpuArrays of different classes cannot be combined
assert_checkerror('gather(gpuArray(int32(1)) + gpuArray(int8(1)))', ...
  _('Integer types must match.'));
%=============================================================================
% sort keeps the integer class, ascending and descending, with indices
assert_isequal(gather(sort(gpuArray(int32([5 -3 2147483647 -2147483648 0])))), ...
  int32([-2147483648 -3 0 5 2147483647]));
[sv, si] = sort(gpuArray(int32([5 1 3])));
assert_isequal(gather(sv), int32([1 3 5]));
assert_isequal(gather(si), single([2 3 1]));
assert_isequal(gather(sort(gpuArray(int32([5 1 3])), 'descend')), int32([5 3 1]));
assert_isequal(gather(sort(gpuArray(uint32([5 4000000000 1])))), uint32([1 5 4000000000]));
assert_isequal(class(gather(sort(gpuArray(int32([3 1 2]))))), 'int32');
%=============================================================================
% find returns the 1-based positions of the non-zero elements
assert_isequal(gather(find(gpuArray(int32([0 0 7 0 -2000000000])))), single([3 5]));
assert_isequal(gather(find(gpuArray(uint32([0 4000000000 0])))), single(2));
%=============================================================================
% power keeps the class, saturates on overflow and is exact when it fits;
% a negative exponent is an error
assert_isequal(gather(gpuArray(int32([2 10 46341 2 -46341 1290 -2])) .^ ...
  int32([3 3 2 31 2 3 31])), ...
  int32([8 1000 2147483647 2147483647 2147483647 2146689000 -2147483648]));
assert_isequal(gather(gpuArray(uint32([100000 3 65536])) .^ uint32([2 6 2])), ...
  uint32([4294967295 729 4294967295]));
assert_isequal(gather(gpuArray(int32([0 5 -3])) .^ int32(0)), int32([1 1 1]));
assert_isequal(class(gather(gpuArray(int32(2)) .^ int32(3))), 'int32');
assert_checkerror('gather(gpuArray(int32(2)) .^ int32(-1))', ...
  _('Integers can only be raised to positive integral powers.'));
%=============================================================================
