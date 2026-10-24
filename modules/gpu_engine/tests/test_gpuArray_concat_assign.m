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
a = single(reshape(1:12, 3, 4));
b = single(reshape(101:112, 3, 4));
A = gpuArray(a);
B = gpuArray(b);
%=============================================================================
% horizontal / vertical concatenation
assert_isequal(gather([A, B]), [a, b]);
assert_isequal(gather([A; B]), [a; b]);
assert_isequal(size(gather([A, B])), [3 8]);
assert_isequal(size(gather([A; B])), [6 4]);
assert_istrue(isgpuarray([A, B]));
%=============================================================================
% multi-way and mixed host/device
assert_isequal(gather([A, B, A]), [a, b, a]);
assert_isequal(gather([A, single(ones(3, 2))]), [a, single(ones(3, 2))]);
%=============================================================================
% cat
assert_isequal(gather(cat(1, A, B)), cat(1, a, b));
assert_isequal(gather(cat(2, A, B)), cat(2, a, b));
%=============================================================================
% indexed assignment (linear), value semantics: A unchanged
C = A;
C(5) = single(999);
assert_isequal(gather(C(5)), single(999));
assert_isequal(gather(A(5)), a(5)); % original intact
ref = a; ref(5) = 999;
assert_isequal(gather(C), ref);
%=============================================================================
% linear assignment with a vector RHS
D = A;
D([1 4 7]) = single([10 20 30]);
ref2 = a; ref2([1 4 7]) = [10 20 30];
assert_isequal(gather(D), ref2);
%=============================================================================
% scalar broadcast assignment
E = A;
E([2 5 8]) = single(0);
ref3 = a; ref3([2 5 8]) = 0;
assert_isequal(gather(E), ref3);
%=============================================================================
% 2-D assignment
F = A;
F(2, 3) = single(-7);
ref4 = a; ref4(2, 3) = -7;
assert_isequal(gather(F), ref4);
F([1 3], [2 4]) = single(5);
ref4([1 3], [2 4]) = 5;
assert_isequal(gather(F), ref4);
%=============================================================================
% colon assignment
G = A;
G(:, 2) = single([7; 8; 9]);
ref5 = a; ref5(:, 2) = [7; 8; 9];
assert_isequal(gather(G), ref5);
%=============================================================================
% any / all
m = single([0 2 0; 0 0 3]);
M = gpuArray(m);
assert_isequal(gather(any(M)), any(m));
assert_isequal(gather(all(M)), all(m));
assert_isequal(gather(any(M, 2)), any(m, 2));
assert_isequal(gather(all(M, 2)), all(m, 2));
assert_isequal(underlyingType(any(M)), 'logical');
%=============================================================================
% concatenation keeps a uniform integer class and casts non-integer parts
assert_isequal(underlyingType([gpuArray(int8([1 2])) gpuArray(int8([3 4]))]), 'int8');
assert_isequal(gather([gpuArray(int16([1; 2])); gpuArray(int16([3; 4]))]), int16([1; 2; 3; 4]));
assert_isequal(underlyingType([gpuArray(int32([1 2])) gpuArray(int32([3 4]))]), 'int32');
assert_isequal(gather([gpuArray(int8(1)) gpuArray(single(2.7))]), int8([1 3]));
% two different integer classes cannot be mixed
assert_checkerror('[gpuArray(int8(1)) gpuArray(int16(1))]', _('Integer types must match.'));
% int32/uint32 (raw-bit storage) cannot be mixed with another class
assert_checkerror('[gpuArray(int32(1)) gpuArray(single(1))]', ...
  _('int32/uint32 gpuArray concatenation requires all inputs of the same class.'));
%=============================================================================
% N-D concatenation (dim >= 3) now dispatches to the gpuArray cat overload
% through the native cat builtin (was blocked while cat was a host macro).
a = single([1 2; 3 4]);
b = single([5 6; 7 8]);
ga = gpuArray(a);
gb = gpuArray(b);
c3 = cat(3, ga, gb);
assert_istrue(isgpuarray(c3));
assert_isequal(gather(c3), cat(3, a, b));
assert_isequal(size(c3), [2 2 2]);
assert_isequal(gather(cat(4, ga, gb)), cat(4, a, b));
% three operands along dim 3
assert_isequal(gather(cat(3, ga, gb, ga)), cat(3, a, b, a));
% dim 1 / 2 still work through the same overload
assert_isequal(gather(cat(1, ga, gb)), [a; b]);
assert_isequal(gather(cat(2, ga, gb)), [a, b]);
%=============================================================================
