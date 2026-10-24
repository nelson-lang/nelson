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
a = single([1 5 3; 8 2 6]);
b = single([4 4 4; 4 4 4]);
A = gpuArray(a);
B = gpuArray(b);
%=============================================================================
% relational operators return a logical gpuArray
R = A > B;
assert_istrue(isgpuarray(R));
assert_isequal(underlyingType(R), 'logical');
r = gather(R);
assert_istrue(islogical(r));
assert_isequal(r, a > b);
%=============================================================================
assert_isequal(gather(A < B), a < b);
assert_isequal(gather(A >= B), a >= b);
assert_isequal(gather(A <= B), a <= b);
assert_isequal(gather(A == B), a == b);
assert_isequal(gather(A ~= B), a ~= b);
%=============================================================================
% scalar broadcast
assert_isequal(gather(A > 4), a > 4);
assert_isequal(gather(3 < A), 3 < a);
%=============================================================================
% logical operators
m1 = A > 3;
m2 = A < 7;
assert_isequal(gather(m1 & m2), (a > 3) & (a < 7));
assert_isequal(gather(m1 | m2), (a > 3) | (a < 7));
assert_isequal(gather(~m1), ~(a > 3));
assert_isequal(gather(xor(m1, m2)), xor(a > 3, a < 7));
%=============================================================================
% gpuArray(logical) round-trips as logical
L = gpuArray(a > 4);
assert_isequal(underlyingType(L), 'logical');
assert_isequal(gather(L), a > 4);
%=============================================================================
