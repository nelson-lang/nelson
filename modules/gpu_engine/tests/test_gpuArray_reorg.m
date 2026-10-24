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
a = single(reshape(1:24, 4, 6));
A = gpuArray(a);
%=============================================================================
% transpose (.' and ')
assert_isequal(gather(A.'), a.');
assert_isequal(gather(A'), a');
assert_isequal(size(gather(A.')), [6 4]);
%=============================================================================
% reshape
assert_isequal(gather(reshape(A, 6, 4)), reshape(a, 6, 4));
assert_isequal(gather(reshape(A, 2, 12)), reshape(a, 2, 12));
assert_isequal(gather(reshape(A, [3 8])), reshape(a, [3 8]));
assert_isequal(gather(reshape(A, 8, [])), reshape(a, 8, []));
assert_isequal(gather(reshape(A, [], 2)), reshape(a, [], 2));
%=============================================================================
% linear indexing
assert_isequal(gather(A(5)), a(5));
assert_isequal(gather(A([1 4 7 10])), a([1 4 7 10]));
assert_isequal(gather(A(:)), a(:));
idx = [2 5; 8 11];
assert_isequal(gather(A(idx)), a(idx));
%=============================================================================
% 2-D indexing
assert_isequal(gather(A(2, 3)), a(2, 3));
assert_isequal(gather(A([1 3], [2 4 6])), a([1 3], [2 4 6]));
assert_isequal(gather(A(:, 2)), a(:, 2));
assert_isequal(gather(A(3, :)), a(3, :));
%=============================================================================
% indexing preserves logical type
L = A > 10;
assert_isequal(underlyingType(L(:)), 'logical');
assert_isequal(gather(L(1:5)), a(1:5) > 10);
%=============================================================================
% out-of-range index errors (message wording depends on call context)
errorThrown = false;
try
  z = A(100);
catch
  errorThrown = true;
end
assert_istrue(errorThrown);
%=============================================================================
% N-D indexed read: three-subscript access into a 3-D gpuArray
nd = single(reshape(1:24, 2, 3, 4));
G = gpuArray(nd);
assert_isequal(gather(G(2, 3, 4)), nd(2, 3, 4));
assert_isequal(gather(G(1, :, 2)), nd(1, :, 2));
assert_isequal(gather(G(:, 2:3, [1 4])), nd(:, 2:3, [1 4]));
assert_isequal(gather(G(:, :, 3)), nd(:, :, 3));
assert_isequal(gather(G([2 1], [3 1], [4 2])), nd([2 1], [3 1], [4 2]));
% partial subscripts default the trailing dimensions to index 1 (host rule)
assert_isequal(gather(G(2, 3)), nd(2, 3));
% complex and integer classes keep their storage through N-D indexing
zc = nd + 1i * single(reshape(24:-1:1, 2, 3, 4));
GZ = gpuArray(zc);
assert_isequal(gather(GZ(:, 2, 3)), zc(:, 2, 3));
GIr = gpuArray(int16(nd));
assert_isequal(underlyingType(GIr(2, :, 3)), 'int16');
%=============================================================================
% N-D indexed assignment (value semantics: the input is untouched)
B = nd;
GB = gpuArray(nd);
GB(1, 2, 3) = single(99);
B(1, 2, 3) = 99;
assert_isequal(gather(GB), B);
GB(:, 1, 2) = single([7; 8]);
B(:, 1, 2) = [7; 8];
assert_isequal(gather(GB), B);
GB(:, :, 4) = single(zeros(2, 3));
B(:, :, 4) = 0;
assert_isequal(gather(GB), B);
% scalar broadcast into an N-D region
GB(1, [1 3], [2 4]) = single(-5);
B(1, [1 3], [2 4]) = -5;
assert_isequal(gather(GB), B);
% the source gpuArray was not modified
assert_isequal(gather(G(1, 2, 3)), nd(1, 2, 3));
% integer class preserved on assignment
GI = gpuArray(int16(nd));
GI(1, 1, 1) = int16(100);
assert_isequal(underlyingType(GI), 'int16');
assert_isequal(double(gather(GI(1, 1, 1))), 100);
%=============================================================================
