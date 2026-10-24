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
A = single([1 2 3; 4 5 6]);
G = gpuArray(A);
%=============================================================================
% flip / rot90
assert_isequal(gather(flip(G)), flip(A));
assert_isequal(gather(flip(G, 1)), flip(A, 1));
assert_isequal(gather(flip(G, 2)), flip(A, 2));
assert_isequal(gather(rot90(G)), rot90(A));
assert_isequal(gather(rot90(G, 2)), rot90(A, 2));
assert_isequal(gather(rot90(G, 3)), rot90(A, 3));
assert_istrue(isgpuarray(flip(G)));
%=============================================================================
% circshift (positive, negative, per dimension)
assert_isequal(gather(circshift(G, 1, 2)), circshift(A, 1, 2));
assert_isequal(gather(circshift(G, -1, 1)), circshift(A, -1, 1));
assert_isequal(gather(circshift(G, 4, 2)), circshift(A, 4, 2));
%=============================================================================
% repmat (scalar factor, two factors, vector factor)
assert_isequal(gather(repmat(G, 2)), repmat(A, 2));
assert_isequal(gather(repmat(G, 2, 3)), repmat(A, 2, 3));
assert_isequal(gather(repmat(G, [1 3])), repmat(A, [1 3]));
%=============================================================================
% diff (order and dimension)
assert_isequal(gather(diff(G)), diff(A));
assert_isequal(gather(diff(G, 1, 2)), diff(A, 1, 2));
assert_isequal(gather(diff(G, 2, 2)), diff(A, 2, 2));
%=============================================================================
% kron / trace / cross
assert_isequal(gather(kron(gpuArray(single([1 2])), gpuArray(single([1 0; 0 1])))), ...
  kron(single([1 2]), single([1 0; 0 1])));
assert_isequal(gather(trace(gpuArray(single([1 2; 3 4])))), trace(single([1 2; 3 4])));
assert_isequal(gather(cross(gpuArray(single([1 0 0])), gpuArray(single([0 1 0])))), ...
  cross(single([1 0 0]), single([0 1 0])));
%=============================================================================
% cummax / cummin
v = single([3 1 4 1 5 9 2 6]);
assert_isequal(gather(cummax(gpuArray(v))), cummax(v));
assert_isequal(gather(cummin(gpuArray(v))), cummin(v));
assert_isequal(gather(cummax(gpuArray(A), 2)), cummax(A, 2));
%=============================================================================
% integer inputs keep their class (data movement is exact)
assert_isequal(gather(flip(gpuArray(int8([1 2 3])))), int8([3 2 1]));
assert_isequal(underlyingType(circshift(gpuArray(int16([1 2 3])), 1)), 'int16');
assert_isequal(gather(repmat(gpuArray(int32([1 2])), 1, 2)), int32([1 2 1 2]));
assert_isequal(gather(trace(gpuArray(int8([1 2; 3 4])))), int8(5));
assert_isequal(gather(cummax(gpuArray(int8([1 3 2 5 4])))), int8([1 3 3 5 5]));
%=============================================================================
% flip works on a complex gpuArray
z = single([1 + 1i 2 - 2i; 3 + 3i 4 - 4i]);
assert_isequal(gather(flip(gpuArray(z), 2)), flip(z, 2));
%=============================================================================
% triu / tril on the device, with the k diagonal offset
M = single(magic(5));
GM = gpuArray(M);
assert_isequal(gather(triu(GM)), triu(M));
assert_isequal(gather(tril(GM)), tril(M));
assert_isequal(gather(triu(GM, 1)), triu(M, 1));
assert_isequal(gather(triu(GM, -2)), triu(M, -2));
assert_isequal(gather(tril(GM, 1)), tril(M, 1));
assert_isequal(gather(tril(GM, -1)), tril(M, -1));
% non-square keeps the shape
R = single(reshape(1:24, 4, 6));
assert_isequal(gather(triu(gpuArray(R), 2)), triu(R, 2));
assert_isequal(gather(tril(gpuArray(R), -1)), tril(R, -1));
% integer class and complex layout are preserved
assert_isequal(gather(triu(gpuArray(int16(magic(4))))), int16(triu(magic(4))));
zc = single([1 + 1i 2 - 2i; 3 + 3i 4 - 4i]);
assert_isequal(gather(triu(gpuArray(zc), 0)), triu(zc));
assert_istrue(isgpuarray(triu(GM)));
%=============================================================================
% fliplr / flipud / squeeze / diag
assert_isequal(gather(fliplr(GM)), fliplr(M));
assert_isequal(gather(flipud(GM)), flipud(M));
nd = single(reshape(1:6, 2, 1, 3));
assert_isequal(gather(squeeze(gpuArray(nd))), squeeze(nd));
assert_isequal(gather(diag(GM)), diag(M));
assert_isequal(gather(diag(gpuArray(single(1:4)))), diag(single(1:4)));
%=============================================================================
