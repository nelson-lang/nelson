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
A = single(reshape(1:24, 2, 3, 4));
G = gpuArray(A);
%=============================================================================
% permute, ipermute, shiftdim
r = permute(G, [3 1 2]);
assert_istrue(isgpuarray(r));
assert_isequal(gather(r), permute(A, [3 1 2]));
assert_isequal(gather(permute(G, [2 1 3 4])), permute(A, [2 1 3 4]));
assert_isequal(gather(ipermute(permute(G, [3 1 2]), [3 1 2])), A);
assert_isequal(gather(shiftdim(G, 1)), shiftdim(A, 1));
assert_isequal(size(shiftdim(G, -2)), [1 1 2 3 4]);
[b, n] = shiftdim(gpuArray(single(ones(1, 1, 3))));
assert_isequal(size(b), [3 1]);
assert_isequal(n, 2);
I = gpuArray(int32([1 2; 3 4]));
assert_isequal(gather(permute(I, [2 1])), int32([1 3; 2 4]));
Z = gpuArray(single([1 + 2i, 3; 4, 5 - 1i]));
assert_isequal(gather(permute(Z, [2 1])), single([1 + 2i, 4; 3, 5 - 1i]));
%=============================================================================
% repmat, repelem, circshift
M = single([1 2; 3 4]);
assert_isequal(gather(repmat(gpuArray(M), 2, 3)), repmat(M, 2, 3));
assert_isequal(gather(repmat(gpuArray(M), [1 2 2])), repmat(M, [1 2 2]));
assert_isequal(gather(repmat(gpuArray(M), gpuArray(2))), repmat(M, 2));
assert_isequal(gather(repelem(gpuArray(single([1 2 3])), 2)), single([1 1 2 2 3 3]));
assert_isequal(gather(repelem(gpuArray(single([1; 2])), 3)), single([1; 1; 1; 2; 2; 2]));
assert_isequal(gather(repelem(gpuArray(M), 2, 3)), repelem(M, 2, 3));
assert_isequal(gather(circshift(G, 1)), circshift(A, 1));
assert_isequal(gather(circshift(G, [1 -1 2])), circshift(A, [1 -1 2]));
assert_isequal(gather(circshift(G, 2, 3)), circshift(A, 2, 3));
assert_isequal(gather(circshift(gpuArray(single(1:5)), -2)), single([3 4 5 1 2]));
%=============================================================================
% grids
x = single(1:3);
y = single([10 20]);
[X, Y] = meshgrid(gpuArray(x), gpuArray(y));
[RX, RY] = meshgrid(x, y);
assert_isequal(gather(X), RX);
assert_isequal(gather(Y), RY);
[X, Y, Zg] = meshgrid(gpuArray(x), y, gpuArray(single([5 6])));
[RX, RY, RZ] = meshgrid(x, y, single([5 6]));
assert_isequal(gather(X), RX);
assert_isequal(gather(Zg), RZ);
assert_isequal(gather(meshgrid(gpuArray(x))), meshgrid(x));
[P, Q] = ndgrid(gpuArray(x), gpuArray(y));
[RP, RQ] = ndgrid(x, y);
assert_isequal(gather(P), RP);
assert_isequal(gather(Q), RQ);
%=============================================================================
% fftshift / ifftshift
v = single(1:5);
assert_isequal(gather(fftshift(gpuArray(v))), fftshift(v));
assert_isequal(gather(ifftshift(gpuArray(v))), ifftshift(v));
assert_isequal(gather(fftshift(G)), fftshift(A));
assert_isequal(gather(fftshift(G, 3)), fftshift(A, 3));
assert_isequal(gather(ifftshift(fftshift(G))), A);
%=============================================================================
