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
% var / std along a dimension match the host result. The default normalization
% is 1/(N-1); the weight 1 selects 1/N.
x = single([2 4 4 4 5 5 7 9]);
X = gpuArray(x);
tol = 1e-4;
assert_isapprox(gather(var(X)), var(x), tol);
assert_isapprox(gather(var(X, 1)), var(x, 1), tol);
assert_isapprox(gather(std(X)), std(x), tol);
assert_isapprox(gather(std(X, 1)), std(x, 1), tol);
%=============================================================================
% per-column and per-row (dim argument)
M = single([1 2; 3 4; 5 6]);
G = gpuArray(M);
assert_isapprox(gather(var(G)), var(M), tol);
assert_isapprox(gather(var(G, 0, 2)), var(M, 0, 2), tol);
assert_isapprox(gather(std(G, 0, 2)), std(M, 0, 2), tol);
%=============================================================================
% variance of a single element is 0; result class is single; result resident
assert_isequal(gather(var(gpuArray(single(5)))), single(0));
assert_isequal(class(gather(var(X))), 'single');
assert_istrue(isgpuarray(std(X)));
%=============================================================================
% integer and complex inputs are rejected, as on the host
assert_checkerror('gather(var(gpuArray(int8([1 2 3]))))', ...
  _('var/std require a floating-point gpuArray.'));
%=============================================================================
% order statistics and set functions (host fallback), re-wrapped as gpuArray
w = single([3 1 4 1 5 9 2 6 5 3]);
W = gpuArray(w);
assert_isapprox(gather(median(W)), median(w), tol);
% the device path stores single; the host mode() of a single returns double
assert_isequal(gather(mode(W)), single(mode(w)));
assert_isapprox(gather(prctile(W, 25)), prctile(w, 25), tol);
assert_isapprox(gather(prctile(W, [25 50 75])), prctile(w, [25 50 75]), tol);
assert_isequal(gather(unique(W)), unique(w));
assert_isequal(gather(ismember(W, gpuArray(single([1 5])))), ismember(w, [1 5]));
assert_isequal(gather(sortrows(gpuArray(single([3 1; 1 2; 2 0])))), ...
  sortrows(single([3 1; 1 2; 2 0])));
assert_istrue(issorted(gpuArray(single([1 2 3 4]))));
assert_isfalse(issorted(gpuArray(single([3 1 2]))));
assert_isequal(gather(nonzeros(gpuArray(single([0 2 0 3])))), nonzeros(single([0 2 0 3])));
% results stay on the device
assert_istrue(isgpuarray(median(W)));
assert_istrue(isgpuarray(unique(W)));
%=============================================================================
