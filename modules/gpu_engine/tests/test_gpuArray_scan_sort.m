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
a = single([3 1 2; 9 5 7; 4 8 6]);
A = gpuArray(a);
tol = 1e-4;
%=============================================================================
% cumsum / cumprod (default dim = 1)
assert_isequal(gather(cumsum(A)), cumsum(a));
assert_isequal(gather(cumsum(A, 2)), cumsum(a, 2));
assert_isapprox(gather(cumprod(A)), cumprod(a), tol);
assert_isapprox(gather(cumprod(A, 2)), cumprod(a, 2), tol);
assert_istrue(isgpuarray(cumsum(A)));
assert_isequal(size(gather(cumsum(A))), [3 3]);
%=============================================================================
% cumsum on a vector
v = single([1 2 3 4 5]);
assert_isequal(gather(cumsum(gpuArray(v))), cumsum(v));
%=============================================================================
% sort (ascending default, dim 1)
assert_isequal(gather(sort(A)), sort(a));
assert_isequal(gather(sort(A, 2)), sort(a, 2));
assert_isequal(gather(sort(A, 'descend')), sort(a, 'descend'));
assert_isequal(gather(sort(A, 2, 'descend')), sort(a, 2, 'descend'));
assert_istrue(isgpuarray(sort(A)));
%=============================================================================
% sort with index output
[s, i] = sort(A);
[refs, refi] = sort(a);
assert_isequal(gather(s), refs);
assert_isequal(gather(i), single(refi));
%=============================================================================
% find (linear indices of non-zeros), device resident
m = single([0 3 0; 5 0 0]);
M = gpuArray(m);
assert_isequal(gather(find(M)), single(find(m)));
assert_istrue(isgpuarray(find(M)));
% row vector -> row result
rv = single([0 1 0 2 0]);
assert_isequal(gather(find(gpuArray(rv))), single(find(rv)));
%=============================================================================
