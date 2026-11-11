%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
M = [0.5 0.2 0.9 0.4; 0.1 0.8 0.3 0.6; 0.7 0.35 0.25 0.95];
B = reshape(mod((1:80) * 7, 13), 10, 8) / 13;
%=============================================================================
% full matrices, default k = min(6, min(m, n)), larger k is clipped
assert_isapprox(svds(M), [1.78275; 0.683005; 0.54113], 1e-5);
assert_isequal(size(svds(B)), [6 1]);
assert_isapprox(svds(M, 5), [1.78275; 0.683005; 0.54113], 1e-5);
assert_isapprox(svds(sparse(M)), [1.78275; 0.683005; 0.54113], 1e-5);
R = svds(single(M));
assert_isequal(class(R), 'single');
assert_isapprox(double(R), [1.78275; 0.683005; 0.54113], 1e-5);
assert_isapprox(svds(M + 0.1i * fliplr(M)), [1.79164; 0.686412; 0.543829], 1e-5);
%=============================================================================
% smallest values and numeric sigma come in decreasing order
assert_isapprox(svds(M, 2, 'smallest'), [0.683005; 0.54113], 1e-5);
assert_isapprox(svds(B, 2, 'smallest'), [0.555056; 0.0111017], 1e-5);
assert_isapprox(svds(sparse(M), 2, 'smallest'), [0.683005; 0.54113], 1e-5);
assert_isapprox(svds(M, 2, 0.6), [0.683005; 0.54113], 1e-5);
%=============================================================================
% factors are full, a fourth output reports convergence
[U, S, V, flag] = svds(M, 2);
assert_isequal(size(U), [3 2]);
assert_isequal(size(S), [2 2]);
assert_isequal(size(V), [4 2]);
assert_isfalse(issparse(S));
assert_isequal(flag, 0);
assert_isapprox(abs(U' * M * V), diag([1.78275 0.683005]), 1e-5);
[U, S, V] = svds(sparse(M), 2);
assert_isfalse(issparse(S));
%=============================================================================
% empty cases
assert_isequal(size(svds(M, 0)), [0 1]);
[U, S, V] = svds(zeros(0, 3));
assert_isequal(size(U), [0 0]);
assert_isequal(size(S), [0 0]);
assert_isequal(size(V), [3 0]);
assert_checkerror('svds(M, -1)', _('K must be a nonnegative integer scalar.'), 'Nelson:svds:invalidK');
assert_checkerror('svds(M, 1.5)', _('K must be a nonnegative integer scalar.'), 'Nelson:svds:invalidK');
%=============================================================================
