%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
A = sparse([4 1 0; 2 3 1; 0 1 2]);
b = [1; 2; 3];
x0 = ones(3, 1);
[x0r, flag0, relres0, iter0, resvec0] = gmres(A, b, [], 1e-12, 20, [], [], x0);
asserts.isequal(flag0, 0);
asserts.istrue(relres0 < 1e-12);
asserts.isequal(iter0, [1 3]);
asserts.isapprox(resvec0(1:3), [5.6568542494923806; 0.79211803438134; 0.278693205716647], 1e-12);
asserts.istrue(resvec0(end) < 1e-12);
asserts.isapprox(x0r, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * x0r, b, 1e-10);
%=============================================================================
[x0s, flag0s, relres0s, iter0s] = gmres(A, b, [], 1e-12, 20, [], [], sparse(x0));
asserts.isequal(flag0s, 0);
asserts.istrue(relres0s < 1e-12);
asserts.isequal(iter0s, [1 3]);
asserts.isapprox(x0s, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * x0s, b, 1e-10);
x0c = 1i * ones(3, 1);
[x0cResult, flag0c, relres0c] = gmres(A, b, [], 1e-12, 20, [], [], x0c);
asserts.isequal(flag0c, 0);
asserts.istrue(relres0c < 1e-12);
asserts.istrue(norm(A * x0cResult - b) < 1e-10);
%=============================================================================
[xm, flagm, relresm, iterm, resvecm] = gmres(A, b, 1, 1e-16, 1);
REF_LIMITED_X = [0.23529411764705876; 0.47058823529411764; 0.70588235294117641];
asserts.isequal(flagm, 1);
asserts.isapprox(relresm, 0.35503580124836315, 1e-12);
asserts.isequal(iterm, [1 1]);
asserts.isapprox(xm, REF_LIMITED_X, 1e-12);
asserts.isapprox(resvecm, [3.7416573867739413; 1.3284223283101431], 1e-12);
%=============================================================================
N = 16;
e = ones(N, 1);
G = spdiags([-e 4 * e -e], [-1 0 1], N, N) + spdiags([-e -e], [-4 4], N, N) + spdiags((0.1 * (1:N))', 1, N, N);
g = (1:N)';
[~, flaglong, relreslong, iterlong, resveclong] = gmres(G, g, 1, 1e-8, 20);
asserts.isequal(flaglong, 1);
asserts.isapprox(relreslong, 0.00013836863724864146, 1e-12);
asserts.isequal(iterlong, [20 1]);
asserts.isequal(length(resveclong), 21);
asserts.isapprox(resveclong(end) / norm(g), relreslong, 1e-12);
%=============================================================================
[xzeroit, flagzeroit, relreszeroit, iterzeroit, resveczeroit] = gmres(A, b, [], 1e-12, 0);
asserts.isequal(flagzeroit, 1);
asserts.isequal(relreszeroit, 1);
asserts.isequal(iterzeroit, [1 0]);
asserts.isapprox(xzeroit, zeros(3, 1), 1e-12);
asserts.isapprox(resveczeroit, 3.7416573867739413, 1e-12);
%=============================================================================
[xzr, flagzr, relreszr, iterzr, resveczr] = gmres(A, zeros(3, 1), [], 1e-12, 20);
asserts.isequal(flagzr, 0);
asserts.isequal(relreszr, 0);
asserts.isequal(iterzr, [0 0]);
asserts.isequal(xzr, zeros(3, 1));
asserts.isequal(resveczr, 0);
%=============================================================================
Z = sparse(zeros(2, 2));
[~, flagz, ~, iterz, resvecz] = gmres(Z, [1; 0], [], 1e-12, 5);
asserts.isequal(flagz, 4);
asserts.isequal(iterz, [1 1]);
asserts.isequal(resvecz, [1; 1]);
%=============================================================================
asserts.checkerror('gmres(A, b, 0)', sprintf(_('%s must be a positive integer scalar.'), 'Restart'));
asserts.checkerror('gmres(A, b, [], -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Tolerance'));
asserts.checkerror('gmres(A, b, [], [], -1)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('gmres(A, b, [], [], Inf)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('gmres(A, b, [], [], [], [], [], ones(2, 1))', _('Initial guess dimensions must agree.'));
asserts.checkerror('gmres(A, b, [], [], [], [1; 2])', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('gmres(A, b, [], [], [], @(x) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('gmres(A, b, [], [], [], speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('gmres(A, b, [], [], [], speye(3), speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('gmres(A, b, [], [], [], [1; 0; 1])', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('gmres(sparse(1), 1, [], [], [], sparse(0))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('gmres(A, b, [], [], [], sparse(diag([1; 0; 1])))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('gmres(A, b, [], [], [], [1 2 0; 2 4 0; 0 0 1])', sprintf(_('%s failed.'), 'Preconditioner factorization'));
asserts.checkerror('gmres(sparse([1 2]), [1; 2])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('gmres(A, [1; 2])', sprintf(_('%s dimensions must agree.'), 'Matrix and vector'));
%=============================================================================
