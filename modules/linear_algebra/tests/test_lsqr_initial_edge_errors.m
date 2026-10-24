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
R = sparse([1 0; 0 1; 1 1; 2 -1]);
c = [1; 2; 4; 1];
x0 = ones(3, 1);
[x0r, flag0, relres0, iter0, resvec0, lsvec0] = lsqr(A, b, 1e-12, 20, [], [], x0);
asserts.isequal(flag0, 0);
asserts.istrue(relres0 < 1e-12);
asserts.isequal(iter0, 3);
asserts.isapprox(resvec0(1:3), [5.6568542494923806; 0.947040555176382; 0.668015053765149], 1e-12);
asserts.istrue(resvec0(end) < 1e-12);
asserts.isapprox(lsvec0(1:3), [5.57701660270536; 0.255915042511607; 0.158250844770988], 1e-12);
asserts.istrue(lsvec0(end) < 1e-12);
asserts.isapprox(x0r, [0.25; 0; 1.5], 1e-10);
asserts.isapprox(A * x0r, b, 1e-10);
%=============================================================================
x0ls = sparse(ones(2, 1));
[x0rs, flag0s, relres0s, iter0s] = lsqr(R, c, 1e-12, 20, [], [], x0ls);
asserts.isequal(flag0s, 0);
asserts.isapprox(relres0s, 0.13680854337396819, 1e-12);
asserts.isequal(iter0s, 2);
asserts.isapprox(x0rs, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * x0rs - c), 0.64168894791974773, 1e-12);
x0c = 1i * ones(2, 1);
[x0cResult, flag0c, relres0c] = lsqr(R, c, 1e-12, 20, [], [], x0c);
asserts.isequal(flag0c, 0);
asserts.isapprox(relres0c, 0.13680854337396819, 1e-12);
asserts.isapprox(norm(R * x0cResult - c), 0.64168894791974773, 1e-10);
%=============================================================================
[xm, flagm, relresm, iterm, resvecm, lsvecm] = lsqr(A, b, 1e-16, 1);
asserts.isequal(flagm, 1);
asserts.isapprox(relresm, 0.553834002972558, 1e-12);
asserts.isequal(iterm, 1);
asserts.isapprox(xm, [0.340552651232263; 0.425690814040329; 0.340552651232263], 1e-12);
asserts.isapprox(resvecm, [3.7416573867739413; 2.0722570882688536], 1e-12);
asserts.isapprox(lsvecm, 3.1154053604819216, 1e-12);
%=============================================================================
[xzeroit, flagzeroit, relreszeroit, iterzeroit, resveczeroit, lsveczeroit] = lsqr(A, b, 1e-12, 0);
asserts.isequal(flagzeroit, 1);
asserts.isequal(relreszeroit, 1);
asserts.isequal(iterzeroit, 0);
asserts.isequal(xzeroit, zeros(3, 1));
asserts.isapprox(resveczeroit, 3.7416573867739413, 1e-12);
asserts.isequal(size(lsveczeroit), [0 1]);
%=============================================================================
[xzr, flagzr, relreszr, iterzr, resveczr, lsveczr] = lsqr(A, zeros(3, 1), 1e-12, 20);
asserts.isequal(flagzr, 0);
asserts.istrue(isnan(relreszr));
asserts.isequal(iterzr, 0);
asserts.isequal(xzr, zeros(3, 1));
asserts.isequal(resveczr, 0);
asserts.isequal(size(lsveczr), [0 1]);
%=============================================================================
Z = sparse(zeros(3, 2));
[xz, flagz, relresz, iterz, resvecz, lsvecz] = lsqr(Z, [1; 0; 0], 1e-12, 5);
asserts.isequal(flagz, 4);
asserts.isequal(relresz, 1);
asserts.isequal(iterz, 0);
asserts.isequal(resvecz, 1);
asserts.isequal(lsvecz, 0);
asserts.isequal(xz, [0; 0]);
%=============================================================================
asserts.checkerror('lsqr(A, b, -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Tolerance'));
asserts.checkerror('lsqr(A, b, [], -1)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('lsqr(A, b, [], Inf)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('lsqr(A, [1; 2])', _('Matrix and vector dimensions must agree.'));
asserts.checkerror('lsqr(A, b, [], [], [], [], ones(2, 1))', _('Initial guess dimensions must agree.'));
asserts.checkerror('lsqr(A, b, [], [], [1; 2])', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('lsqr(A, b, [], [], @(x) x)', _('Too many input arguments.'));
asserts.checkerror('lsqr(A, b, [], [], @(x, t) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('lsqr(A, b, [], [], [], @(x) x)', _('Too many input arguments.'));
asserts.checkerror('lsqr(A, b, [], [], [], @(x, t) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('lsqr(A, b, [], [], speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('lsqr(A, b, [], [], speye(3), speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('lsqr(A, b, [], [], [1; 0; 1])', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('lsqr(sparse(1), 1, [], [], sparse(0))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('lsqr(A, b, [], [], sparse(diag([1; 0; 1])))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('lsqr(R, c, [], [], [1 2; 2 4])', sprintf(_('%s failed.'), 'Preconditioner factorization'));
%=============================================================================
