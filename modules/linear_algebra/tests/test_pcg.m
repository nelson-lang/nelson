%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--REUSE PROCESS-->
asserts.isequal(nargin('pcg'), 7);
asserts.isequal(nargout('pcg'), 5);
%=============================================================================
A = sparse([4 -1 0; -1 4 -1; 0 -1 3]);
b = [15; 10; 10];
[x, flag, relres, iter, resvec] = pcg(A, b, 1e-12, 20);
REF_RESVEC = [20.615528128088304; 6.4412876003492672; 1.4269792670368815; 0];
asserts.isequal(flag, 0);
asserts.istrue(relres < 1e-12);
asserts.isequal(iter, 3);
asserts.isequal(size(resvec, 2), 1);
asserts.isapprox(resvec, REF_RESVEC, 1e-12);
asserts.isapprox(x, [5; 5; 5], 1e-12);
asserts.isapprox(A * x, b, 1e-10);
%=============================================================================
[xd, flagd, relresd, iterd, resvecd] = pcg(A, b);
asserts.isequal(flagd, 0);
asserts.istrue(relresd < 1e-12);
asserts.isequal(iterd, 3);
asserts.isapprox(resvecd(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecd(end) < 1e-12);
asserts.isapprox(xd, [5; 5; 5], 1e-12);
asserts.isapprox(A * xd, b, 1e-10);
%=============================================================================
As = sparse(single([4 -1 0; -1 4 -1; 0 -1 3]));
bs = single([15; 10; 10]);
[xs, flags, relress, iters, resvecs] = pcg(As, bs, 1e-6, 20);
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.istrue(relress < 1e-6);
asserts.isequal(iters, 3);
asserts.isapprox(resvecs, single(REF_RESVEC), 1e-5);
asserts.isapprox(xs, single([5; 5; 5]), 1e-5);
asserts.isapprox(As * xs, bs, 1e-4);
%=============================================================================
Dsingle = single(diag(full(As)));
[xjs, flagjs, relresjs, iterjs, resvecjs] = pcg(As, bs, 1e-6, 20, Dsingle);
asserts.isequal(class(xjs), 'single');
asserts.isequal(flagjs, 0);
asserts.istrue(relresjs < 1e-6);
asserts.isequal(iterjs, 3);
asserts.isapprox(resvecjs, single([20.615528128088304; 6.6093686275597285; 0.56942472027150526; 0]), 1e-5);
asserts.isapprox(xjs, single([5; 5; 5]), 1e-5);
asserts.isapprox(As * xjs, bs, 1e-4);
%=============================================================================
[xfn, flagfn, relresfn, iterfn, resvecfn] = pcg(A, b, 1e-12, 20, @(x) x);
asserts.isequal(flagfn, 0);
asserts.istrue(relresfn < 1e-12);
asserts.isequal(iterfn, 3);
asserts.isapprox(resvecfn(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfn(end) < 1e-12);
asserts.isapprox(xfn, [5; 5; 5], 1e-12);
asserts.isapprox(A * xfn, b, 1e-10);
%=============================================================================
[xfn2, flagfn2, relresfn2, iterfn2, resvecfn2] = pcg(A, b, 1e-12, 20, [], @(x) x);
asserts.isequal(flagfn2, 0);
asserts.istrue(relresfn2 < 1e-12);
asserts.isequal(iterfn2, 3);
asserts.isapprox(resvecfn2(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfn2(end) < 1e-12);
asserts.isapprox(xfn2, [5; 5; 5], 1e-12);
asserts.isapprox(A * xfn2, b, 1e-10);
%=============================================================================
[xfns, flagfns, relresfns, iterfns] = pcg(A, b, 1e-12, 20, @(x) sparse(x));
asserts.isequal(flagfns, 0);
asserts.istrue(relresfns < 1e-12);
asserts.isequal(iterfns, 3);
asserts.isapprox(xfns, [5; 5; 5], 1e-12);
asserts.isapprox(A * xfns, b, 1e-10);
%=============================================================================
L = ichol(A);
[xp, flagp, relresp, iterp, resvecp] = pcg(A, b, 1e-12, 20, L, L');
asserts.isequal(flagp, 0);
asserts.istrue(relresp < 1e-12);
asserts.isequal(iterp, 1);
asserts.isapprox(resvecp(1), REF_RESVEC(1), 1e-12);
asserts.istrue(resvecp(end) < 1e-12);
asserts.isapprox(xp, [5; 5; 5], 1e-12);
asserts.isapprox(A * xp, b, 1e-10);
%=============================================================================
D = diag(full(A));
[xj, flagj, relresj, iterj, resvecj] = pcg(A, b, 1e-12, 20, D);
REF_JACOBI_RESVEC = [20.615528128088304; 6.6093686275597285; 0.56942472027150526; 3.5527136788005009e-15];
asserts.isequal(flagj, 0);
asserts.istrue(relresj < 1e-12);
asserts.isequal(iterj, 3);
asserts.isapprox(resvecj(1:3), REF_JACOBI_RESVEC(1:3), 1e-12);
asserts.istrue(resvecj(end) < 1e-12);
asserts.isapprox(xj, [5; 5; 5], 1e-12);
asserts.isapprox(A * xj, b, 1e-10);
Dmatrix = sparse(diag(D));
[xjm, flagjm, relresjm, iterjm, resvecjm] = pcg(A, b, 1e-12, 20, Dmatrix);
asserts.isequal(flagjm, 0);
asserts.istrue(relresjm < 1e-12);
asserts.isequal(iterjm, 3);
asserts.isapprox(resvecjm(1:3), REF_JACOBI_RESVEC(1:3), 1e-12);
asserts.istrue(resvecjm(end) < 1e-12);
asserts.isapprox(xjm, [5; 5; 5], 1e-12);
asserts.isapprox(A * xjm, b, 1e-10);
%=============================================================================
H = sparse([4 1 + 1i; 1 - 1i 3]);
ch = [1 + 2i; 3 - 1i];
[xh, flagh, relresh, iterh, resvech] = pcg(H, ch, 1e-12, 20);
asserts.isequal(flagh, 0);
asserts.istrue(relresh < 1e-12);
asserts.isequal(iterh, 2);
asserts.isapprox(resvech(1:2), [3.872983346207417; 1.056268185329295], 1e-12);
asserts.istrue(resvech(end) < 1e-12);
asserts.isapprox(xh, [-0.1 + 0.4i; 0.9 - 0.5i], 1e-12);
asserts.istrue(norm(H * xh - ch) < 1e-10);
%=============================================================================
Hs = sparse(single([4 1 + 1i; 1 - 1i 3]));
chs = single([1 + 2i; 3 - 1i]);
[xhs, flaghs, relreshs, iterhs, resvechs] = pcg(Hs, chs, 1e-6, 20);
asserts.isequal(class(xhs), 'single');
asserts.isequal(flaghs, 0);
asserts.istrue(relreshs < 1e-6);
asserts.isequal(iterhs, 2);
asserts.isapprox(resvechs(1:2), single([3.872983346207417; 1.056268185329295]), 1e-5);
asserts.istrue(resvechs(end) < 1e-6);
asserts.isapprox(xhs, single([-0.1 + 0.4i; 0.9 - 0.5i]), 1e-5);
asserts.istrue(norm(Hs * xhs - chs) < 1e-4);
%=============================================================================
A2 = sparse([4 1; 1 3]);
bc = [1 + 1i; 2 - 1i];
[xmix, flagmix, relresmix, itermix, resvecmix] = pcg(A2, bc, 1e-12, 20);
asserts.isequal(flagmix, 0);
asserts.istrue(relresmix < 1e-12);
asserts.isequal(itermix, 2);
asserts.isapprox(resvecmix(1:2), [2.645751311064591; 0.826559132790873], 1e-12);
asserts.istrue(resvecmix(end) < 1e-12);
asserts.isapprox(xmix, [0.090909090909091 + 0.363636363636364i; 0.636363636363636 - 0.454545454545455i], 1e-12);
asserts.istrue(norm(A2 * xmix - bc) < 1e-10);
%=============================================================================
D2 = diag([4; 3]);
[xhp, flaghp, relreshp, iterhp, resvechp] = pcg(H, ch, 1e-12, 20, D2);
asserts.isequal(flaghp, 0);
asserts.istrue(relreshp < 1e-12);
asserts.isequal(iterhp, 2);
asserts.isapprox(resvechp(1:2), [3.872983346207417; 0.917064078611027], 1e-12);
asserts.istrue(resvechp(end) < 1e-12);
asserts.isapprox(xhp, [-0.1 + 0.4i; 0.9 - 0.5i], 1e-12);
asserts.istrue(norm(H * xhp - ch) < 1e-10);
%=============================================================================
x0 = ones(3, 1);
[x0r, flag0, relres0, iter0, resvec0] = pcg(A, b, 1e-12, 20, [], [], x0);
asserts.isequal(flag0, 0);
asserts.istrue(relres0 < 1e-12);
asserts.isequal(iter0, 3);
asserts.isapprox(resvec0(1:3), [16.492422502470642; 5.153030080279413; 1.141583413629505], 1e-12);
asserts.istrue(resvec0(end) < 1e-12);
asserts.isapprox(x0r, [5; 5; 5], 1e-12);
asserts.isapprox(A * x0r, b, 1e-10);
%=============================================================================
[x0s, flag0s, relres0s, iter0s] = pcg(A, b, 1e-12, 20, [], [], sparse(x0));
asserts.isequal(flag0s, 0);
asserts.istrue(relres0s < 1e-12);
asserts.isequal(iter0s, 3);
asserts.isapprox(x0s, [5; 5; 5], 1e-12);
asserts.isapprox(A * x0s, b, 1e-10);
x0c = 1i * ones(3, 1);
[x0cResult, flag0c, relres0c] = pcg(A, b, 1e-12, 20, [], [], x0c);
asserts.isequal(flag0c, 0);
asserts.istrue(relres0c < 1e-12);
asserts.istrue(norm(A * x0cResult - b) < 1e-10);
%=============================================================================
[xm, flagm, relresm, iterm, resvecm] = pcg(A, b, 1e-16, 1);
asserts.isequal(flagm, 1);
asserts.isapprox(relresm, 0.312448342837898, 1e-12);
asserts.isequal(iterm, 1);
asserts.isapprox(xm, [5.795454545454545; 3.863636363636363; 3.863636363636363], 1e-12);
asserts.isapprox(resvecm, [20.615528128088304; 6.4412876003492672], 1e-12);
%=============================================================================
[xzeroit, flagzeroit, relreszeroit, iterzeroit, resveczeroit] = pcg(A, b, 1e-12, 0);
asserts.isequal(flagzeroit, 1);
asserts.isequal(relreszeroit, 1);
asserts.isequal(iterzeroit, 0);
asserts.isequal(xzeroit, zeros(3, 1));
asserts.isapprox(resveczeroit, 20.615528128088304, 1e-12);
%=============================================================================
[xzr, flagzr, relreszr, iterzr, resveczr] = pcg(A, zeros(3, 1), 1e-12, 20);
asserts.isequal(flagzr, 0);
asserts.isequal(relreszr, 0);
asserts.isequal(iterzr, 0);
asserts.isequal(xzr, zeros(3, 1));
asserts.isequal(resveczr, 0);
%=============================================================================
Z = sparse(zeros(2, 2));
[~, flagz, ~, iterz, resvecz] = pcg(Z, [1; 0], 1e-12, 5);
asserts.isequal(flagz, 4);
asserts.isequal(iterz, 0);
asserts.isequal(resvecz, 1);
%=============================================================================
asserts.checkerror('pcg(A, b, -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Tolerance'));
asserts.checkerror('pcg(A, b, [], -1)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('pcg(A, b, [], Inf)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('pcg(A, b, [], [], [], [], ones(2, 1))', _('Initial guess dimensions must agree.'));
asserts.checkerror('pcg(A, b, [], [], [1; 2])', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('pcg(A, b, [], [], @(x) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('pcg(A, b, [], [], speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('pcg(A, b, [], [], speye(3), speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('pcg(A, b, [], [], [1; 0; 1])', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('pcg(sparse(1), 1, [], [], sparse(0))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('pcg(A, b, [], [], sparse(diag([1; 0; 1])))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('pcg(A, b, [], [], [1 2 0; 2 4 0; 0 0 1])', sprintf(_('%s failed.'), 'Preconditioner factorization'));
asserts.checkerror('pcg(sparse([1 2]), [1; 2])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('pcg(A, [1; 2])', sprintf(_('%s dimensions must agree.'), 'Matrix and vector'));
%=============================================================================
