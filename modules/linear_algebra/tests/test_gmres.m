%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--REUSE PROCESS-->
asserts.isequal(nargin('gmres'), 8);
asserts.isequal(nargout('gmres'), 5);
%=============================================================================
A = sparse([4 1 0; 2 3 1; 0 1 2]);
b = [1; 2; 3];
[x, flag, relres, iter, resvec] = gmres(A, b, [], 1e-12, 20);
REF_RESVEC = [3.7416573867739413; 1.3284223283101431; 0.49501013102639851; 0];
asserts.isequal(flag, 0);
asserts.istrue(relres < 1e-12);
asserts.isequal(iter, [1 3]);
asserts.isequal(size(resvec, 2), 1);
asserts.isapprox(resvec, REF_RESVEC, 1e-12);
asserts.isapprox(x, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * x, b, 1e-10);
%=============================================================================
[xd, flagd, relresd, iterd, resvecd] = gmres(A, b);
REF_DEFAULT_RESVEC = [3.7416573867739413; 1.3284223283101431; 0.49501013102639851; 0];
asserts.isequal(flagd, 0);
asserts.istrue(relresd < 1e-12);
asserts.isequal(iterd, [1 3]);
asserts.isapprox(resvecd(1:3), REF_DEFAULT_RESVEC(1:3), 1e-12);
asserts.istrue(resvecd(end) < 1e-12);
asserts.isapprox(xd, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xd, b, 1e-10);
%=============================================================================
As = sparse(single([4 1 0; 2 3 1; 0 1 2]));
bs = single([1; 2; 3]);
[xs, flags, relress, iters, resvecs] = gmres(As, bs, [], 1e-6, 20);
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.istrue(relress < 1e-6);
asserts.isequal(iters, [1 3]);
asserts.isapprox(resvecs, single(REF_DEFAULT_RESVEC), 1e-5);
asserts.isapprox(xs, single([0.25; 0; 1.5]), 1e-5);
asserts.isapprox(As * xs, bs, 1e-5);
%=============================================================================
Ds = single(diag(full(As)));
[xjs, flagjs, relresjs, iterjs] = gmres(As, bs, [], 1e-6, 20, Ds);
asserts.isequal(class(xjs), 'single');
asserts.isequal(flagjs, 0);
asserts.istrue(relresjs < 1e-6);
asserts.isequal(iterjs, [1 3]);
asserts.isapprox(As * xjs, bs, 1e-5);
%=============================================================================
[xfn, flagfn, relresfn, iterfn, resvecfn] = gmres(A, b, [], 1e-12, 20, @(x) x);
asserts.isequal(flagfn, 0);
asserts.istrue(relresfn < 1e-12);
asserts.isequal(iterfn, [1 3]);
asserts.isapprox(resvecfn(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfn(end) < 1e-12);
asserts.isapprox(xfn, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xfn, b, 1e-10);
%=============================================================================
[xfn2, flagfn2, relresfn2, iterfn2, resvecfn2] = gmres(A, b, [], 1e-12, 20, [], @(x) x);
asserts.isequal(flagfn2, 0);
asserts.istrue(relresfn2 < 1e-12);
asserts.isequal(iterfn2, [1 3]);
asserts.isapprox(resvecfn2(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfn2(end) < 1e-12);
asserts.isapprox(xfn2, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xfn2, b, 1e-10);
%=============================================================================
[xfns, flagfns, relresfns, iterfns] = gmres(A, b, [], 1e-12, 20, @(x) sparse(x));
asserts.isequal(flagfns, 0);
asserts.istrue(relresfns < 1e-12);
asserts.isequal(iterfns, [1 3]);
asserts.isapprox(xfns, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xfns, b, 1e-10);
%=============================================================================
[L, U] = ilu(A);
[xp, flagp, relresp, iterp, resvecp] = gmres(A, b, [], 1e-12, 20, L, U);
asserts.isequal(flagp, 0);
asserts.istrue(relresp < 1e-12);
asserts.isequal(iterp, [1 1]);
asserts.isequal(length(resvecp), 2);
asserts.isapprox(resvecp(1), 1.5206906325745546, 1e-12);
asserts.isapprox(xp, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xp, b, 1e-10);
%=============================================================================
D = diag(full(A));
[xj, flagj, relresj, iterj, resvecj] = gmres(A, b, [], 1e-12, 20, D);
REF_JACOBI_RESVEC = [1.660404903764273; 0.34594864471262787; 0.09081532183729997; 0];
asserts.isequal(flagj, 0);
asserts.istrue(relresj < 1e-12);
asserts.isequal(iterj, [1 3]);
asserts.isapprox(resvecj(1:3), REF_JACOBI_RESVEC(1:3), 1e-12);
asserts.istrue(resvecj(end) < 1e-12);
asserts.isapprox(xj(1), 0.25, 1e-12);
asserts.istrue(abs(xj(2)) < 1e-12);
asserts.isapprox(xj(3), 1.5, 1e-12);
asserts.isapprox(A * xj, b, 1e-10);
Dmatrix = sparse(diag(D));
[xjm, flagjm, relresjm, iterjm, resvecjm] = gmres(A, b, [], 1e-12, 20, Dmatrix);
asserts.isequal(flagjm, 0);
asserts.istrue(relresjm < 1e-12);
asserts.isequal(iterjm, [1 3]);
asserts.isapprox(resvecjm(1:3), REF_JACOBI_RESVEC(1:3), 1e-12);
asserts.istrue(resvecjm(end) < 1e-12);
asserts.isapprox(xjm(1), 0.25, 1e-12);
asserts.istrue(abs(xjm(2)) < 1e-12);
asserts.isapprox(xjm(3), 1.5, 1e-12);
asserts.isapprox(A * xjm, b, 1e-10);
Dc = D + [1i; 0; 0];
[xjc, flagjc, relresjc] = gmres(A, b, [], 1e-12, 20, Dc);
asserts.isequal(flagjc, 0);
asserts.istrue(relresjc < 1e-12);
asserts.istrue(norm(A * xjc - b) < 1e-10);
%=============================================================================
Mfull = full([4 1 0; 2 3 1; 1 1 2]);
[xf, flagf, relresf, iterf, resvecf] = gmres(A, b, [], 1e-12, 20, Mfull);
REF_FULL_PRECOND_RESVEC = [1.37450840535858; 0.068816559401422];
asserts.isequal(flagf, 0);
asserts.istrue(relresf < 1e-12);
asserts.isequal(iterf, [1 2]);
asserts.isapprox(resvecf(1:2), REF_FULL_PRECOND_RESVEC, 1e-12);
asserts.istrue(resvecf(end) < 1e-12);
asserts.isapprox(xf, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xf, b, 1e-10);
%=============================================================================
A2 = sparse([4 1; 2 3]);
b2 = [5; 5];
M1d = [2 0; 0 1];
M2d = [2 0.5; 2 3];
[xsplit, flagsplit, relressplit, itersplit, resvecsplit] = gmres(A2, b2, [], 1e-12, 10, M1d, M2d);
asserts.isequal(flagsplit, 0);
asserts.istrue(relressplit < 1e-12);
asserts.isequal(itersplit, [1 1]);
asserts.isapprox(resvecsplit, [sqrt(2); 0], 1e-12);
asserts.isapprox(xsplit, [1; 1], 1e-12);
asserts.isapprox(A2 * xsplit, b2, 1e-10);
%=============================================================================
