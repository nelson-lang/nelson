%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
%=============================================================================
C = sparse([3 + 1i 1; 0 2 - 1i]);
d = [4 + 2i; 3 - 1i];
[y, cflag, crelres, citer, cresvec] = bicg(C, d, 1e-12, 30);
asserts.isequal(cflag, 0);
asserts.istrue(crelres < 1e-12);
asserts.isequal(citer, 2);
asserts.isapprox(cresvec(1:2), [5.4772255750516612; 1.8257418583505538], 1e-12);
asserts.istrue(cresvec(end) < 1e-12);
asserts.isapprox(y, [0.96 + 0.28i; 1.4 + 0.2i], 1e-12);
asserts.isapprox(C * y, d, 1e-10);
%=============================================================================
Cs = sparse(single([3 + 1i 1; 0 2 - 1i]));
ds = single([4 + 2i; 3 - 1i]);
[ys, cflags, crelress, citers, cresvecs] = bicg(Cs, ds, 1e-6, 30);
asserts.isequal(class(ys), 'single');
asserts.isequal(cflags, 0);
asserts.istrue(crelress < 1e-6);
asserts.isequal(citers, 2);
asserts.isapprox(cresvecs, single([5.4772255750516612; 1.8257418583505538; 0]), 1e-5);
asserts.isapprox(ys, single([0.96 + 0.28i; 1.4 + 0.2i]), 1e-5);
asserts.isapprox(Cs * ys, ds, 1e-4);
%=============================================================================
bc = [1 + 1i; 2; 3 - 1i];
[ymix1, mixflag1, mixrelres1, mixiter1, mixresvec1] = bicg(A, bc, 1e-12, 20);
asserts.isequal(mixflag1, 0);
asserts.istrue(mixrelres1 < 1e-12);
asserts.isequal(mixiter1, 3);
asserts.isapprox(mixresvec1(1:3), [4; 1.547352347531397; 0.542591575695258], 1e-12);
asserts.istrue(mixresvec1(end) < 1e-12);
asserts.isapprox(ymix1, [0.25 + 0.25i; 0; 1.5 - 0.5i], 1e-12);
asserts.istrue(norm(A * ymix1 - bc) < 1e-10);
%=============================================================================
br = [4; 3];
[ymix2, mixflag2, mixrelres2, mixiter2, mixresvec2] = bicg(C, br, 1e-12, 20);
asserts.isequal(mixflag2, 0);
asserts.istrue(mixrelres2 < 1e-12);
asserts.isequal(mixiter2, 2);
asserts.isapprox(mixresvec2(1:2), [5; 2.036076064257512], 1e-12);
asserts.istrue(mixresvec2(end) < 1e-12);
asserts.isapprox(ymix2, [0.78 - 0.46i; 1.2 + 0.6i], 1e-12);
asserts.istrue(norm(C * ymix2 - br) < 1e-10);
%=============================================================================
x0 = ones(3, 1);
[x0r, flag0, relres0, iter0, resvec0] = bicg(A, b, 1e-12, 20, [], [], x0);
asserts.isequal(flag0, 0);
asserts.istrue(relres0 < 1e-12);
asserts.isequal(iter0, 3);
asserts.isapprox(resvec0(1:3), [5.6568542494923806; 0.8; 1.0886621079036347], 1e-12);
asserts.istrue(resvec0(end) < 1e-12);
asserts.isapprox(A * x0r, b, 1e-10);
%=============================================================================
[x0s, flag0s, relres0s, iter0s] = bicg(A, b, 1e-12, 20, [], [], sparse(x0));
asserts.isequal(flag0s, 0);
asserts.istrue(relres0s < 1e-12);
asserts.isequal(iter0s, 3);
asserts.isapprox(A * x0s, b, 1e-10);
%=============================================================================
[xm, flagm, relresm, iterm, resvecm] = bicg(A, b, 1e-16, 1);
asserts.isequal(flagm, 1);
asserts.isapprox(relresm, 0.37977726265637515, 1e-12);
asserts.isequal(iterm, 1);
asserts.isapprox(resvecm, [3.7416573867739413; 1.4209964001470149], 1e-12);
%=============================================================================
[xzeroit, flagzeroit, relreszeroit, iterzeroit, resveczeroit] = bicg(A, b, 1e-12, 0);
asserts.isequal(flagzeroit, 1);
asserts.isequal(relreszeroit, 1);
asserts.isequal(iterzeroit, 0);
asserts.isequal(xzeroit, zeros(3, 1));
asserts.isapprox(resveczeroit, 3.7416573867739413, 1e-12);
%=============================================================================
[xzr, flagzr, relreszr, iterzr, resveczr] = bicg(A, zeros(3, 1), 1e-12, 20);
asserts.isequal(flagzr, 0);
asserts.isequal(relreszr, 0);
asserts.isequal(iterzr, 0);
asserts.isequal(xzr, zeros(3, 1));
asserts.isequal(resveczr, 0);
%=============================================================================
Z = sparse(zeros(2, 2));
[xz, flagz, relresz, iterz, resvecz] = bicg(Z, [1; 0], 1e-12, 5);
asserts.isequal(flagz, 4);
asserts.isequal(relresz, 1);
asserts.isequal(iterz, 0);
asserts.isequal(resvecz, 1);
asserts.isequal(xz, [0; 0]);
%=============================================================================
asserts.checkerror('bicg(A, b, -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Tolerance'));
asserts.checkerror('bicg(A, b, [], -1)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('bicg(A, b, [], Inf)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('bicg(A, b, [], [], [], [], ones(2, 1))', _('Initial guess dimensions must agree.'));
asserts.checkerror('bicg(A, b, [], [], [1; 2])', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicg(A, b, [], [], speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicg(A, b, [], [], speye(3), speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicg(A, b, [], [], [1; 0; 1])', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('bicg(sparse(1), 1, [], [], sparse(0))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('bicg(A, b, [], [], [1 2 0; 2 4 0; 0 0 1])', sprintf(_('%s failed.'), 'Preconditioner factorization'));
asserts.checkerror('bicg(A, b, [], [], @(x) x)', _('Too many input arguments.'));
asserts.checkerror('bicg(A, b, [], [], @(x, t) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicg(A, b, [], [], [], @(x, t) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicg(sparse([1 2]), [1; 2])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('bicg(A, [1; 2])', sprintf(_('%s dimensions must agree.'), 'Matrix and vector'));
%=============================================================================
