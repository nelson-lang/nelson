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
[y, cflag, crelres, citer, cresvec] = bicgstab(C, d, 1e-12, 30);
asserts.isequal(cflag, 0);
asserts.istrue(crelres < 1e-12);
asserts.isequal(citer, 1.5);
asserts.isapprox(cresvec, [5.4772255750516612; 1.8257418583505538; 1.0540925533894598; 0], 1e-12);
asserts.isapprox(y, [0.96 + 0.28i; 1.4 + 0.2i], 1e-12);
asserts.isapprox(C * y, d, 1e-10);
%=============================================================================
Cs = sparse(single([3 + 1i 1; 0 2 - 1i]));
ds = single([4 + 2i; 3 - 1i]);
[ys, cflags, crelress, citers, cresvecs] = bicgstab(Cs, ds, 1e-6, 30);
asserts.isequal(class(ys), 'single');
asserts.isequal(cflags, 0);
asserts.istrue(crelress < 1e-6);
asserts.isequal(citers, 1.5);
asserts.isapprox(cresvecs, single([5.4772255750516612; 1.8257418583505538; 1.0540925533894598; 0]), 1e-5);
asserts.isapprox(ys, single([0.96 + 0.28i; 1.4 + 0.2i]), 1e-5);
asserts.isapprox(Cs * ys, ds, 1e-4);
%=============================================================================
bc = [1 + 1i; 2; 3 - 1i];
[ymix1, mixflag1, mixrelres1, mixiter1, mixresvec1] = bicgstab(A, bc, 1e-12, 20);
asserts.isequal(mixflag1, 0);
asserts.istrue(mixrelres1 < 1e-12);
asserts.isequal(mixiter1, 2.5);
asserts.isapprox(mixresvec1(1:5), [4; 1.547352347531397; 0.74452950360748; 0.36159182740124; 0.158425881654397], 1e-12);
asserts.istrue(mixresvec1(end) < 1e-12);
asserts.isapprox(ymix1, [0.25 + 0.25i; 0; 1.5 - 0.5i], 1e-12);
asserts.istrue(norm(A * ymix1 - bc) < 1e-10);
%=============================================================================
br = [4; 3];
[ymix2, mixflag2, mixrelres2, mixiter2, mixresvec2] = bicgstab(C, br, 1e-12, 20);
asserts.isequal(mixflag2, 0);
asserts.istrue(mixrelres2 < 1e-12);
asserts.isequal(mixiter2, 1.5);
asserts.isapprox(mixresvec2(1:3), [5; 2.036076064257512; 0.927966763952221], 1e-12);
asserts.istrue(mixresvec2(end) < 1e-12);
asserts.isapprox(ymix2, [0.78 - 0.46i; 1.2 + 0.6i], 1e-12);
asserts.istrue(norm(C * ymix2 - br) < 1e-10);
%=============================================================================
x0 = ones(3, 1);
[x0r, flag0, relres0, iter0, resvec0] = bicgstab(A, b, 1e-12, 30, [], [], x0);
REF_X0_RESVEC = [5.6568542494923806; 0.8; 0.35777087639996635; 0.12570787221094171; 0.06564879518897734; 3.1401849173675503e-16];
asserts.isequal(flag0, 0);
asserts.istrue(relres0 < 1e-12);
asserts.isequal(iter0, 2.5);
asserts.isapprox(resvec0(1:5), REF_X0_RESVEC(1:5), 1e-12);
asserts.istrue(resvec0(end) < 1e-12);
asserts.isapprox(x0r, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * x0r, b, 1e-10);
%=============================================================================
[x0s, flag0s, relres0s, iter0s] = bicgstab(A, b, 1e-12, 30, [], [], sparse(x0));
asserts.isequal(flag0s, 0);
asserts.istrue(relres0s < 1e-12);
asserts.isequal(iter0s, 2.5);
asserts.isapprox(x0s, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * x0s, b, 1e-10);
%=============================================================================
[ym, flagm, relresm, iterm, resvecm] = bicgstab(A, b, 1e-16, 1);
REF_LIMITED_X = [0.111803106289467; 0.292480815115754; 1.0241553442366];
asserts.isequal(flagm, 1);
asserts.isapprox(relresm, 0.19235226112848, 1e-12);
asserts.isequal(iterm, 1);
asserts.isapprox(ym, REF_LIMITED_X, 1e-12);
asserts.isapprox(resvecm, [3.7416573867739413; 1.4209964001470126; 0.71971625871404632], 1e-12);
%=============================================================================
[xzeroit, flagzeroit, relreszeroit, iterzeroit, resveczeroit] = bicgstab(A, b, 1e-12, 0);
asserts.isequal(flagzeroit, 1);
asserts.isequal(relreszeroit, 1);
asserts.isequal(iterzeroit, 0);
asserts.isequal(xzeroit, zeros(3, 1));
asserts.isapprox(resveczeroit, 3.7416573867739413, 1e-12);
%=============================================================================
[xzr, flagzr, relreszr, iterzr, resveczr] = bicgstab(A, zeros(3, 1), 1e-12, 20);
asserts.isequal(flagzr, 0);
asserts.isequal(relreszr, 0);
asserts.isequal(iterzr, 0);
asserts.isequal(xzr, zeros(3, 1));
asserts.isequal(resveczr, 0);
%=============================================================================
Z = sparse(zeros(2, 2));
[~, flagz, ~, iterz, resvecz] = bicgstab(Z, [1; 0], 1e-12, 5);
asserts.isequal(flagz, 4);
asserts.isequal(iterz, 0);
asserts.isequal(resvecz, 1);
%=============================================================================
asserts.checkerror('bicgstab(A, b, -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Tolerance'));
asserts.checkerror('bicgstab(A, b, [], -1)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('bicgstab(A, b, [], Inf)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('bicgstab(A, b, [], [], [], [], ones(2, 1))', _('Initial guess dimensions must agree.'));
asserts.checkerror('bicgstab(A, b, [], [], [1; 2])', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicgstab(A, b, [], [], speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicgstab(A, b, [], [], speye(3), speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicgstab(A, b, [], [], [1; 0; 1])', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('bicgstab(A, b, [], [], [1 2 0; 2 4 0; 0 0 1])', sprintf(_('%s failed.'), 'Preconditioner factorization'));
asserts.checkerror('bicgstab(A, b, [], [], @(x) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicgstab(A, b, [], [], [], @(x) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('bicgstab(sparse([1 2]), [1; 2])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('bicgstab(A, [1; 2])', sprintf(_('%s dimensions must agree.'), 'Matrix and vector'));
%=============================================================================
