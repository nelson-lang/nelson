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
asserts.isequal(nargin('minres'), 7);
asserts.isequal(nargout('minres'), 5);
%=============================================================================
A = sparse([4 -1 0; -1 4 -1; 0 -1 3]);
b = [15; 10; 10];
[x, flag, relres, iter, resvec] = minres(A, b, 1e-12, 20);
REF_RESVEC = [20.615528128088304; 6.14817045957576; 1.3900301824226604; 0];
asserts.isequal(flag, 0);
asserts.istrue(relres < 1e-12);
asserts.isequal(iter, 3);
asserts.isequal(size(resvec, 2), 1);
asserts.isapprox(resvec, REF_RESVEC, 1e-12);
asserts.isapprox(x, [5; 5; 5], 1e-12);
asserts.isapprox(A * x, b, 1e-10);
Dmatrix = sparse(diag(full(diag(A))));
[xjm, flagjm, relresjm, iterjm, resvecjm] = minres(A, b, 1e-12, 20, Dmatrix);
asserts.isequal(flagjm, 0);
asserts.istrue(relresjm < 1e-12);
asserts.isequal(iterjm, 3);
asserts.isapprox(resvecjm(1), REF_RESVEC(1), 1e-12);
asserts.istrue(resvecjm(end) < 1e-12);
asserts.isapprox(xjm, [5; 5; 5], 1e-12);
asserts.isapprox(A * xjm, b, 1e-10);
%=============================================================================
Dfull = full(Dmatrix);
[xjf, flagjf, relresjf, iterjf, resvecjf] = minres(A, b, 1e-12, 20, Dfull);
asserts.isequal(flagjf, 0);
asserts.istrue(relresjf < 1e-12);
asserts.isequal(iterjf, 3);
asserts.isapprox(resvecjf(1), REF_RESVEC(1), 1e-12);
asserts.istrue(resvecjf(end) < 1e-12);
asserts.isapprox(xjf, [5; 5; 5], 1e-12);
asserts.isapprox(A * xjf, b, 1e-10);
%=============================================================================
M1d = diag([2 2 1]);
M2d = diag([2 2 3]);
[xsplit, flagsplit, relressplit, itersplit, resvecsplit] = minres(A, b, 1e-12, 20, M1d, M2d);
asserts.isequal(flagsplit, 0);
asserts.istrue(relressplit < 1e-12);
asserts.isequal(itersplit, 3);
asserts.isapprox(resvecsplit, [20.615528128088304; 6.3311611243497685; 0.58408774102513239; 0], 1e-12);
asserts.isapprox(xsplit, [5; 5; 5], 1e-12);
asserts.isapprox(A * xsplit, b, 1e-10);
%=============================================================================
[xd, flagd, relresd, iterd, resvecd] = minres(A, b);
asserts.isequal(flagd, 0);
asserts.istrue(relresd < 1e-12);
asserts.isequal(iterd, 3);
asserts.isapprox(resvecd(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecd(end) < 1e-12);
asserts.isapprox(xd, [5; 5; 5], 1e-12);
asserts.isapprox(A * xd, b, 1e-10);
%=============================================================================
As = sparse(single([2 1 0; 1 -1 1; 0 1 2]));
bs = single([1; 2; 3]);
[xs, flags, relress, iters, resvecs] = minres(As, bs, 1e-6, 20);
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.istrue(relress < 1e-6);
asserts.isequal(iters, 3);
asserts.isapprox(resvecs, [3.7416573867739413; 1.3451854182690986; 0.6531972647421809; 0], 1e-5);
asserts.isapprox(xs, single([0.5; 0; 1.5]), 1e-5);
asserts.isapprox(As * xs, bs, 1e-5);
%=============================================================================
Is = sparse(single(eye(3, 3)));
[xps, flagps, relresps, iterps] = minres(As, bs, 1e-6, 20, Is);
asserts.isequal(class(xps), 'single');
asserts.isequal(flagps, 0);
asserts.istrue(relresps < 1e-6);
asserts.isequal(iterps, 3);
asserts.isapprox(As * xps, bs, 1e-5);
%=============================================================================
[xfn, flagfn, relresfn, iterfn, resvecfn] = minres(A, b, 1e-12, 20, @(x) x);
asserts.isequal(flagfn, 0);
asserts.istrue(relresfn < 1e-12);
asserts.isequal(iterfn, 3);
asserts.isapprox(resvecfn(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfn(end) < 1e-12);
asserts.isapprox(xfn, [5; 5; 5], 1e-12);
asserts.isapprox(A * xfn, b, 1e-10);
%=============================================================================
[xfn2, flagfn2, relresfn2, iterfn2, resvecfn2] = minres(A, b, 1e-12, 20, [], @(x) x);
asserts.isequal(flagfn2, 0);
asserts.istrue(relresfn2 < 1e-12);
asserts.isequal(iterfn2, 3);
asserts.isapprox(resvecfn2(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfn2(end) < 1e-12);
asserts.isapprox(xfn2, [5; 5; 5], 1e-12);
asserts.isapprox(A * xfn2, b, 1e-10);
%=============================================================================
[xfns, flagfns, relresfns, iterfns] = minres(A, b, 1e-12, 20, @(x) sparse(x));
asserts.isequal(flagfns, 0);
asserts.istrue(relresfns < 1e-12);
asserts.isequal(iterfns, 3);
asserts.isapprox(xfns, [5; 5; 5], 1e-12);
asserts.isapprox(A * xfns, b, 1e-10);
%=============================================================================
S = sparse([0 1; 1 0]);
c = [1; 2];
[xs, flags, relress, iters, resvecs] = minres(S, c, 1e-12, 20);
asserts.isequal(flags, 0);
asserts.istrue(relress < 1e-12);
asserts.isequal(iters, 2);
asserts.isapprox(resvecs, [2.23606797749979; 1.3416407864998738; 0], 1e-12);
asserts.isapprox(xs, [2; 1], 1e-12);
asserts.isapprox(S * xs, c, 1e-10);
%=============================================================================
[xi, flagi, relresi, iteri, resveci] = minres(S, c, 1e-12, 20, speye(2));
REF_IDENTITY_RESVEC = [2.2360679774997898; 1.3416407864998738; 1.1102230246251565e-16];
asserts.isequal(flagi, 0);
asserts.istrue(relresi < 1e-12);
asserts.isequal(iteri, 2);
asserts.isapprox(resveci(1:2), REF_IDENTITY_RESVEC(1:2), 1e-12);
asserts.istrue(resveci(end) < 1e-12);
asserts.isapprox(xi, [2; 1], 1e-12);
asserts.isapprox(S * xi, c, 1e-10);
%=============================================================================
H = sparse([2 1 + 1i; 1 - 1i -1]);
ch = [1 + 2i; 3 - 1i];
[xh, flagh, relresh, iterh, resvech] = minres(H, ch, 1e-12, 20);
asserts.isequal(flagh, 0);
asserts.istrue(relresh < 1e-12);
asserts.isequal(iterh, 2);
asserts.isapprox(resvech(1:2), [3.872983346207417; 3.4105100714363002], 1e-12);
asserts.istrue(resvech(end) < 1e-12);
asserts.isapprox(xh, [1.25 + 1i; -0.75 + 0.75i], 1e-12);
asserts.istrue(norm(H * xh - ch) < 1e-12);
%=============================================================================
Hs = sparse(single([2 1 + 1i; 1 - 1i -1]));
chs = single([1 + 2i; 3 - 1i]);
[xhs, flaghs, relreshs, iterhs, resvechs] = minres(Hs, chs, 1e-6, 20);
asserts.isequal(class(xhs), 'single');
asserts.isequal(flaghs, 0);
asserts.istrue(relreshs < 1e-6);
asserts.isequal(iterhs, 2);
asserts.isapprox(resvechs, [3.872983346207417; 3.4105100714363002; 0], 1e-5);
asserts.isapprox(xhs, single([1.25 + 1i; -0.75 + 0.75i]), 1e-5);
asserts.isapprox(Hs * xhs, chs, 1e-5);
%=============================================================================
[xfhs, flagfhs, relresfhs, iterfhs] = minres(Hs, chs, 1e-6, 20, @(x) sparse(x));
asserts.isequal(class(xfhs), 'single');
asserts.isequal(flagfhs, 0);
asserts.istrue(relresfhs < 1e-6);
asserts.isequal(iterfhs, 2);
asserts.isapprox(Hs * xfhs, chs, 1e-5);
%=============================================================================
cc = [1 + 1i; 2 - 1i];
[ymix1, mixflag1, mixrelres1, mixiter1, mixresvec1] = minres(S, cc, 1e-12, 20);
asserts.isequal(mixflag1, 0);
asserts.istrue(mixrelres1 < 1e-12);
asserts.isequal(mixiter1, 2);
asserts.isapprox(mixresvec1(1:2), [2.645751311064591; 2.53546276418555], 1e-12);
asserts.istrue(mixresvec1(end) < 1e-12);
asserts.isapprox(ymix1, [2 - 1i; 1 + 1i], 1e-12);
asserts.istrue(norm(S * ymix1 - cc) < 1e-10);
%=============================================================================
hr = [1; 3];
[ymix2, mixflag2, mixrelres2, mixiter2, mixresvec2] = minres(H, hr, 1e-12, 20);
asserts.isequal(mixflag2, 0);
asserts.istrue(mixrelres2 < 1e-12);
asserts.isequal(mixiter2, 2);
asserts.isapprox(mixresvec2(1:2), [3.16227766016838; 3.158220855855235], 1e-12);
asserts.istrue(mixresvec2(end) < 1e-12);
asserts.isapprox(ymix2, [1 + 0.75i; -1.25 - 0.25i], 1e-12);
asserts.istrue(norm(H * ymix2 - hr) < 1e-10);
%=============================================================================
x0 = ones(3, 1);
[x0r, flag0, relres0, iter0, resvec0] = minres(A, b, 1e-12, 20, [], [], x0);
asserts.isequal(flag0, 0);
asserts.istrue(relres0 < 1e-12);
asserts.isequal(iter0, 3);
asserts.isapprox(resvec0(1:3), [16.492422502470642; 4.918536367660608; 1.112024145938127], 1e-12);
asserts.istrue(resvec0(end) < 1e-12);
asserts.isapprox(x0r, [5; 5; 5], 1e-12);
asserts.isapprox(A * x0r, b, 1e-10);
%=============================================================================
[x0s, flag0s, relres0s, iter0s] = minres(A, b, 1e-12, 20, [], [], sparse(x0));
asserts.isequal(flag0s, 0);
asserts.istrue(relres0s < 1e-12);
asserts.isequal(iter0s, 3);
asserts.isapprox(x0s, [5; 5; 5], 1e-12);
asserts.isapprox(A * x0s, b, 1e-10);
x0c = 1i * ones(3, 1);
[x0cResult, flag0c, relres0c] = minres(A, b, 1e-12, 20, [], [], x0c);
asserts.isequal(flag0c, 0);
asserts.istrue(relres0c < 1e-12);
asserts.istrue(norm(A * x0cResult - b) < 1e-10);
%=============================================================================
[xm, flagm, relresm, iterm, resvecm] = minres(A, b, 1e-16, 1);
asserts.isequal(flagm, 1);
asserts.isapprox(relresm, 0.298230073048625, 1e-12);
asserts.isequal(iterm, 1);
asserts.isapprox(xm, [5.28; 3.52; 3.52], 1e-12);
asserts.isapprox(resvecm, [20.615528128088304; 6.14817045957576], 1e-12);
%=============================================================================
[xzeroit, flagzeroit, relreszeroit, iterzeroit, resveczeroit] = minres(A, b, 1e-12, 0);
asserts.isequal(flagzeroit, 1);
asserts.isapprox(relreszeroit, 0.298230073048625, 1e-12);
asserts.isequal(iterzeroit, 1);
asserts.isapprox(xzeroit, [5.28; 3.52; 3.52], 1e-12);
asserts.isapprox(resveczeroit, [20.615528128088304; 6.14817045957576], 1e-12);
%=============================================================================
[xzr, flagzr, relreszr, iterzr, resveczr] = minres(A, zeros(3, 1), 1e-12, 20);
asserts.isequal(flagzr, 0);
asserts.isequal(relreszr, 0);
asserts.isequal(iterzr, 0);
asserts.isequal(xzr, zeros(3, 1));
asserts.isequal(resveczr, 0);
%=============================================================================
Z = sparse(zeros(2, 2));
[xz, flagz, relresz, iterz, resvecz] = minres(Z, [1; 0], 1e-12, 5);
asserts.isequal(flagz, 4);
asserts.isequal(relresz, 1);
asserts.isequal(iterz, 1);
asserts.isequal(resvecz, [1; 1]);
asserts.istrue(isinf(xz(1)) && isnan(xz(2)));
%=============================================================================
asserts.checkerror('minres(A, b, -1)', sprintf(_('%s must be a nonnegative scalar.'), 'Tolerance'));
asserts.checkerror('minres(A, b, [], -1)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('minres(A, b, [], Inf)', _('Expected a nonnegative integer scalar.'));
asserts.checkerror('minres(sparse([1 2]), [1; 2])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('minres(A, [1; 2])', sprintf(_('%s dimensions must agree.'), 'Matrix and vector'));
asserts.checkerror('minres(A, b, [], [], [], [], ones(2, 1))', _('Initial guess dimensions must agree.'));
asserts.checkerror('minres(A, b, [], [], [1; 2])', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('minres(A, b, [], [], @(x) x(1:2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('minres(A, b, [], [], speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('minres(A, b, [], [], speye(3), speye(2))', sprintf(_('%s dimensions must agree.'), 'Preconditioner'));
asserts.checkerror('minres(A, b, [], [], [1; 0; 1])', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('minres(sparse(1), 1, [], [], sparse(0))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('minres(A, b, [], [], sparse(diag([1; 0; 1])))', _('Preconditioner diagonal must not contain zeros.'));
asserts.checkerror('minres(A, b, [], [], [1 2 0; 2 4 0; 0 0 1])', sprintf(_('%s failed.'), 'Preconditioner factorization'));
%=============================================================================
