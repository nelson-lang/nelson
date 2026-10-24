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
asserts.isequal(nargin('cgs'), 7);
asserts.isequal(nargout('cgs'), 5);
%=============================================================================
A = sparse([4 1 0; 2 3 1; 0 1 2]);
b = [1; 2; 3];
[x, flag, relres, iter, resvec] = cgs(A, b, 1e-12, 20);
REF_RESVEC = [3.7416573867739413; 0.72257673312553283; 0.1638855054072853; 0];
asserts.isequal(flag, 0);
asserts.istrue(relres < 1e-12);
asserts.isequal(iter, 3);
asserts.isequal(size(resvec, 2), 1);
asserts.isapprox(resvec(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvec(end) < 1e-12);
asserts.isapprox(x(1), 0.25, 1e-12);
asserts.istrue(abs(x(2)) < 1e-12);
asserts.isapprox(x(3), 1.5, 1e-12);
asserts.isapprox(A * x, b, 1e-10);
%=============================================================================
[xd, flagd, relresd, iterd, resvecd] = cgs(A, b);
REF_DEFAULT_RESVEC = [3.7416573867739413; 0.72257673312553283; 0.1638855054072853; 0];
asserts.isequal(flagd, 0);
asserts.istrue(relresd < 1e-12);
asserts.isequal(iterd, 3);
asserts.isapprox(resvecd(1:3), REF_DEFAULT_RESVEC(1:3), 1e-12);
asserts.istrue(resvecd(end) < 1e-12);
asserts.isapprox(xd(1), 0.25, 1e-12);
asserts.istrue(abs(xd(2)) < 1e-12);
asserts.isapprox(xd(3), 1.5, 1e-12);
asserts.isapprox(A * xd, b, 1e-10);
%=============================================================================
As = sparse(single([4 1 0; 2 3 1; 0 1 2]));
bs = single([1; 2; 3]);
[xs, flags, relress, iters, resvecs] = cgs(As, bs, 1e-6, 20);
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.istrue(relress < 1e-6);
asserts.isequal(iters, 3);
asserts.isapprox(resvecs, single(REF_DEFAULT_RESVEC), 1e-5);
asserts.isapprox(xs, single([0.25; 0; 1.5]), 1e-5);
asserts.isapprox(As * xs, bs, 1e-4);
%=============================================================================
Ds = single(diag(full(As)));
[xjs, flagjs, relresjs, iterjs, resvecjs] = cgs(As, bs, 1e-6, 30, Ds);
asserts.isequal(class(xjs), 'single');
asserts.isequal(flagjs, 0);
asserts.istrue(relresjs < 1e-6);
asserts.isequal(iterjs, 3);
asserts.isapprox(resvecjs, single([3.7416573867739413; 0.50046295867778112; 0.11821607335722162; 0]), 1e-5);
asserts.isapprox(xjs, single([0.25; 0; 1.5]), 1e-5);
asserts.isapprox(As * xjs, bs, 1e-4);
%=============================================================================
