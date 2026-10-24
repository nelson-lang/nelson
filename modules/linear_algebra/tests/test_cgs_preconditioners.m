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
REF_DEFAULT_RESVEC = [3.7416573867739413; 0.72257673312553283; 0.1638855054072853; 0];
%=============================================================================
Mfun = @(x) x;
[xfh1, flagfh1, relresfh1, iterfh1, resvecfh1] = cgs(A, b, 1e-12, 20, Mfun);
asserts.isequal(flagfh1, 0);
asserts.istrue(relresfh1 < 1e-12);
asserts.isequal(iterfh1, 3);
asserts.isapprox(resvecfh1(1:3), REF_DEFAULT_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfh1(end) < 1e-12);
asserts.isapprox(xfh1, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xfh1, b, 1e-10);
%=============================================================================
[xfh2, flagfh2, relresfh2, iterfh2, resvecfh2] = cgs(A, b, 1e-12, 20, [], Mfun);
asserts.isequal(flagfh2, 0);
asserts.istrue(relresfh2 < 1e-12);
asserts.isequal(iterfh2, 3);
asserts.isapprox(resvecfh2(1:3), REF_DEFAULT_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfh2(end) < 1e-12);
asserts.isapprox(xfh2, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xfh2, b, 1e-10);
%=============================================================================
MfunSparse = @(x) sparse(x);
[xfs1, flagfs1, relresfs1, iterfs1, resvecfs1] = cgs(A, b, 1e-12, 20, MfunSparse);
asserts.isequal(flagfs1, 0);
asserts.istrue(relresfs1 < 1e-12);
asserts.isequal(iterfs1, 3);
asserts.isapprox(resvecfs1(1:3), REF_DEFAULT_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfs1(end) < 1e-12);
asserts.isapprox(xfs1, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xfs1, b, 1e-10);
%=============================================================================
[xfs2, flagfs2, relresfs2, iterfs2, resvecfs2] = cgs(A, b, 1e-12, 20, [], MfunSparse);
asserts.isequal(flagfs2, 0);
asserts.istrue(relresfs2 < 1e-12);
asserts.isequal(iterfs2, 3);
asserts.isapprox(resvecfs2(1:3), REF_DEFAULT_RESVEC(1:3), 1e-12);
asserts.istrue(resvecfs2(end) < 1e-12);
asserts.isapprox(xfs2, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xfs2, b, 1e-10);
%=============================================================================
[L, U] = ilu(A);
[xp, flagp, relresp, iterp, resvecp] = cgs(A, b, 1e-12, 30, L, U);
asserts.isequal(flagp, 0);
asserts.istrue(relresp < 1e-12);
asserts.isequal(iterp, 1);
asserts.isapprox(resvecp, [3.7416573867739413; 8.8817841970012523e-16], 1e-12);
asserts.isapprox(xp, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xp, b, 1e-10);
%=============================================================================
D = diag(full(A));
[xj, flagj, relresj, iterj, resvecj] = cgs(A, b, 1e-12, 30, D);
REF_JACOBI_RESVEC = [3.7416573867739413; 0.50046295867778112; 0.11821607335722162; 0];
asserts.isequal(flagj, 0);
asserts.istrue(relresj < 1e-12);
asserts.isequal(iterj, 3);
asserts.isapprox(resvecj(1:3), REF_JACOBI_RESVEC(1:3), 1e-12);
asserts.istrue(resvecj(end) < 1e-12);
asserts.isapprox(xj(1), 0.25, 1e-12);
asserts.istrue(abs(xj(2)) < 1e-12);
asserts.isapprox(xj(3), 1.5, 1e-12);
asserts.isapprox(A * xj, b, 1e-10);
%=============================================================================
Mfull = diag(D);
[xmfull, flagmfull, relresmfull] = cgs(A, b, 1e-12, 30, Mfull);
asserts.isequal(flagmfull, 0);
asserts.istrue(relresmfull < 1e-12);
asserts.isapprox(A * xmfull, b, 1e-10);
%=============================================================================
Msparse = sparse(Mfull);
[xmsparse, flagmsparse, relresmsparse] = cgs(A, b, 1e-12, 30, Msparse);
asserts.isequal(flagmsparse, 0);
asserts.istrue(relresmsparse < 1e-12);
asserts.isapprox(A * xmsparse, b, 1e-10);
%=============================================================================
A2 = sparse([4 1; 2 3]);
b2 = [5; 5];
M1d = [2 0; 0 1];
M2d = [2 0.5; 2 3];
[xsplit, flagsplit, relressplit, itersplit, resvecsplit] = cgs(A2, b2, 1e-12, 10, M1d, M2d);
asserts.isequal(flagsplit, 0);
asserts.istrue(relressplit < 1e-12);
asserts.isequal(itersplit, 1);
asserts.isapprox(resvecsplit, [sqrt(50); 0], 1e-12);
asserts.isapprox(xsplit, [1; 1], 1e-12);
asserts.isapprox(A2 * xsplit, b2, 1e-10);
%=============================================================================
