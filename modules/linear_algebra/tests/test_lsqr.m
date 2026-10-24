%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
asserts.isequal(nargin('lsqr'), 7);
asserts.isequal(nargout('lsqr'), 6);
%=============================================================================
A = sparse([4 1 0; 2 3 1; 0 1 2]);
b = [1; 2; 3];
[x, flag, relres, iter, resvec, lsvec] = lsqr(A, b, 1e-12, 20);
REF_RESVEC = [3.7416573867739413; 2.0722570882688536; 1.009922135498653; 0];
REF_LSVEC = [3.1154053604819216; 0.87608249821832174; 0.1961960595253307; 0];
asserts.isequal(flag, 0);
asserts.istrue(relres < 1e-12);
asserts.isequal(iter, 3);
asserts.isequal(size(resvec, 2), 1);
asserts.isapprox(resvec, REF_RESVEC, 1e-12);
asserts.isequal(size(lsvec), size(resvec));
asserts.isapprox(lsvec(1:3), REF_LSVEC(1:3), 1e-12);
asserts.istrue(lsvec(end) < 1e-12);
asserts.isapprox(x, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * x, b, 1e-10);
%=============================================================================
[L, U] = ilu(A);
[xp, flagp, relresp, iterp, resvecp, lsvecp] = lsqr(A, b, 1e-12, 30, L, U);
asserts.isequal(flagp, 0);
asserts.istrue(relresp < 1e-12);
asserts.isequal(iterp, 1);
asserts.isapprox(resvecp(1), 3.7416573867739413, 1e-12);
asserts.istrue(resvecp(end) < 1e-12);
asserts.isequal(size(lsvecp), size(resvecp));
asserts.isapprox(lsvecp(1), 3.7416573867739413, 1e-12);
asserts.istrue(lsvecp(end) < 1e-12);
asserts.isapprox(xp, [0.25; 0; 1.5], 1e-12);
asserts.isapprox(A * xp, b, 1e-10);
%=============================================================================
R = sparse([1 0; 0 1; 1 1; 2 -1]);
c = [1; 2; 4; 1];
[xr, flagr, relresr, iterr, resvecr, lsvecr] = lsqr(R, c, 1e-12, 20);
asserts.isequal(flagr, 0);
asserts.isequal(iterr, 2);
asserts.isapprox(relresr, 0.13680854337396819, 1e-12);
asserts.isapprox(resvecr, [4.6904157598234297; 1.9197965332547597; 0.64168894791974773], 1e-12);
asserts.isequal(size(lsvecr), size(resvecr));
asserts.isapprox(lsvecr(1:2), [4.2795304965501781; 1.2371237006114824], 1e-12);
asserts.istrue(lsvecr(end) < 1e-12);
asserts.isapprox(xr, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xr - c), resvecr(end), 1e-10);
%=============================================================================
[xd, flagd, relresd, iterd, resvecd, lsvecd] = lsqr(R, c);
REF_DEFAULT_RESVEC = [4.6904157598234297; 1.9197965332547597; 0.64168894791974773];
REF_DEFAULT_LSVEC = [4.2795304965501781; 1.2371237006114824];
asserts.isequal(flagd, 0);
asserts.isapprox(relresd, 0.13680854337396819, 1e-12);
asserts.isequal(iterd, 2);
asserts.isapprox(resvecd, REF_DEFAULT_RESVEC, 1e-12);
asserts.isapprox(lsvecd(1:2), REF_DEFAULT_LSVEC, 1e-12);
asserts.istrue(lsvecd(end) < 1e-12);
asserts.isapprox(xd, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xd - c), resvecd(end), 1e-10);
%=============================================================================
