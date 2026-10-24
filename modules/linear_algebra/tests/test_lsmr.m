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
asserts.isequal(nargin('lsmr'), 7);
asserts.isequal(nargout('lsmr'), 6);
%=============================================================================
A = sparse([4 1 0; 2 3 1; 0 1 2]);
b = [1; 2; 3];
[x, flag, relres, iter, resvec, lsvec] = lsmr(A, b, 1e-12, 20);
REF_RESVEC = [3.7416573867739413; 2.097093372796366; 1.015073540825995; 0];
REF_LSVEC = [15.0996688705415; 4.852872701137604; 1.143999965124791; 0];
asserts.isequal(flag, 0);
asserts.istrue(relres < 1e-12);
asserts.isequal(iter, 3);
asserts.isequal(size(resvec, 2), 1);
asserts.isapprox(resvec(1:3), REF_RESVEC(1:3), 1e-12);
asserts.istrue(resvec(end) < 1e-12);
asserts.isequal(size(lsvec), size(resvec));
asserts.isapprox(lsvec(1:3), REF_LSVEC(1:3), 1e-12);
asserts.istrue(lsvec(end) < 1e-12);
asserts.isapprox(x(1), 0.25, 1e-12);
asserts.istrue(abs(x(2)) < 1e-12);
asserts.isapprox(x(3), 1.5, 1e-12);
asserts.isapprox(A * x, b, 1e-10);
%=============================================================================
[L, U] = ilu(A);
[xp, flagp, relresp, iterp, resvecp, lsvecp] = lsmr(A, b, 1e-12, 30, L, U);
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
[xr, flagr, relresr, iterr, resvecr, lsvecr] = lsmr(R, c, 1e-12, 20);
asserts.isequal(flagr, 0);
asserts.isequal(iterr, 2);
asserts.isapprox(relresr, 0.13680854337396819, 1e-12);
asserts.isapprox(resvecr, [4.6904157598234297; 2.033872705011283; 0.641688947919748], 1e-12);
asserts.isequal(size(lsvecr), size(resvecr));
asserts.isapprox(lsvecr(1:2), [8.602325267042625; 3.407741279506566], 1e-12);
asserts.isapprox(xr, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xr - c), resvecr(end), 1e-10);
asserts.istrue(lsvecr(end) < 1e-12);
%=============================================================================
Rs = sparse(single([1 0; 0 1; 1 1; 2 -1]));
cs = single([1; 2; 4; 1]);
[xs, flags, relress, iters, resvecs, lsvecs] = lsmr(Rs, cs, 1e-6, 20);
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.isapprox(relress, 0.13680854337396819, 1e-6);
asserts.isequal(iters, 2);
asserts.isapprox(resvecs, [4.6904157598234297; 2.033872705011283; 0.641688947919748], 1e-5);
asserts.isapprox(lsvecs, [8.602325267042625; 3.407741279506566; 0], 1e-5);
asserts.isapprox(xs, single([1.5294117647058829; 2.1764705882352953]), 1e-5);
asserts.isapprox(norm(Rs * xs - cs), resvecs(end), 1e-5);
%=============================================================================
Ds = single([1; 2]);
[xjs, flagjs, relresjs, iterjs] = lsmr(Rs, cs, 1e-6, 20, Ds);
asserts.isequal(class(xjs), 'single');
asserts.isequal(flagjs, 0);
asserts.isapprox(relresjs, 0.13680854337396819, 1e-6);
asserts.isequal(iterjs, 2);
asserts.isapprox(norm(Rs * xjs - cs), 0.641688947919748, 1e-5);
%=============================================================================
[xfun, flagfun, relresfun, iterfun, resvecfun, lsvecfun] = lsmr(R, c, 1e-12, 20, @(x, t) x);
asserts.isequal(flagfun, 0);
asserts.isapprox(relresfun, 0.13680854337396819, 1e-12);
asserts.isequal(iterfun, 2);
asserts.isapprox(resvecfun, [4.6904157598234297; 2.033872705011283; 0.641688947919748], 1e-12);
asserts.isapprox(lsvecfun(1:2), [8.602325267042625; 3.407741279506566], 1e-12);
asserts.istrue(lsvecfun(end) < 1e-12);
asserts.isapprox(xfun, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xfun - c), resvecfun(end), 1e-10);
%=============================================================================
[xfun2, flagfun2, relresfun2, iterfun2, resvecfun2, lsvecfun2] = lsmr(R, c, 1e-12, 20, [], @(x, t) x);
asserts.isequal(flagfun2, 0);
asserts.isapprox(relresfun2, 0.13680854337396819, 1e-12);
asserts.isequal(iterfun2, 2);
asserts.isapprox(resvecfun2, [4.6904157598234297; 2.033872705011283; 0.641688947919748], 1e-12);
asserts.isapprox(lsvecfun2(1:2), [8.602325267042625; 3.407741279506566], 1e-12);
asserts.istrue(lsvecfun2(end) < 1e-12);
asserts.isapprox(xfun2, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xfun2 - c), resvecfun2(end), 1e-10);
%=============================================================================
[xfuns, flagfuns, relresfuns, iterfuns] = lsmr(R, c, 1e-12, 20, @(x, t) sparse(x));
asserts.isequal(flagfuns, 0);
asserts.isapprox(relresfuns, 0.13680854337396819, 1e-12);
asserts.isequal(iterfuns, 2);
asserts.isapprox(xfuns, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xfuns - c), 0.64168894791974773, 1e-12);
%=============================================================================
D = [1; 2];
[xj, flagj, relresj, iterj, resvecj, lsvecj] = lsmr(R, c, 1e-12, 20, D);
asserts.isequal(flagj, 0);
asserts.isequal(iterj, 2);
asserts.isapprox(relresj, 0.13680854337396819, 1e-12);
asserts.isapprox(resvecj, [4.6904157598234297; 3.369793839376338; 0.641688947919748], 1e-12);
asserts.isequal(size(lsvecj), size(resvecj));
asserts.isapprox(lsvecj(1:2), [7.433034373659252; 2.776934042349611], 1e-12);
asserts.isapprox(xj, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xj - c), resvecj(end), 1e-10);
asserts.istrue(lsvecj(end) < 1e-12);
Dmatrix = sparse(diag(D));
[xjm, flagjm, relresjm, iterjm, resvecjm, lsvecjm] = lsmr(R, c, 1e-12, 20, Dmatrix);
asserts.isequal(flagjm, 0);
asserts.isequal(iterjm, 2);
asserts.isapprox(relresjm, 0.13680854337396819, 1e-12);
asserts.isapprox(resvecjm, [4.6904157598234297; 3.369793839376338; 0.641688947919748], 1e-12);
asserts.isequal(size(lsvecjm), size(resvecjm));
asserts.isapprox(lsvecjm(1:2), [7.433034373659252; 2.776934042349611], 1e-12);
asserts.isapprox(xjm, [1.5294117647058829; 2.1764705882352953], 1e-12);
asserts.isapprox(norm(R * xjm - c), resvecjm(end), 1e-10);
asserts.istrue(lsvecjm(end) < 1e-12);
Dc = D + [1i; 0];
[xjc, flagjc, relresjc] = lsmr(R, c, 1e-12, 20, Dc);
asserts.isequal(flagjc, 0);
asserts.isapprox(relresjc, 0.13680854337396819, 1e-12);
asserts.isapprox(norm(R * xjc - c), 0.64168894791974773, 1e-10);
%=============================================================================
M1d = [2 0; 0 1];
M2d = [1 0.5; 0 3];
[xsplit, flagsplit, relressplit, itersplit, resvecsplit, lsvecsplit] = lsmr(R, c, 1e-12, 20, M1d, M2d);
asserts.isequal(flagsplit, 0);
asserts.isapprox(relressplit, 0.13680854337396825, 1e-12);
asserts.isequal(itersplit, 2);
asserts.isapprox(resvecsplit, [4.6904157598234297; 3.6048720207494713; 0.64168894791974795], 1e-12);
asserts.isapprox(lsvecsplit, [3.5355339059327369; 1.7966937585465443; 0], 1e-12);
asserts.isapprox(xsplit, [1.5294117647058825; 2.1764705882352939], 1e-12);
asserts.isapprox(norm(R * xsplit - c), resvecsplit(end), 1e-10);
%=============================================================================
Mns = [2 1; 0 3];
MnsFun = @(x, t)(strcmp(t, 'notransp') .* (Mns \ x) + strcmp(t, 'transp') .* (Mns' \ x));
asserts.istrue(norm(MnsFun([1; 1], 'notransp') - MnsFun([1; 1], 'transp')) > 0);
[xfunm, flagfunm, relresfunm, iterfunm, resvecfunm, lsvecfunm] = lsmr(R, c, 1e-12, 20, MnsFun);
[xmatm, flagmatm, relresmatm, itermatm, resvecmatm, lsvecmatm] = lsmr(R, c, 1e-12, 20, Mns);
asserts.isequal(flagfunm, flagmatm);
asserts.isequal(iterfunm, itermatm);
asserts.isapprox(relresfunm, relresmatm, 1e-12);
asserts.isapprox(resvecfunm, resvecmatm, 1e-12);
asserts.isapprox(lsvecfunm, lsvecmatm, 1e-12);
asserts.isapprox(xfunm, xmatm, 1e-12);
asserts.isapprox(norm(R * xfunm - c), resvecfunm(end), 1e-10);
%=============================================================================
