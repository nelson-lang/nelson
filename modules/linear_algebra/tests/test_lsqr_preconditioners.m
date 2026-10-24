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
R = sparse([1 0; 0 1; 1 1; 2 -1]);
c = [1; 2; 4; 1];
D = [1; 2];
[xj, flagj, relresj, iterj, resvecj, lsvecj] = lsqr(R, c, 1e-12, 20, D);
REF_JACOBI_RESVEC = [4.6904157598234297; 3.3382678153666303; 0.64168894791974818];
REF_JACOBI_LSVEC = [3.2948396004794085; 1.1522758370761763; 0];
asserts.isequal(flagj, 0);
asserts.isapprox(relresj, 0.13680854337396831, 1e-12);
asserts.isequal(iterj, 2);
asserts.isapprox(resvecj, REF_JACOBI_RESVEC, 1e-12);
asserts.isequal(size(lsvecj), size(resvecj));
asserts.isapprox(lsvecj(1:2), REF_JACOBI_LSVEC(1:2), 1e-12);
asserts.istrue(lsvecj(end) < 1e-12);
asserts.isapprox(xj, [1.529411764705882; 2.1764705882352948], 1e-12);
asserts.isapprox(norm(R * xj - c), resvecj(end), 1e-10);
Dmatrix = sparse(diag(D));
[xjm, flagjm, relresjm, iterjm, resvecjm, lsvecjm] = lsqr(R, c, 1e-12, 20, Dmatrix);
asserts.isequal(flagjm, 0);
asserts.isapprox(relresjm, 0.13680854337396831, 1e-12);
asserts.isequal(iterjm, 2);
asserts.isapprox(resvecjm, REF_JACOBI_RESVEC, 1e-12);
asserts.isequal(size(lsvecjm), size(resvecjm));
asserts.isapprox(lsvecjm(1:2), REF_JACOBI_LSVEC(1:2), 1e-12);
asserts.istrue(lsvecjm(end) < 1e-12);
asserts.isapprox(xjm, [1.529411764705882; 2.1764705882352948], 1e-12);
asserts.isapprox(norm(R * xjm - c), resvecjm(end), 1e-10);
Dc = D + [1i; 0];
[xjc, flagjc, relresjc] = lsqr(R, c, 1e-12, 20, Dc);
asserts.isequal(flagjc, 0);
asserts.isapprox(relresjc, 0.13680854337396819, 1e-12);
asserts.isapprox(norm(R * xjc - c), 0.64168894791974773, 1e-10);
%=============================================================================
Mfull = full([2 1; 1 3]);
[xf, flagf, relresf, iterf, resvecf, lsvecf] = lsqr(R, c, 1e-12, 20, Mfull);
REF_FULL_PRECOND_RESVEC = [4.6904157598234297; 4.09141789292317; 0.641688947919749];
REF_FULL_PRECOND_LSVEC = [2.29353430876195; 1.27301331628649; 1.02314196104134e-15];
asserts.isequal(flagf, 0);
asserts.isapprox(relresf, 0.136808543373968, 1e-12);
asserts.isequal(iterf, 2);
asserts.isapprox(resvecf, REF_FULL_PRECOND_RESVEC, 1e-12);
asserts.isequal(size(lsvecf), size(resvecf));
asserts.isapprox(lsvecf, REF_FULL_PRECOND_LSVEC, 1e-12);
asserts.isapprox(xf, [1.52941176470588; 2.1764705882353], 1e-12);
asserts.isapprox(norm(R * xf - c), resvecf(end), 1e-10);
%=============================================================================
M1d = [2 0; 0 1];
M2d = [1 0.5; 0 3];
[xsplit, flagsplit, relressplit, itersplit, resvecsplit, lsvecsplit] = lsqr(R, c, 1e-12, 20, M1d, M2d);
asserts.isequal(flagsplit, 0);
asserts.isapprox(relressplit, 0.13680854337396825, 1e-12);
asserts.isequal(itersplit, 2);
asserts.isapprox(resvecsplit, [4.6904157598234297; 3.5144885122266234; 0.64168894791974795], 1e-12);
asserts.isapprox(lsvecsplit, [3.1061826246096831; 1.4357847907971439; 0], 1e-12);
asserts.isapprox(xsplit, [1.5294117647058834; 2.1764705882352948], 1e-12);
asserts.isapprox(norm(R * xsplit - c), resvecsplit(end), 1e-10);
%=============================================================================
Mns = [2 1; 0 3];
MnsFun = @(x, t)(strcmp(t, 'notransp') .* (Mns \ x) + strcmp(t, 'transp') .* (Mns' \ x));
asserts.istrue(norm(MnsFun([1; 1], 'notransp') - MnsFun([1; 1], 'transp')) > 0);
[xfunm, flagfunm, relresfunm, iterfunm, resvecfunm, lsvecfunm] = lsqr(R, c, 1e-12, 20, MnsFun);
[xmatm, flagmatm, relresmatm, itermatm, resvecmatm, lsvecmatm] = lsqr(R, c, 1e-12, 20, Mns);
asserts.isequal(flagfunm, flagmatm);
asserts.isequal(iterfunm, itermatm);
asserts.isapprox(relresfunm, relresmatm, 1e-12);
asserts.isapprox(resvecfunm, resvecmatm, 1e-12);
asserts.isapprox(lsvecfunm, lsvecmatm, 1e-12);
asserts.isapprox(xfunm, xmatm, 1e-12);
asserts.isapprox(norm(R * xfunm - c), resvecfunm(end), 1e-10);
%=============================================================================
