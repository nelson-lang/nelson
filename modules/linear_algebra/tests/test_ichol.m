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
asserts.isequal(nargin('ichol'), 2);
asserts.isequal(nargout('ichol'), 1);
%=============================================================================
A = sparse([4 -1 0; -1 4 -1; 0 -1 3]);
L = ichol(A);
REF_L = [2 0 0; -0.5 1.9364916731037085 0; 0 -0.5163977794943222 1.6532795690182993];
asserts.istrue(issparse(L));
asserts.isequal(size(L), size(A));
asserts.isapprox(full(L), REF_L, 1e-12);
asserts.isequal(nnz(L), 5);
asserts.istrue(norm(full(L * L' - A), 'fro') < norm(full(A), 'fro'));
%=============================================================================
opts.type = 'nofill';
Ln = ichol(A, opts);
asserts.istrue(issparse(Ln));
asserts.isequal(size(Ln), size(A));
asserts.isapprox(full(Ln), REF_L, 1e-12);
%=============================================================================
REF_R = [2 -0.5 0; 0 1.9364916731037085 -0.5163977794943222; 0 0 1.6532795690182993];
optsString.type = "ict";
optsString.shape = "upper";
optsString.droptol = 0;
Rstr = ichol(A, optsString);
asserts.istrue(issparse(Rstr));
asserts.isequal(size(Rstr), size(A));
asserts.isapprox(full(Rstr), REF_R, 1e-12);
%=============================================================================
opts.shape = 'upper';
R = ichol(A, opts);
asserts.istrue(issparse(R));
asserts.isequal(size(R), size(A));
asserts.isapprox(full(R), REF_R, 1e-12);
asserts.istrue(norm(full(R' * R - A), 'fro') < norm(full(A), 'fro'));
%=============================================================================
As = sparse(single([4 -1 0; -1 4 -1; 0 -1 3]));
Ls = ichol(As);
asserts.istrue(issparse(Ls));
asserts.isequal(class(Ls), 'single');
asserts.isequal(size(Ls), size(As));
asserts.isapprox(full(Ls), single(REF_L), 1e-6);
bs = single([15; 10; 10]);
[xs, flags, relress, iters] = pcg(As, bs, 1e-6, 20, Ls, Ls');
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.istrue(relress < 1e-6);
asserts.isequal(iters, 1);
asserts.isapprox(As * xs, bs, 1e-4);
opts.shape = 'upper';
Rs = ichol(As, opts);
asserts.istrue(issparse(Rs));
asserts.isequal(class(Rs), 'single');
asserts.isapprox(full(Rs), single(REF_R), 1e-6);
%=============================================================================
AZ = A;
AZ(1, 3) = 5;
AZ(1, 3) = 0;
AZ(3, 1) = 5;
AZ(3, 1) = 0;
Lz = ichol(AZ);
asserts.istrue(issparse(Lz));
asserts.isapprox(full(Lz), REF_L, 1e-12);
asserts.isequal(nnz(Lz), 5);
%=============================================================================
opts2.type = 'ict';
opts2.droptol = 0;
opts2.diagcomp = 0;
Li = ichol(A, opts2);
asserts.istrue(issparse(Li));
asserts.isequal(size(Li), size(A));
asserts.isapprox(full(Li), REF_L, 1e-12);
%=============================================================================
opts3.type = 'ict';
opts3.droptol = 0.6;
Ld = ichol(A, opts3);
REF_DROP_L = [2 0 0; 0 2 0; 0 0 1.7320508075688772];
asserts.istrue(issparse(Ld));
asserts.isequal(size(Ld), size(A));
asserts.isapprox(full(Ld), REF_DROP_L, 1e-12);
asserts.isequal(nnz(Ld), 3);
asserts.istrue(norm(full(Ld * Ld' - A), 'fro') < norm(full(A), 'fro'));
Lds = ichol(As, opts3);
asserts.isequal(class(Lds), 'single');
asserts.isapprox(full(Lds), single(REF_DROP_L), 1e-6);
asserts.isequal(nnz(Lds), 3);
%=============================================================================
opts3.michol = 'on';
Lm = ichol(A, opts3);
REF_MICHOL_L = [1.7320508075688774 0 0; 0 1.4142135623730949 0; 0 0 1.4142135623730949];
asserts.istrue(issparse(Lm));
asserts.isapprox(full(Lm), REF_MICHOL_L, 1e-12);
asserts.isequal(nnz(Lm), 3);
asserts.isapprox(full(A * ones(3, 1)), full(Lm * Lm' * ones(3, 1)), 1e-12);
opts3.michol = 'off';
Lmo = ichol(A, opts3);
asserts.isapprox(full(Lmo), REF_DROP_L, 1e-12);
%=============================================================================
opts4.diagcomp = 1;
Ls = ichol(A, opts4);
REF_DIAGCOMP_L = [2.8284271247461903 0 0; -0.35355339059327373 2.806243040080456 0; 0 -0.35634832254989918 2.423430599999902];
asserts.isapprox(full(Ls), REF_DIAGCOMP_L, 1e-12);
%=============================================================================
B = sparse([1 2; 2 1]);
optsb.diagcomp = 2;
Lb = ichol(B, optsb);
REF_DIAGCOMP_HARD_L = [1.7320508075688772 0; 1.1547005383792517 1.2909944487358056];
asserts.istrue(issparse(Lb));
asserts.isapprox(full(Lb), REF_DIAGCOMP_HARD_L, 1e-12);
optsb.type = 'ict';
optsb.droptol = 0;
Lbt = ichol(B, optsb);
asserts.isapprox(full(Lbt), REF_DIAGCOMP_HARD_L, 1e-12);
%=============================================================================
T = spdiags([-ones(5, 1) 4 * ones(5, 1) -ones(5, 1)], -1:1, 5, 5);
opts5.type = 'ict';
opts5.droptol = 0.25;
opts5.diagcomp = 0.5;
Lt = ichol(T, opts5);
asserts.istrue(issparse(Lt));
asserts.isequal(nnz(Lt), 5);
asserts.isapprox(full(Lt), sqrt(6) * eye(5, 5), 1e-12);
opts5.michol = 'on';
Ltm = ichol(T, opts5);
REF_T_MICHOL_L = diag([2.2360679774997898; 2; 2; 2; 2.2360679774997898]);
asserts.istrue(issparse(Ltm));
asserts.isequal(nnz(Ltm), 5);
asserts.isapprox(full(Ltm), REF_T_MICHOL_L, 1e-12);
asserts.isapprox(full((T + 2 * speye(5)) * ones(5, 1)), full(Ltm * Ltm' * ones(5, 1)), 1e-12);
%=============================================================================
opts5.droptol = 0.1;
opts5.michol = 'off';
Lk = ichol(T, opts5);
REF_KEEP_L = [2.449489742783178 0 0 0 0; -0.408248290463863 2.41522945769824 0 0 0; 0 -0.414039335605413 2.41424344848887 0 0; 0 0 -0.414208434789757 2.41421444212171 0; 0 0 0 -0.414213411432151 2.41421358827046];
asserts.istrue(issparse(Lk));
asserts.isequal(nnz(Lk), 9);
asserts.isapprox(full(Lk), REF_KEEP_L, 1e-12);
%=============================================================================
H = sparse([4 1 + 1i; 1 - 1i 3]);
Lc = ichol(H);
REF_COMPLEX_L = [2 0; 0.5 - 0.5i 1.58113883008419];
asserts.istrue(issparse(Lc));
asserts.isequal(size(Lc), size(H));
asserts.isapprox(full(Lc), REF_COMPLEX_L, 1e-12);
asserts.istrue(norm(full(Lc * Lc' - H), 'fro') < 1e-12);
bc = [1 + 2i; 3 - 1i];
[xc, flagc, relresc, iterc, resvecc] = pcg(H, bc, 1e-12, 20, Lc, Lc');
asserts.isequal(flagc, 0);
asserts.istrue(relresc < 1e-12);
asserts.isequal(iterc, 1);
asserts.isapprox(resvecc(1), 3.872983346207417, 1e-12);
asserts.istrue(resvecc(end) < 1e-12);
asserts.isapprox(xc, [-0.1 + 0.4i; 0.9 - 0.5i], 1e-12);
asserts.istrue(norm(H * xc - bc) < 1e-10);
%=============================================================================
opts6.type = 'ict';
opts6.droptol = 0;
Lci = ichol(H, opts6);
asserts.isapprox(full(Lci), REF_COMPLEX_L, 1e-12);
opts6.shape = 'upper';
Rci = ichol(H, opts6);
REF_COMPLEX_R = [2 0.5 + 0.5i; 0 1.58113883008419];
asserts.istrue(issparse(Rci));
asserts.isapprox(full(Rci), REF_COMPLEX_R, 1e-12);
%=============================================================================
Hs = sparse(single([4 1 + 1i; 1 - 1i 3]));
Lcs = ichol(Hs);
asserts.istrue(issparse(Lcs));
asserts.isequal(class(Lcs), 'single');
asserts.isapprox(full(Lcs), single(REF_COMPLEX_L), 1e-6);
chs = single([1 + 2i; 3 - 1i]);
[xcs, cflags, crelress, citers] = pcg(Hs, chs, 1e-6, 20, Lcs, Lcs');
asserts.isequal(class(xcs), 'single');
asserts.isequal(cflags, 0);
asserts.istrue(crelress < 1e-6);
asserts.isequal(citers, 1);
asserts.isapprox(Hs * xcs, chs, 1e-4);
opts6s.type = 'ict';
opts6s.droptol = 0;
Lcis = ichol(Hs, opts6s);
asserts.istrue(issparse(Lcis));
asserts.isequal(class(Lcis), 'single');
asserts.isapprox(full(Lcis), single(REF_COMPLEX_L), 1e-6);
opts6s.shape = 'upper';
Rcis = ichol(Hs, opts6s);
asserts.istrue(issparse(Rcis));
asserts.isequal(class(Rcis), 'single');
asserts.isapprox(full(Rcis), single(REF_COMPLEX_R), 1e-6);
%=============================================================================
HC = sparse([1 2 + 1i; 2 - 1i 1]);
opts7.diagcomp = 3;
Lhc = ichol(HC, opts7);
REF_COMPLEX_DIAGCOMP_L = [2 0; 1 - 0.5i 1.6583123951777];
asserts.istrue(issparse(Lhc));
asserts.isapprox(full(Lhc), REF_COMPLEX_DIAGCOMP_L, 1e-12);
opts7.type = 'ict';
opts7.droptol = 0;
Lhct = ichol(HC, opts7);
asserts.isapprox(full(Lhct), REF_COMPLEX_DIAGCOMP_L, 1e-12);
HCs = sparse(single([1 2 + 1i; 2 - 1i 1]));
opts7s.diagcomp = 3;
Lhcs = ichol(HCs, opts7s);
asserts.istrue(issparse(Lhcs));
asserts.isequal(class(Lhcs), 'single');
asserts.isapprox(full(Lhcs), single(REF_COMPLEX_DIAGCOMP_L), 1e-6);
opts7s.type = 'ict';
opts7s.droptol = 0;
Lhcts = ichol(HCs, opts7s);
asserts.istrue(issparse(Lhcts));
asserts.isequal(class(Lhcts), 'single');
asserts.isapprox(full(Lhcts), single(REF_COMPLEX_DIAGCOMP_L), 1e-6);
%=============================================================================
asserts.checkerror('ichol(A, ones(1, 2))', _('Options must be a scalar structure.'));
asserts.checkerror('ichol(sparse([1 2]))', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('ichol(A, struct(''type'', ''bad''))', sprintf(_('Unsupported %s.'), 'incomplete Cholesky type'));
asserts.checkerror('ichol(A, struct(''type'', 1))', _('Option value must be a character row vector or a string scalar.'));
asserts.checkerror('ichol(A, struct(''droptol'', Inf))', sprintf(_('%s must be a finite real scalar.'), 'Option value'));
asserts.checkerror('ichol(A, struct(''droptol'', -1))', sprintf(_('%s must be a nonnegative scalar.'), 'droptol'));
asserts.checkerror('ichol(A, struct(''diagcomp'', Inf))', sprintf(_('%s must be a finite real scalar.'), 'Option value'));
asserts.checkerror('ichol(A, struct(''diagcomp'', -1))', sprintf(_('%s must be a nonnegative scalar.'), 'diagcomp'));
asserts.checkerror('ichol(A, struct(''michol'', ''bad''))', _('michol must be ''on'' or ''off''.'));
asserts.checkerror('ichol(A, struct(''michol'', 1))', _('Option value must be a character row vector or a string scalar.'));
asserts.checkerror('ichol(A, struct(''shape'', ''bad''))', sprintf(_('%s must be ''lower'' or ''upper''.'), 'shape'));
asserts.checkerror('ichol(A, struct(''shape'', 1))', _('Option value must be a character row vector or a string scalar.'));
asserts.checkerror('ichol(A, struct(''unknown'', 1))', sprintf(_('Unsupported %s.'), 'incomplete Cholesky option'));
asserts.checkerror('ichol(sparse([1 2; 0 1]))', _('Matrix must be symmetric.'));
asserts.checkerror('ichol(sparse([1 1i; 0 1]))', _('Matrix must be Hermitian.'));
asserts.checkerror('ichol(sparse([0 0; 0 1]), struct(''diagcomp'', 1))', _('Input matrix must have a structurally nonzero diagonal.'));
asserts.checkerror('ichol(sparse([1 2; 2 1]))', sprintf(_('%s failed.'), 'Incomplete Cholesky factorization'));
%=============================================================================
