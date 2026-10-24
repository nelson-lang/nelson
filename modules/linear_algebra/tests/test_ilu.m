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
asserts.isequal(nargin('ilu'), 2);
asserts.isequal(nargout('ilu'), 3);
%=============================================================================
A = sparse([4 1 0; 2 3 1; 0 1 2]);
[L, U] = ilu(A);
REF_L = [1 0 0; 0.5 1 0; 0 0.4 1];
REF_U = [4 1 0; 0 2.5 1; 0 0 1.6];
asserts.istrue(issparse(L));
asserts.istrue(issparse(U));
asserts.isequal(size(L), size(A));
asserts.isequal(size(U), size(A));
asserts.isapprox(full(L), REF_L, 1e-12);
asserts.isapprox(full(U), REF_U, 1e-12);
asserts.isequal(nnz(L), 5);
asserts.isequal(nnz(U), 5);
asserts.istrue(norm(full(L * U - A), 'fro') < norm(full(A), 'fro'));
LU = ilu(A);
REF_LU = [4 1 0; 0.5 2.5 1; 0 0.4 1.6];
asserts.istrue(issparse(LU));
asserts.isapprox(full(LU), REF_LU, 1e-12);
%=============================================================================
opts.type = 'nofill';
[Ln, Un] = ilu(A, opts);
asserts.istrue(issparse(Ln));
asserts.istrue(issparse(Un));
asserts.isapprox(full(Ln), REF_L, 1e-12);
asserts.isapprox(full(Un), REF_U, 1e-12);
%=============================================================================
optsString.type = "nofill";
[Lstr, Ustr] = ilu(A, optsString);
asserts.istrue(issparse(Lstr));
asserts.istrue(issparse(Ustr));
asserts.isapprox(full(Lstr), REF_L, 1e-12);
asserts.isapprox(full(Ustr), REF_U, 1e-12);
%=============================================================================
As = sparse(single([4 1 0; 2 3 1; 0 1 2]));
[Ls, Us] = ilu(As);
asserts.istrue(issparse(Ls));
asserts.istrue(issparse(Us));
asserts.isequal(class(Ls), 'single');
asserts.isequal(class(Us), 'single');
asserts.isapprox(full(Ls), single(REF_L), 1e-6);
asserts.isapprox(full(Us), single(REF_U), 1e-6);
LUs = ilu(As);
asserts.istrue(issparse(LUs));
asserts.isequal(class(LUs), 'single');
asserts.isapprox(full(LUs), single(REF_LU), 1e-6);
bs = single([1; 2; 3]);
[xs, flags, relress, iters] = bicgstab(As, bs, 1e-6, 30, Ls, Us);
asserts.isequal(class(xs), 'single');
asserts.isequal(flags, 0);
asserts.istrue(relress < 1e-6);
asserts.isequal(iters, 0.5);
asserts.isapprox(As * xs, bs, 1e-4);
%=============================================================================
opts2.type = 'ilutp';
opts2.droptol = 1e-6;
opts2.fillfactor = 5;
opts2.udiag = false;
[Li, Ui] = ilu(A, opts2);
asserts.istrue(issparse(Li));
asserts.istrue(issparse(Ui));
asserts.isapprox(full(Li), REF_L, 1e-12);
asserts.isapprox(full(Ui), REF_U, 1e-12);
[Lip, Uip, Pip] = ilu(A, opts2);
asserts.istrue(issparse(Lip));
asserts.istrue(issparse(Uip));
asserts.istrue(issparse(Pip));
asserts.isapprox(full(Lip), REF_L, 1e-12);
asserts.isapprox(full(Uip), REF_U, 1e-12);
asserts.isapprox(full(Pip), eye(3, 3), 1e-12);
asserts.isapprox(full(Pip * A - Lip * Uip), zeros(3, 3), 1e-12);
%=============================================================================
optsu.type = 'nofill';
optsu.udiag = true;
[Lu, Uu] = ilu(A, optsu);
asserts.istrue(issparse(Lu));
asserts.istrue(issparse(Uu));
asserts.isapprox(full(Lu), REF_L, 1e-12);
asserts.isapprox(full(Uu), REF_U, 1e-12);
%=============================================================================
AZ = A;
AZ(1, 3) = 7;
AZ(1, 3) = 0;
[Lz, Uz] = ilu(AZ);
asserts.istrue(issparse(Lz));
asserts.istrue(issparse(Uz));
asserts.isapprox(full(Lz), REF_L, 1e-12);
asserts.isapprox(full(Uz), REF_U, 1e-12);
asserts.isequal(nnz(Lz), 5);
asserts.isequal(nnz(Uz), 5);
%=============================================================================
C = sparse([3 + 1i 1; 0 2 - 1i]);
[Lc, Uc] = ilu(C);
asserts.istrue(issparse(Lc));
asserts.istrue(issparse(Uc));
asserts.isapprox(full(Lc), eye(2, 2), 1e-12);
asserts.isapprox(full(Uc), full(C), 1e-12);
asserts.isapprox(C * bicgstab(C, [4 + 2i; 3 - 1i], 1e-12, 30, Lc, Uc), [4 + 2i; 3 - 1i], 1e-10);
optsc.type = 'ilutp';
optsc.droptol = 1e-8;
optsc.fillfactor = 5;
[Lct, Uct] = ilu(C, optsc);
asserts.istrue(issparse(Lct));
asserts.istrue(issparse(Uct));
asserts.isequal(size(Lct), size(C));
asserts.isequal(size(Uct), size(C));
asserts.isapprox(full(Lct), eye(2, 2), 1e-12);
asserts.isapprox(full(Uct), full(C), 1e-12);
asserts.isapprox(C * bicgstab(C, [4 + 2i; 3 - 1i], 1e-12, 30, Lct, Uct), [4 + 2i; 3 - 1i], 1e-10);
%=============================================================================
Cs = sparse(single([3 + 1i 1; 0 2 - 1i]));
[Lcs, Ucs] = ilu(Cs);
asserts.istrue(issparse(Lcs));
asserts.istrue(issparse(Ucs));
asserts.isequal(class(Lcs), 'single');
asserts.isequal(class(Ucs), 'single');
asserts.isapprox(full(Lcs), single(eye(2, 2)), 1e-6);
asserts.isapprox(full(Ucs), full(Cs), 1e-6);
d = single([4 + 2i; 3 - 1i]);
[ys, cflags, crelress, citers] = bicgstab(Cs, d, 1e-6, 30, Lcs, Ucs);
asserts.isequal(class(ys), 'single');
asserts.isequal(cflags, 0);
asserts.istrue(crelress < 1e-6);
asserts.isequal(citers, 0.5);
asserts.isapprox(Cs * ys, d, 1e-4);
optscs.type = 'ilutp';
optscs.droptol = 1e-8;
optscs.fillfactor = 5;
[Lcts, Ucts] = ilu(Cs, optscs);
asserts.istrue(issparse(Lcts));
asserts.istrue(issparse(Ucts));
asserts.isequal(class(Lcts), 'single');
asserts.isequal(class(Ucts), 'single');
asserts.isequal(size(Lcts), size(Cs));
asserts.isequal(size(Ucts), size(Cs));
asserts.isapprox(full(Lcts), single(eye(2, 2)), 1e-6);
asserts.isapprox(full(Ucts), full(Cs), 1e-6);
[yts, tflags, trelress, titers] = bicgstab(Cs, d, 1e-6, 30, Lcts, Ucts);
asserts.isequal(class(yts), 'single');
asserts.isequal(tflags, 0);
asserts.istrue(trelress < 1e-6);
asserts.isequal(titers, 0.5);
asserts.isapprox(Cs * yts, d, 1e-4);
%=============================================================================
P = sparse([0 1; 1 1]);
optp.type = 'ilutp';
optp.droptol = 0;
[Lp, Up] = ilu(P, optp);
asserts.istrue(issparse(Lp));
asserts.istrue(issparse(Up));
asserts.isapprox(full(Lp), [0 1; 1 0], 1e-12);
asserts.isapprox(full(Up), [1 1; 0 1], 1e-12);
asserts.isapprox(full(Lp * Up), full(P), 1e-12);
[Lpp, Upp, Ppp] = ilu(P, optp);
asserts.istrue(issparse(Lpp));
asserts.istrue(issparse(Upp));
asserts.istrue(issparse(Ppp));
asserts.isapprox(full(Lpp), eye(2, 2), 1e-12);
asserts.isapprox(full(Upp), [1 1; 0 1], 1e-12);
asserts.isapprox(full(Ppp), [0 1; 1 0], 1e-12);
asserts.isapprox(full(Ppp * P - Lpp * Upp), zeros(2, 2), 1e-12);
LUp = ilu(P, optp);
asserts.istrue(issparse(LUp));
asserts.isapprox(full(LUp), [1 1; 0 1], 1e-12);

Cp = sparse([0 1 + 2i; 3 - 1i 4]);
[Lcp, Ucp] = ilu(Cp, optp);
asserts.isapprox(full(Lcp), [0 1; 1 0], 1e-12);
asserts.isapprox(full(Ucp), [3 - 1i 4; 0 1 + 2i], 1e-12);
asserts.isapprox(full(Lcp * Ucp), full(Cp), 1e-12);
[Lcpp, Ucpp, Pcpp] = ilu(Cp, optp);
asserts.istrue(issparse(Pcpp));
asserts.isapprox(full(Lcpp), eye(2, 2), 1e-12);
asserts.isapprox(full(Ucpp), [3 - 1i 4; 0 1 + 2i], 1e-12);
asserts.isapprox(full(Pcpp), [0 1; 1 0], 1e-12);
asserts.isapprox(full(Pcpp * Cp - Lcpp * Ucpp), zeros(2, 2), 1e-12);

Cps = sparse(single([0 1 + 2i; 3 - 1i 4]));
[Lcps, Ucps] = ilu(Cps, optp);
asserts.istrue(issparse(Lcps));
asserts.istrue(issparse(Ucps));
asserts.isequal(class(Lcps), 'single');
asserts.isequal(class(Ucps), 'single');
asserts.isapprox(full(Lcps), single([0 1; 1 0]), 1e-6);
asserts.isapprox(full(Ucps), single([3 - 1i 4; 0 1 + 2i]), 1e-6);
asserts.isapprox(full(Lcps * Ucps), full(Cps), 1e-5);
[Lcpps, Ucpps, Pcpps] = ilu(Cps, optp);
asserts.istrue(issparse(Lcpps));
asserts.istrue(issparse(Ucpps));
asserts.istrue(issparse(Pcpps));
asserts.isequal(class(Lcpps), 'single');
asserts.isequal(class(Ucpps), 'single');
asserts.isapprox(full(Lcpps), single(eye(2, 2)), 1e-6);
asserts.isapprox(full(Ucpps), single([3 - 1i 4; 0 1 + 2i]), 1e-6);
asserts.isapprox(full(Pcpps), [0 1; 1 0], 1e-12);
asserts.isapprox(full(Pcpps * Cps - Lcpps * Ucpps), single(zeros(2, 2)), 1e-5);

T = sparse([0.2 1; 1 1]);
optt.type = 'ilutp';
optt.droptol = 0;
optt.thresh = 0;
[Ltn, Utn, Ptn] = ilu(T, optt);
asserts.isapprox(full(Ltn), [1 0; 5 1], 1e-12);
asserts.isapprox(full(Utn), [0.2 1; 0 -4], 1e-12);
asserts.isapprox(full(Ptn), eye(2, 2), 1e-12);
asserts.isapprox(full(Ptn * T - Ltn * Utn), zeros(2, 2), 1e-12);
optt.thresh = 0.25;
[Ltp, Utp, Ptp] = ilu(T, optt);
asserts.isapprox(full(Ltp), [1 0; 0.2 1], 1e-12);
asserts.isapprox(full(Utp), [1 1; 0 0.8], 1e-12);
asserts.isapprox(full(Ptp), [0 1; 1 0], 1e-12);
asserts.isapprox(full(Ptp * T - Ltp * Utp), zeros(2, 2), 1e-12);

optp.udiag = true;
[Lzp, Uzp] = ilu(sparse(zeros(2, 2)), optp);
asserts.isapprox(full(Lzp), eye(2, 2), 1e-12);
asserts.isequal(nnz(Uzp), 0);
%=============================================================================
asserts.checkerror('ilu(A, ones(1, 2))', _('Options must be a scalar structure.'));
asserts.checkerror('ilu(sparse([1 2]))', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('ilu(A, struct(''type'', ''bad''))', sprintf(_('Unsupported %s.'), 'incomplete LU type'));
asserts.checkerror('ilu(A, struct(''type'', 1))', _('Option value must be a character row vector or a string scalar.'));
asserts.checkerror('ilu(A, struct(''droptol'', Inf))', sprintf(_('%s must be a finite real scalar.'), 'Option value'));
asserts.checkerror('ilu(A, struct(''droptol'', -1))', sprintf(_('%s must be a nonnegative scalar.'), 'droptol'));
asserts.checkerror('ilu(A, struct(''fillfactor'', 0))', sprintf(_('%s must be a positive integer scalar.'), 'fillfactor'));
asserts.checkerror('ilu(A, struct(''fillfactor'', 1.5))', sprintf(_('%s must be a positive integer scalar.'), 'fillfactor'));
asserts.checkerror('ilu(A, struct(''udiag'', 1))', _('udiag must be a logical scalar.'));
asserts.checkerror('ilu(A, struct(''thresh'', -1))', _('thresh must be a scalar between 0 and 1.'));
asserts.checkerror('ilu(A, struct(''thresh'', 2))', _('thresh must be a scalar between 0 and 1.'));
asserts.checkerror('ilu(A, struct(''unknown'', 1))', sprintf(_('Unsupported %s.'), 'incomplete LU option'));
asserts.checkerror('ilu(sparse([0 1; 1 1]))', sprintf(_('%s failed.'), 'Incomplete LU factorization'));
asserts.checkerror('ilu(sparse([0 1; 1 1]), struct(''type'', ''nofill'', ''udiag'', true))', sprintf(_('%s failed.'), 'Incomplete LU factorization'));
asserts.checkerror('ilu(sparse(zeros(2, 2)), struct(''type'', ''ilutp''))', sprintf(_('%s failed.'), 'Incomplete LU factorization'));
%=============================================================================
