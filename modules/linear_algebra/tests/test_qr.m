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
asserts.isequal(nargin('qr'), 3);
asserts.isequal(nargout('qr'), 3);
%=============================================================================
A = reshape(1:6, 3, 2);
R = qr(A);
asserts.isequal(size(R), [3, 2]);
asserts.istrue(norm(tril(R, -1)) < 1e-12);
[Q, R] = qr(A);
asserts.isequal(size(Q), [3, 3]);
asserts.isequal(size(R), [3, 2]);
asserts.isapprox(A, Q * R, 1e-12);
asserts.isapprox(Q' * Q, eye(3), 1e-12);
%=============================================================================
[Qe, Re] = qr(A, 'econ');
asserts.isequal(size(Qe), [3, 2]);
asserts.isequal(size(Re), [2, 2]);
asserts.isapprox(A, Qe * Re, 1e-12);
[Q0, R0, p0] = qr(A, 0);
asserts.isequal(size(Q0), [3, 2]);
asserts.isequal(size(R0), [2, 2]);
asserts.isequal(size(p0), [1, 2]);
asserts.isapprox(A(:, p0), Q0 * R0, 1e-12);
%=============================================================================
[Qp, Rp, P] = qr(A);
asserts.isequal(class(P), 'double');
asserts.isequal(size(P), [2, 2]);
asserts.isapprox(A * P, Qp * Rp, 1e-12);
[Qv, Rv, p] = qr(A, 'vector');
asserts.isequal(class(p), 'double');
asserts.isequal(size(p), [1, 2]);
asserts.isapprox(A(:, p), Qv * Rv, 1e-12);
[Qm, Rm, Pm] = qr(A, 'econ', 'matrix');
asserts.isequal(size(Qm), [3, 2]);
asserts.isequal(size(Rm), [2, 2]);
asserts.isapprox(A * Pm, Qm * Rm, 1e-12);
[QsOpt, RsOpt, psOpt] = qr(A, string('econ'), string('vector'));
asserts.isequal(size(QsOpt), [3, 2]);
asserts.isequal(size(RsOpt), [2, 2]);
asserts.isapprox(A(:, psOpt), QsOpt * RsOpt, 1e-12);
%=============================================================================
% Scalar and vector edge cases: qr of a scalar is Q=1, R=x, P=1.
[Qscalar, Rscalar, Pscalar] = qr(5);
asserts.isequal(Qscalar, 1);
asserts.isequal(Rscalar, 5);
asserts.isequal(Pscalar, 1);
[Qnan, Rnan] = qr(NaN);
asserts.isequal(Qnan, 1);
asserts.istrue(isnan(Rnan));
[Qinf, Rinf] = qr(Inf);
asserts.isequal(Qinf, 1);
asserts.isequal(Rinf, Inf);
columnVector = [1; 2; 3];
[Qc, Rc] = qr(columnVector);
asserts.isequal(size(Qc), [3, 3]);
asserts.isequal(size(Rc), [3, 1]);
asserts.isapprox(columnVector, Qc * Rc, 1e-12);
[Qce, Rce, pce] = qr(columnVector, 0);
asserts.isequal(size(Qce), [3, 1]);
asserts.isequal(size(Rce), [1, 1]);
asserts.isequal(pce, 1);
asserts.isapprox(columnVector(:, pce), Qce * Rce, 1e-12);
rowVector = [1 2 3];
[Qr, Rr, pr] = qr(rowVector, 'vector');
asserts.isequal(size(Qr), [1, 1]);
asserts.isequal(size(Rr), [1, 3]);
asserts.isequal(pr, [3 2 1]);
asserts.isapprox(rowVector(:, pr), Qr * Rr, 1e-12);
%=============================================================================
rankDeficient = [1 2; 2 4; 3 6];
[Qrd, Rrd, prd] = qr(rankDeficient, 'vector');
asserts.isequal(prd, [2 1]);
asserts.istrue(abs(Rrd(2, 2)) < 1e-12);
asserts.isapprox(rankDeficient(:, prd), Qrd * Rrd, 1e-12);
zeroMatrix = zeros(3, 2);
[Qz, Rz, pz] = qr(zeroMatrix, 'vector');
asserts.isequal(pz, [1 2]);
asserts.isequal(size(Qz), [3, 3]);
asserts.isequal(size(Rz), [3, 2]);
asserts.isequal(Qz, eye(3));
asserts.isequal(Rz, zeroMatrix);
%=============================================================================
W = reshape(1:6, 2, 3);
[Qw, Rw] = qr(W, 'econ');
asserts.isequal(size(Qw), [2, 2]);
asserts.isequal(size(Rw), [2, 3]);
asserts.isapprox(W, Qw * Rw, 1e-12);
%=============================================================================
[Qempty, Rempty] = qr(zeros(0, 3));
asserts.isequal(size(Qempty), [0, 0]);
asserts.isequal(size(Rempty), [0, 3]);
[Qempty2, Rempty2] = qr(zeros(3, 0));
asserts.isequal(size(Qempty2), [3, 3]);
asserts.isequal(size(Rempty2), [3, 0]);
%=============================================================================
AS = single(A);
[QS, RS, PS] = qr(AS);
asserts.isequal(class(QS), 'single');
asserts.isequal(class(RS), 'single');
asserts.isequal(class(PS), 'single');
asserts.isapprox(AS * PS, QS * RS, single(1e-5));
[~, ~, pS] = qr(AS, 'vector');
asserts.isequal(class(pS), 'double');
%=============================================================================
AC = A + 1i;
[QC, RC, PC] = qr(AC);
asserts.isequal(class(QC), 'double');
asserts.isequal(class(RC), 'double');
asserts.isequal(class(PC), 'double');
asserts.isapprox(AC * PC, QC * RC, 1e-12);
ACS = single(AC);
[QCS, RCS, PCS] = qr(ACS);
asserts.isequal(class(QCS), 'single');
asserts.isequal(class(RCS), 'single');
asserts.isequal(class(PCS), 'single');
asserts.isapprox(ACS * PCS, QCS * RCS, single(1e-5));
%=============================================================================
S = sparse([1 0; 2 3; 0 4]);
[SQ, SR] = qr(S);
asserts.istrue(issparse(SQ));
asserts.istrue(issparse(SR));
asserts.istrue(norm(full(S - SQ * SR)) < 1e-12);
[SQp, SRp, SP] = qr(S);
asserts.istrue(issparse(SP));
asserts.istrue(norm(full(S * SP - SQp * SRp)) < 1e-12);
[SQv, SRv, sp] = qr(S, 'vector');
asserts.isequal(class(sp), 'double');
asserts.istrue(norm(full(S(:, sp) - SQv * SRv)) < 1e-12);
SW = sparse([1 0 2; 0 3 0]);
[SQw, SRw, spw] = qr(SW, 'vector');
asserts.istrue(issparse(SQw));
asserts.istrue(issparse(SRw));
asserts.isequal(size(SQw), [2, 2]);
asserts.isequal(size(SRw), [2, 3]);
asserts.isequal(size(spw), [1, 3]);
asserts.istrue(norm(full(SW(:, spw) - SQw * SRw)) < 1e-12);
%=============================================================================
B = [1; 2; 3];
[C, Rls] = qr(S, B, 'econ');
asserts.isfalse(issparse(C));
asserts.istrue(issparse(Rls));
asserts.isequal(size(C), [2, 1]);
asserts.isequal(size(Rls), [2, 2]);
x = Rls \ C;
asserts.isapprox(norm(full(S * x - B)), 0.953998092, 1e-8);
[Cv, Rvls, pvls] = qr(S, B, 'vector');
xv = zeros(size(S, 2), size(B, 2));
xv(pvls, :) = Rvls \ Cv;
asserts.isapprox(norm(full(S * xv - B)), 0.953998092, 1e-8);
Bs = sparse(B);
[Cs, Rsp] = qr(S, Bs, 'econ');
asserts.istrue(issparse(Cs));
asserts.istrue(issparse(Rsp));
Bmulti = [1 2; 3 4; 5 6];
[Cm, Rm] = qr(S, Bmulti, 'econ');
asserts.isequal(size(Cm), [2, 2]);
asserts.isequal(size(Rm), [2, 2]);
xm = Rm \ Cm;
asserts.isapprox(norm(full(S * xm - Bmulti)), 2.23606797749979, 1e-12);
%=============================================================================
SS = sparse(single(full(S)));
BS = single(B);
[QSS, RSS, PSS] = qr(SS);
asserts.istrue(issparse(QSS));
asserts.istrue(issparse(RSS));
asserts.istrue(issparse(PSS));
asserts.isequal(class(QSS), 'single');
asserts.isequal(class(RSS), 'single');
asserts.isequal(class(PSS), 'single');
asserts.istrue(norm(full(SS * PSS - QSS * RSS)) < single(1e-5));
[CSS, RSS2] = qr(SS, BS, 'econ');
asserts.isequal(class(CSS), 'single');
asserts.isequal(class(RSS2), 'single');
%=============================================================================
SC = sparse(full(S) + 1i * full(S));
[QSC, RSC, PSC] = qr(SC);
asserts.istrue(issparse(QSC));
asserts.istrue(issparse(RSC));
asserts.istrue(issparse(PSC));
asserts.isequal(class(PSC), 'double');
asserts.istrue(norm(full(SC * PSC - QSC * RSC)) < 1e-12);
SCS = sparse(single(full(S) + 1i * full(S)));
[QSCS, RSCS, PSCS] = qr(SCS);
asserts.istrue(issparse(QSCS));
asserts.istrue(issparse(RSCS));
asserts.istrue(issparse(PSCS));
asserts.isequal(class(PSCS), 'single');
asserts.istrue(norm(full(SCS * PSCS - QSCS * RSCS)) < single(1e-5));
%=============================================================================
asserts.checkerror('qr([1 2; 3 4], ''bad'')', _('Option must be ''econ'', ''matrix'', or ''vector''.'));
asserts.checkerror('qr([1 2; 3 4], 1)', _('Right-hand side matrix is only supported if the first input matrix is sparse.'));
asserts.checkerror('qr([1 2; 3 4], ''econ'', 0)', _('Option 0 must not be combined with other options. Use ''econ'' instead.'));
asserts.checkerror('qr(ones(2, 2, 2))', sprintf(_('%s expected.'), 'Vector or matrix 2D'));
%=============================================================================
