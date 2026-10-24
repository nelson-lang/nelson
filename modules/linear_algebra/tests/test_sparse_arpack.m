%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
A = sparse([4 1 0; 1 3 0; 0 0 2]);
try
  d = eigs(A, 2);
catch M
  if strcmp(M.message, _('ARPACK support is not available.'))
    return
  end
  error(M.message);
end
%=============================================================================
REF = [4.618033988749895; 2.381966011250105];
asserts.isapprox(d, REF, 1e-12);
%=============================================================================
[V, D] = eigs(A, 2);
asserts.isapprox(diag(D), REF, 1e-12);
asserts.isapprox(A * V, V * D, 1e-12);
asserts.isequal(issparse(V), false);
asserts.isequal(issparse(D), false);
%=============================================================================
s = svds(A, 2);
asserts.isapprox(s, REF, 1e-12);
%=============================================================================
[U, S, W] = svds(A, 2);
asserts.isapprox(diag(S), REF, 1e-12);
asserts.isapprox(A * W, U * S, 1e-12);
asserts.isequal(issparse(U), false);
asserts.isequal(issparse(S), false);
asserts.isequal(issparse(W), false);
%=============================================================================
AS = sparse(single([4 1 0; 1 3 0; 0 0 2]));
ds = eigs(AS, 2);
asserts.isequal(class(ds), 'single');
asserts.isapprox(ds, single(REF), 1e-5);
[VS, DS] = eigs(AS, 2);
asserts.isequal(class(VS), 'single');
asserts.isequal(class(DS), 'single');
asserts.isapprox(diag(DS), single(REF), 1e-5);
asserts.isapprox(AS * VS, VS * DS, 1e-5);
ss = svds(AS, 2);
asserts.isequal(class(ss), 'single');
asserts.isapprox(ss, single(REF), 1e-5);
[US, SS, WS] = svds(AS, 2);
asserts.isequal(class(US), 'single');
asserts.isequal(class(SS), 'single');
asserts.isequal(class(WS), 'single');
asserts.isapprox(diag(SS), single(REF), 1e-5);
asserts.isapprox(AS * WS, US * SS, 1e-5);
%=============================================================================
T = sparse([1 0 0; 0 2 0; 3 0 0; 0 4 0; 0 0 5]);
REFS = [5; sqrt(20)];
s = svds(T, 2);
asserts.isapprox(s, REFS, 1e-12);
asserts.isapprox(svds(T, 2, 'largest'), REFS, 1e-12);
asserts.isapprox(svds(T, 2, 'lm'), REFS, 1e-12);
[U, S, W] = svds(T, 2);
asserts.isapprox(diag(S), REFS, 1e-12);
asserts.istrue(norm(T * W - U * S) < 1e-12);
asserts.isequal(issparse(U), false);
asserts.isequal(issparse(S), false);
asserts.isequal(issparse(W), false);
REFS_SM = [sqrt(20); sqrt(10)];
s = svds(T, 2, 'smallest');
asserts.isapprox(s, REFS_SM, 1e-12);
asserts.isapprox(svds(T, 2, 'sm'), REFS_SM, 1e-12);
[U, S, W] = svds(T, 2, 'smallest');
asserts.isapprox(diag(S), REFS_SM, 1e-12);
asserts.istrue(norm(T * W - U * S) < 1e-12);
asserts.isequal(issparse(U), false);
asserts.isequal(issparse(S), false);
asserts.isequal(issparse(W), false);
%=============================================================================
WIDE = sparse([1 0 3 0 0; 0 2 0 4 0; 0 0 0 0 5]);
s = svds(WIDE, 2);
asserts.isapprox(s, REFS, 1e-12);
asserts.isapprox(svds(WIDE, 2, 'largest'), REFS, 1e-12);
asserts.isapprox(svds(WIDE, 2, 'lm'), REFS, 1e-12);
[U, S, W] = svds(WIDE, 2);
asserts.isapprox(diag(S), REFS, 1e-12);
asserts.istrue(norm(WIDE * W - U * S) < 1e-12);
asserts.isequal(issparse(U), false);
asserts.isequal(issparse(S), false);
asserts.isequal(issparse(W), false);
s = svds(WIDE, 2, 'smallest');
asserts.isapprox(s, REFS_SM, 1e-12);
asserts.isapprox(svds(WIDE, 2, 'sm'), REFS_SM, 1e-12);
[U, S, W] = svds(WIDE, 2, 'smallest');
asserts.isapprox(diag(S), REFS_SM, 1e-12);
asserts.istrue(norm(WIDE * W - U * S) < 1e-12);
asserts.isequal(issparse(U), false);
asserts.isequal(issparse(S), false);
asserts.isequal(issparse(W), false);
%=============================================================================
S = sparse(diag([5 3 -2 -4]));
assertValuesMatch(eigs(S, 2, 'lm'), [5; -4], 1e-12);
assertValuesMatch(eigs(S, 2, 'sm'), [-2; 3], 1e-12);
assertValuesMatch(eigs(S, 2, 'lr'), [5; 3], 1e-12);
assertValuesMatch(eigs(S, 2, 'sr'), [-4; -2], 1e-12);
assertValuesMatch(eigs(S, 2, 'la'), [5; 3], 1e-12);
assertValuesMatch(eigs(S, 2, 'sa'), [-4; -2], 1e-12);
assertValuesMatch(eigs(S, 2, 'li'), [5; -4], 1e-12);
assertValuesMatch(eigs(S, 2, 'si'), [5; -4], 1e-12);
SHIFT = sparse(diag([1 2 4 8 16]));
lambda = eigs(SHIFT, 2, 3.5);
assertValuesMatch(lambda, [4; 2], 1e-12);
lambda = eigs(SHIFT, 2, 7);
assertValuesMatch(lambda, [8; 4], 1e-12);
[V, D] = eigs(SHIFT, 2, 3.5);
lambda = diag(D);
assertValuesMatch(lambda, [4; 2], 1e-12);
asserts.istrue(norm(SHIFT * V - V * D) < 1e-12);
%=============================================================================
B = sparse([4 1 0 0; 0 3 1 0; 0 0 2 1; 0 0 0 1]);
d = eigs(B, 2);
asserts.isapprox(d, [4; 3], 1e-12);
[V, D] = eigs(B, 2);
asserts.isapprox(diag(D), [4; 3], 1e-12);
asserts.isapprox(B * V, V * D, 1e-12);
asserts.isequal(issparse(V), false);
asserts.isequal(issparse(D), false);
asserts.isapprox(eigs(B, 2, 'sm'), [1; 2], 1e-12);
asserts.isapprox(eigs(B, 2, 'lr'), [4; 3], 1e-12);
asserts.isapprox(eigs(B, 2, 'sr'), [1; 2], 1e-12);
asserts.isapprox(eigs(B, 2, 2.6), [3; 2], 1e-12);
[V, D] = eigs(B, 2, 2.6);
asserts.isapprox(diag(D), [3; 2], 1e-12);
asserts.istrue(norm(B * V - V * D) < 1e-12);
%=============================================================================
C = sparse([0 -1 0 0; 1 0 0 0; 0 0 0.5 0; 0 0 0 0.25]);
d = eigs(C, 2);
asserts.istrue(all(abs(d - [1i; -1i]) < 1e-12));
[V, D] = eigs(C, 2);
asserts.istrue(all(abs(diag(D) - [1i; -1i]) < 1e-12));
asserts.istrue(norm(C * V - V * D) < 1e-12);
asserts.isequal(issparse(V), false);
asserts.isequal(issparse(D), false);
asserts.istrue(all(abs(eigs(C, 2, 'li') - [1i; -1i]) < 1e-12));
asserts.istrue(all(abs(eigs(C, 2, 'si') - [0.5; 0.25]) < 1e-12));
%=============================================================================
CS = sparse(diag([1 + 1i, 2 - 1i, 4 + 2i, -3 + 5i]));
d = eigs(CS, 2);
asserts.istrue(all(abs(d - [-3 + 5i; 4 + 2i]) < 1e-10));
[V, D] = eigs(CS, 2);
asserts.istrue(all(abs(diag(D) - [-3 + 5i; 4 + 2i]) < 1e-10));
asserts.istrue(norm(CS * V - V * D) < 1e-10);
asserts.istrue(all(abs(eigs(CS, 2, 'lm') - [-3 + 5i; 4 + 2i]) < 1e-10));
asserts.istrue(all(abs(eigs(CS, 2, 'sm') - [1 + 1i; 2 - 1i]) < 1e-10));
asserts.istrue(all(abs(eigs(CS, 2, 'lr') - [4 + 2i; 2 - 1i]) < 1e-10));
asserts.istrue(all(abs(eigs(CS, 2, 'sr') - [-3 + 5i; 1 + 1i]) < 1e-10));
asserts.istrue(all(abs(eigs(CS, 2, 'li') - [-3 + 5i; 4 + 2i]) < 1e-10));
asserts.istrue(all(abs(eigs(CS, 2, 'si') - [2 - 1i; 1 + 1i]) < 1e-10));
d = eigs(CS, 2, 2);
asserts.istrue(all(abs(d - [2 - 1i; 1 + 1i]) < 1e-10));
d = eigs(CS, 2, 2 + 0.5i);
asserts.istrue(all(abs(d - [1 + 1i; 2 - 1i]) < 1e-10));
[V, D] = eigs(CS, 2, 2 + 0.5i);
asserts.istrue(all(abs(diag(D) - [1 + 1i; 2 - 1i]) < 1e-10));
asserts.istrue(norm(CS * V - V * D) < 1e-10);
%=============================================================================
CSS = sparse(single(diag([1 + 1i, 2 - 1i, 4 + 2i, -3 + 5i])));
dcs = eigs(CSS, 2);
asserts.isequal(class(dcs), 'single');
asserts.isfalse(isreal(dcs));
asserts.istrue(all(abs(dcs - single([-3 + 5i; 4 + 2i])) < 1e-5));
[VCS, DCS] = eigs(CSS, 2);
asserts.isequal(class(VCS), 'single');
asserts.isequal(class(DCS), 'single');
asserts.isfalse(isreal(DCS));
asserts.istrue(all(abs(diag(DCS) - single([-3 + 5i; 4 + 2i])) < 1e-5));
asserts.istrue(norm(CSS * VCS - VCS * DCS) < 1e-5);
%=============================================================================
TC = sparse([1 + 1i 0 0; 0 2i 0; 3 0 0; 0 4 0; 0 0 5i]);
REFSC = [5; sqrt(20)];
s = svds(TC, 2);
asserts.isapprox(s, REFSC, 1e-10);
[U, S, W] = svds(TC, 2);
asserts.isapprox(diag(S), REFSC, 1e-10);
asserts.istrue(norm(TC * W - U * S) < 1e-10);
asserts.isequal(issparse(U), false);
asserts.isequal(issparse(S), false);
asserts.isequal(issparse(W), false);
REFSC_SM = [sqrt(20); sqrt(11)];
s = svds(TC, 2, 'smallest');
asserts.isapprox(s, REFSC_SM, 1e-10);
[U, S, W] = svds(TC, 2, 'smallest');
asserts.isapprox(diag(S), REFSC_SM, 1e-10);
asserts.istrue(norm(TC * W - U * S) < 1e-10);
WC = sparse([1 + 1i 0 3 0 0; 0 2i 0 4 0; 0 0 0 0 5i]);
s = svds(WC, 2);
asserts.isapprox(s, REFSC, 1e-10);
[U, S, W] = svds(WC, 2);
asserts.isapprox(diag(S), REFSC, 1e-10);
asserts.istrue(norm(WC * W - U * S) < 1e-10);
QC = sparse([1 + 2i 0 0 0; 0 3 - 1i 0 0; 0 0 -4i 0; 0 0 0 5 + 1i]);
asserts.isapprox(svds(QC, 2), [sqrt(26); 4], 1e-10);
asserts.isapprox(svds(QC, 2, 'smallest'), [sqrt(10); sqrt(5)], 1e-10);
%=============================================================================
TCS = sparse(single([1 + 1i 0 0; 0 2i 0; 3 0 0; 0 4 0; 0 0 5i]));
scs = svds(TCS, 2);
asserts.isequal(class(scs), 'single');
asserts.isapprox(scs, single(REFSC), 1e-5);
[UCS, SCS, WCS] = svds(TCS, 2);
asserts.isequal(class(UCS), 'single');
asserts.isequal(class(SCS), 'single');
asserts.isequal(class(WCS), 'single');
asserts.isapprox(diag(SCS), single(REFSC), 1e-5);
asserts.istrue(norm(TCS * WCS - UCS * SCS) < 1e-5);
%=============================================================================
asserts.checkerror('eigs(sparse(eye(3)), 1, ''bad'')', sprintf(_('Unsupported %s.'), 'WHICH value'));
asserts.checkerror('eigs(sparse(eye(3)), 1, 1 + 1i)', sprintf(_('%s must be a finite real scalar.'), 'SIGMA'));
try
  x = eigs(sparse(eye(3)), 1, 1);
  asserts.isapprox(x, 1, 1e-12);
catch M
  asserts.isequal(M.message, sprintf(_('%s failed.'), 'Sparse linear equation solver'));
end
asserts.checkerror('eigs(sparse(diag([1 + NaN * 1i, 2])), 1)', sprintf(_('%s must not contain NaN or Inf.'), 'Input argument'));
try
  x = eigs(sparse(diag([1 + 1i, 2])), 1, 1 + 1i);
  asserts.isapprox(x, 1 + 1i, 1e-12);
catch M
  asserts.isequal(M.message, sprintf(_('%s failed.'), 'Sparse linear equation solver'));
end
asserts.checkerror('svds(sparse(eye(3)), 1, ''bad'')', sprintf(_('Unsupported %s.'), 'WHICH value'));
asserts.checkerror('eigs(sparse(eye(2)), 2)', _('K must be a positive integer smaller than the matrix dimension.'));
%=============================================================================
BIG_N = 513;
BIG = sparse(1:BIG_N, 1:BIG_N, 1:BIG_N, BIG_N, BIG_N);
try
  d = eigs(BIG, 1);
  asserts.isapprox(d, BIG_N, 1e-8);
catch M
  asserts.isequal(M.message, _('ARPACK support is not available.'));
end
try
  s = svds(BIG, 1);
  asserts.isapprox(s, BIG_N, 1e-8);
catch M
  asserts.isequal(M.message, _('ARPACK support is not available.'));
end
%=============================================================================
function assertValuesMatch(actual, expected, tolerance)
  actual = actual(:);
  expected = expected(:);
  asserts.isequal(numel(actual), numel(expected));
  for k = 1:numel(actual)
    [distance, idx] = min(abs(expected - actual(k)));
    asserts.istrue(distance <= tolerance);
    expected(idx) = [];
  end
end
%=============================================================================
