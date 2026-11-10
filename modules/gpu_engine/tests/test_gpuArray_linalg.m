%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
A = single([4 -2 1; 3 6 -4; 2 1 8]);
B = single([12 1; -25 2; 32 3]);
G = gpuArray(A);
%=============================================================================
% square solve, mrdivide and linsolve stay on the device
X = G \ gpuArray(B);
assert_istrue(isgpuarray(X));
assert_isapprox(gather(X), A \ B, 1e-5);
assert_isapprox(gather(gpuArray(B') / G), B' / A, 1e-5);
assert_isapprox(gather(linsolve(G, gpuArray(B))), A \ B, 1e-5);
assert_isapprox(gather(A \ gpuArray(B)), A \ B, 1e-5);
assert_isapprox(gather(gpuArray(single(2)) \ gpuArray(single(6))), single(3));
%=============================================================================
% det and inv
d = det(G);
assert_istrue(isgpuarray(d));
assert_isapprox(gather(d), det(A), 1e-5);
assert_isapprox(gather(det(gpuArray(single([0 1; 1 0])))), single(-1));
assert_isapprox(gather(inv(G)), inv(A), 1e-5);
assert_isapprox(gather(inv(gpuArray(single(4)))), single(0.25));
%=============================================================================
% lu forms
[L, U, P] = lu(G);
assert_istrue(isgpuarray(L) && isgpuarray(U) && isgpuarray(P));
[l, u, p] = lu(A);
assert_isapprox(gather(L), l, 1e-6);
assert_isapprox(gather(U), u, 1e-5);
assert_isequal(gather(P), single(p));
[L2, U2] = lu(G);
assert_isapprox(gather(L2 * U2), A, 1e-5);
[~, ~, v] = lu(G, 'vector');
[~, ~, pv] = lu(A, 'vector');
assert_isequal(gather(v), single(pv));
assert_isapprox(gather(lu(G)), lu(A), 1e-5);
% rectangular
R = single([1 2 3; 4 5 6; 7 8 10; 1 0 1; 2 2 2]);
[L, U, P] = lu(gpuArray(R));
assert_isequal(size(L), [5 3]);
assert_isequal(size(U), [3 3]);
assert_isapprox(gather(L * U), gather(P) * R, 1e-5);
[L, U] = lu(gpuArray(R'));
assert_isequal(size(L), [3 3]);
assert_isequal(size(U), [3 5]);
assert_isapprox(gather(L * U), R', 1e-5);
%=============================================================================
% larger system
rng(3);
M = single(rand(300) + 300 * eye(300));
b = single(rand(300, 2));
assert_isapprox(gather(gpuArray(M) \ gpuArray(b)), M \ b, 1e-4);
[L, U, P] = lu(gpuArray(M));
assert_isapprox(gather(L * U), gather(P) * M, 1e-4);
%=============================================================================
% a singular matrix keeps the host answer
assert_isequal(gather(det(gpuArray(single([1 2; 2 4])))), single(0));
%=============================================================================
% Cholesky from the upper (or lower) triangle
S = single([4 1 2; 1 3 0; 2 0 5]);
R = chol(gpuArray(S));
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), chol(S), 1e-6);
assert_isapprox(gather(chol(gpuArray(S), 'lower')), chol(S, 'lower'), 1e-6);
T = S;
T(3, 1) = 99;
assert_isapprox(gather(chol(gpuArray(T))), chol(S), 1e-6);
rng(4);
B = single(rand(200));
M = B' * B + 200 * eye(200, 'single');
assert_isapprox(gather(chol(gpuArray(M))), chol(M), 1e-5);
% not positive definite: the host error and partial form
assert_checkerror('chol(gpuArray(single([1 2; 2 1])))', _('Positive finite matrix expected.'));
[~, p] = chol(gpuArray(single([1 2; 2 1])));
assert_isequal(double(gather(p)), 2);
%=============================================================================
% Householder qr, same signs as the host
rng(5);
for sz = {[3 2], [2 3], [5 5], [40 12]}
  A = single(rand(sz{1}) - 0.5);
  G = gpuArray(A);
  [Q, R] = qr(G);
  [q, r] = qr(A);
  assert_istrue(isgpuarray(Q) && isgpuarray(R));
  assert_isequal(size(Q), size(q));
  assert_isapprox(gather(Q), q, 1e-5);
  assert_isapprox(gather(R), r, 1e-5);
  [Qe, Re] = qr(G, 0);
  [qe, re] = qr(A, 0);
  assert_isequal(size(Re), size(re));
  assert_isapprox(gather(Qe), qe, 1e-5);
  assert_isapprox(gather(qr(G, 'econ')), qr(A, 'econ'), 1e-5);
  assert_isapprox(gather(qr(G)), qr(A), 1e-5);
end
[Q, R] = qr(gpuArray(single(zeros(3, 2))));
assert_isequal(gather(Q), single(eye(3)));
% the pivoted form keeps the host answer
[Q, R, P] = qr(gpuArray(single(magic(3))));
[q, r, pp] = qr(single(magic(3)));
assert_isequal(gather(P), single(pp));
%=============================================================================
% page functions
rng(7);
X = single(rand(3, 4, 2, 2));
Y = single(rand(4, 2, 2, 2));
Y1 = single(rand(4, 5));
P = pagemtimes(gpuArray(X), gpuArray(Y));
assert_istrue(isgpuarray(P));
assert_isapprox(gather(P), pagemtimes(X, Y), 1e-6);
assert_isapprox(gather(pagemtimes(gpuArray(X), gpuArray(Y1))), pagemtimes(X, Y1), 1e-6);
X2 = single(rand(3, 4));
assert_isapprox(gather(pagemtimes(gpuArray(X2), gpuArray(Y))), pagemtimes(X2, Y), 1e-6);
T = pagetranspose(gpuArray(X));
assert_istrue(isgpuarray(T));
assert_isequal(gather(T), pagetranspose(X));
assert_isequal(size(pagemtimes(gpuArray(X), 'transpose', gpuArray(single(rand(3, 2, 2, 2))), 'none')), [4 2 2 2]);
%=============================================================================
% matrix powers and exponential
rng(9);
A = single(rand(5) / 3);
G = gpuArray(A);
for p = [0 1 2 7 -1 -3]
  R = G ^ p;
  assert_istrue(isgpuarray(R));
  assert_isapprox(gather(R), A ^ p, 1e-5);
end
assert_isapprox(gather(G ^ 0.5), A ^ 0.5, 1e-5);
for scale = [0.01 1 20]
  M = single(rand(6) - 0.5) * scale;
  E = expm(gpuArray(M));
  assert_istrue(isgpuarray(E));
  assert_isapprox(gather(E), expm(M), 1e-5);
end
assert_isequal(gather(expm(gpuArray(single(zeros(3))))), single(eye(3)));
%=============================================================================
% matrix square root and logarithm
rng(10);
B = single(rand(5));
S = B * B' + single(eye(5));
R = sqrtm(gpuArray(S));
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), sqrtm(S), 1e-5);
L = logm(gpuArray(S));
assert_istrue(isgpuarray(L));
assert_isapprox(gather(L), logm(S), 1e-4);
assert_isapprox(gather(logm(gpuArray(single([4 1; 0 9])))), logm(single([4 1; 0 9])), 1e-5);
% no real root: the host answer
N = single([-1 0; 0 4]);
assert_isapprox(gather(sqrtm(gpuArray(N))), sqrtm(N), 1e-6);
assert_isfalse(isreal(gather(logm(gpuArray(N)))));
%=============================================================================
% singular value decomposition (one-sided Jacobi)
rng(13);
for sz = {[4 4], [6 3], [3 6], [40 20]}
  A = single(rand(sz{1}) - 0.5);
  G = gpuArray(A);
  s = svd(G);
  h = svd(A);
  assert_istrue(isgpuarray(s));
  assert_isapprox(gather(s), h, 1e-5);
  [U, S, V] = svd(G, 'econ');
  [u, ss, vv] = svd(A, 'econ');
  assert_istrue(isgpuarray(U) && isgpuarray(S) && isgpuarray(V));
  assert_isequal(size(U), size(u));
  assert_isequal(size(S), size(ss));
  assert_isequal(size(V), size(vv));
  assert_isapprox(gather(U * S * V'), A, 1e-5);
  assert_isapprox(gather(U' * U), single(eye(size(U, 2))), 1e-5);
  assert_isapprox(gather(V' * V), single(eye(size(V, 2))), 1e-5);
end
[U, S, V] = svd(gpuArray(single(magic(4))));
assert_isapprox(gather(U * S * V'), single(magic(4)), 1e-4);
[U, S, V] = svd(gpuArray(single(rand(5, 3))));
assert_isequal(size(U), [5 5]);
assert_isequal(double(gather(rank(gpuArray(single(magic(4)))))), 3);
assert_isapprox(gather(pinv(gpuArray(single([1 2; 3 4; 5 6])))), pinv(single([1 2; 3 4; 5 6])), 1e-5);
%=============================================================================
% eigenvalues of symmetric matrices from the Jacobi sweeps
rng(14);
for n = [2 5 30]
  B = single(rand(n) - 0.5);
  A = (B + B') / 2;
  G = gpuArray(A);
  e = eig(G);
  assert_istrue(isgpuarray(e));
  assert_isapprox(gather(e), eig(A), 1e-5);
  [V, D] = eig(G);
  assert_isapprox(gather(G * V), gather(V * D), 1e-4);
  assert_isapprox(gather(V' * V), single(eye(n)), 1e-5);
end
assert_isequal(gather(eig(gpuArray(single([2 0; 0 -2])))), single([-2; 2]));
% non symmetric matrices keep the host answer
assert_isapprox(gather(eig(gpuArray(single([1 2; 3 4])))), eig(single([1 2; 3 4])), 1e-6);
%=============================================================================
% page inverses and binary functions with expansion
rng(3);
P = single(rand(3, 3, 2, 2)) + single(3 * eye(3));
X = pageinv(gpuArray(P));
assert_istrue(isgpuarray(X));
assert_isapprox(gather(X), pageinv(P), 1e-5);
assert_checkerror('pageinv(gpuArray(single(rand(2, 3))))', _('Each page of the input array must be a square matrix.'));
C = bsxfun(@plus, gpuArray(single([1 2])), gpuArray(single([10; 20])));
assert_istrue(isgpuarray(C));
assert_isequal(gather(C), single([11 12; 21 22]));
assert_isequal(gather(bsxfun(@(a, b) a - b, gpuArray(single([1 2 3])), gpuArray(single([1; 1])))), single([0 1 2; 0 1 2]));
%=============================================================================
