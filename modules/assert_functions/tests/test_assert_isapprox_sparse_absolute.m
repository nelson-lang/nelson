%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
expected = [.03; .03];
computed = expected + [eps(.03); eps(.03)];
for computedSparse = 0:1
  for expectedSparse = 0:1
    a = computed; b = expected;
    if computedSparse, a = sparse(a); end
    if expectedSparse, b = sparse(b); end
    [ok, msg] = assert_isapprox(a, b, 0, 1e-12);
    asserts.istrue(ok); asserts.isequal(msg, '');
    asserts.isapprox(a, b, 0, 1e-12);
  end
end
% Implicit zeros participate at their coordinates, in both argument orders.
tau = 2 ^ -12;
z = sparse(3, 2);
a = sparse([2, 3], [1, 2], [tau, -tau], 3, 2);
asserts.isapprox(a, z, 0, tau);
asserts.isapprox(z, a, 0, tau);
asserts.isapprox(a, zeros(3, 2), 0, tau);
asserts.isapprox(zeros(3, 2), a, 0, tau);
[ok, msg] = assert_isapprox(a, z, 0, tau / 2);
asserts.isfalse(ok); asserts.istrue(contains(msg, '(2, 1)'));
[ok, msg] = assert_isapprox(z, a, 0, tau / 2, 'Sparse mismatch');
asserts.isfalse(ok); asserts.istrue(contains(msg, 'Sparse mismatch'));
asserts.istrue(contains(msg, '(2, 1)'));
% Matching storage patterns are unnecessary; opposite signs must not cancel.
a = sparse([1, 3], [1, 2], [1, tau], 3, 2);
b = sparse([1, 2], [1, 2], [1, -tau], 3, 2);
asserts.isapprox(a, b, 0, tau);
asserts.isfalse(assert_isapprox(a, b, 0, tau / 2));
% Exact boundaries and each complex component follow the dense fallback rule.
b = [1 + 2i, 0; 0, -3 + 4i];
a = b + [tau - 1i * tau, 1i * tau; tau, -tau];
asserts.isapprox(sparse(a), sparse(b), 0, tau);
asserts.isapprox(sparse(a), b, 0, tau);
asserts.isapprox(a, sparse(b), 0, tau);
asserts.isfalse(assert_isapprox(sparse(a), sparse(b), 0, tau / 2));
% Relative and absolute budgets can admit different coordinates.
asserts.isapprox(sparse([1000; tau]), sparse([1001; 0]), .002, tau);
% Non-finite values retain their documented comparison and are never erased.
a = sparse([NaN, 0; Inf, -Inf]); b = a;
asserts.isapprox(a, b, 0, tau);
asserts.istrue(isequaln(a, b));
b(1, 2) = tau;
asserts.isapprox(a, b, 0, tau);
b(2, 2) = Inf;
asserts.isfalse(assert_isapprox(a, b, 0, tau));
b = a; b(1, 1) = 0;
asserts.isfalse(assert_isapprox(a, b, 0, tau));
% Mixed numeric classes use the same elementwise absolute fallback.
asserts.isapprox(sparse([1; 2]), single([1; 2 + tau]), 0, tau);
asserts.isapprox(sparse(single([1 + 2i; 0])), single([1 + tau + (2 - tau) * 1i; tau]), 0, tau);
asserts.isfalse(assert_isapprox(sparse([1; 2]), zeros(1, 2), 0, 10));
asserts.isapprox(sparse(0, 3), zeros(0, 3), 0, tau);
% Two hundred million coordinates with two stored values must stay sparse.
largeRows = 100000000;
a = sparse([1, largeRows], [1, 2], [1, tau], largeRows, 2);
b = sparse(1, 1, 1, largeRows, 2);
asserts.isapprox(a, b, 0, tau);
[ok, msg] = assert_isapprox(a, b, 0, tau / 2);
asserts.isfalse(ok); asserts.istrue(contains(msg, '(100000000, 2)'));
asserts.isapprox(sparse(largeRows, 2), sparse(largeRows, 2), 0, tau);
asserts.isequal(nnz(a), 2); asserts.isequal(nnz(b), 1);
