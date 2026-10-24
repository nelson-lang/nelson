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
% Regression tests for equality-constrained fmincon / quadprog.
% Historically the active-set QP dropped the constraint residual from the KKT
% right-hand side, so an infeasible starting point stayed infeasible, and the
% SQP/interior-point merit line search stalled on equality-heavy problems
% (e.g. collocation NLPs), returning exitflag -2 at the initial point.
%=============================================================================
% quadprog with an INFEASIBLE start point must still reach the equality set.
% min 0.5 (x1^2 + x2^2) s.t. x1 + x2 = 2  ->  x = [1; 1]
H = eye(2);
f = [0; 0];
Aeq = [1 1];
beq = 2;
[x, fval, exitflag] = quadprog(H, f, [], [], Aeq, beq, [], [], [0; 0]);
asserts.isapprox(x(:), [1; 1], 1e-6);
asserts.isapprox(fval, 1, 1e-6);
asserts.istrue(exitflag > 0);
%=============================================================================
% Same problem, feasible and empty starts agree.
[x2, ~, ef2] = quadprog(H, f, [], [], Aeq, beq, [], [], []);
asserts.isapprox(x2(:), [1; 1], 1e-6);
asserts.istrue(ef2 > 0);
%=============================================================================
% Equality-constrained quadratic via fmincon across the general algorithms.
% min (x1-1)^2 + (x2-2)^2 s.t. x1 + x2 = 2  ->  x = [0.5; 1.5], fval = 0.5
obj = @(x)(x(1) - 1) ^ 2 + (x(2) - 2) ^ 2;
algos = {'interior-point', 'sqp', 'active-set'};
for k = 1:numel(algos)
  o = optimoptions('fmincon', 'Display', 'off', 'Algorithm', algos{k});
  [x, fval, exitflag, output] = fmincon(obj, [0; 0], [], [], [1 1], 2, [], [], [], o);
  asserts.isapprox(x(:), [0.5; 1.5], 1e-4);
  asserts.isapprox(fval, 0.5, 1e-4);
  asserts.istrue(exitflag > 0);
  asserts.istrue(output.constrviolation <= 1e-6);
end
%=============================================================================
% Nonlinear EQUALITY constraint (equality-only nonlcon must not size-mismatch).
% min x1 + x2 s.t. x1^2 + x2^2 = 2  ->  x = [-1; -1], fval = -2
nlcon = @(x) deal([], x(1) ^ 2 + x(2) ^ 2 - 2);
for k = 1:2
  o = optimoptions('fmincon', 'Display', 'off', 'Algorithm', algos{k});
  [x, fval, exitflag, output] = fmincon(@(x) x(1) + x(2), [-0.5; -1.5], ...
    [], [], [], [], [], [], nlcon, o);
  asserts.isapprox(x(:), [-1; -1], 1e-3);
  asserts.isapprox(fval, -2, 1e-3);
  asserts.istrue(exitflag > 0);
  asserts.istrue(output.constrviolation <= 1e-5);
end
%=============================================================================
% Trapezoidal collocation NLP: minimum-effort double integrator BVP
% (0,0) -> (1,0), T = 1. Many linear defect equality constraints.
% A correct solver reaches objective ~ 12; Nelson previously stalled at exitflag -2.
N = 15;
T = 1;
h = T / (N - 1);
nz = 3 * N;
w = ones(N, 1); w(1) = 0.5; w(N) = 0.5; w = h * w;
objColl = @(z) sum(w .* z(2 * N + 1:3 * N) .^ 2);
Aeq = []; beq = [];
for k = 1:N - 1
  a = zeros(1, nz); a(k + 1) = 1; a(k) = -1; a(N + k) = -h / 2; a(N + k + 1) = -h / 2;
  Aeq = [Aeq; a]; beq = [beq; 0];
end
for k = 1:N - 1
  a = zeros(1, nz); a(N + k + 1) = 1; a(N + k) = -1; a(2 * N + k) = -h / 2; a(2 * N + k + 1) = -h / 2;
  Aeq = [Aeq; a]; beq = [beq; 0];
end
b1 = zeros(1, nz); b1(1) = 1; Aeq = [Aeq; b1]; beq = [beq; 0];
b2 = zeros(1, nz); b2(N + 1) = 1; Aeq = [Aeq; b2]; beq = [beq; 0];
b3 = zeros(1, nz); b3(N) = 1; Aeq = [Aeq; b3]; beq = [beq; 1];
b4 = zeros(1, nz); b4(2 * N) = 1; Aeq = [Aeq; b4]; beq = [beq; 0];
for k = 1:2
  o = optimoptions('fmincon', 'Display', 'off', 'Algorithm', algos{k}, 'MaxIterations', 100);
  [z, fval, exitflag, output] = fmincon(objColl, zeros(nz, 1), [], [], Aeq, beq, [], [], [], o);
  asserts.istrue(exitflag > 0);
  asserts.istrue(output.constrviolation <= 1e-6);
  asserts.istrue(abs(fval - 12) < 1);
end
%=============================================================================
