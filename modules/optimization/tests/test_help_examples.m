%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opts = optimset('TolX', 1e-8, 'TolFun', 1e-8);
[x, fval] = fminsearch(@(x)(x(1) - 1) ^ 2 + (x(2) + 2) ^ 2, [0 0], opts);
asserts.istrue(max(abs(x(:) - [1; -2])) < 1e-3);
%=============================================================================
fun = @(x) 3 * x(1) ^ 2 + 2 * x(1) * x(2) + x(2) ^ 2 - 4 * x(1) + 5 * x(2);
opts = optimoptions('fminunc', 'Display', 'off');
[x, fval] = fminunc(fun, [1, 1], opts);
asserts.istrue(max(abs(x(:) - [2.25; -4.75])) < 1e-4);
%=============================================================================
[x, fval] = fminbnd(@(x)(x - 1.5) ^ 2, -2, 4);
asserts.istrue(abs(x - 1.5) < 1e-3);
%=============================================================================
[x, fval] = fzero(@(x) x ^ 2 - 4, [0 5]);
asserts.istrue(abs(x - 2) < 1e-6);
%=============================================================================
C = [1 0; 0 1; 1 1];
d = [1; 2; 3];
[x, resnorm] = lsqnonneg(C, d);
asserts.istrue(max(abs(x - [1; 2])) < 1e-7);
%=============================================================================
H = [2 0; 0 2];
f = [-2; -4];
lb = [0; 0];
[x, fval] = quadprog(H, f, [], [], [], [], lb, []);
asserts.istrue(max(abs(x - [1; 2])) < 1e-7);
%=============================================================================
opts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp');
fun = @(x) 100 * (x(2) - x(1) ^ 2) ^ 2 + (1 - x(1)) ^ 2;
[x, fval] = fmincon(fun, [0; 0], [], [], [], [], [], [], @optimTestFminconUnitDisk, opts);
asserts.istrue(max(abs(x(:) - [0.7864; 0.6177])) < 5e-3);
%=============================================================================
fun = @(x)[x(1) - 2; x(2) + 1];
[x, resnorm] = lsqnonlin(fun, [0; 0]);
asserts.istrue(max(abs(x - [2; -1])) < 1e-5);
%=============================================================================
fun = @(x)[x(1) - 3; x(2) + 4];
[x, fval] = fsolve(fun, [0; 0]);
asserts.istrue(max(abs(x - [3; -4])) < 1e-5);
%=============================================================================
opts = optimset('TolX', 1e-8, 'Display', 'off');
tol = optimget(opts, 'TolX');
asserts.isequal(tol, 1e-8);
%=============================================================================
opts = optimset('MaxIter', 200);
maxiter = optimget(opts, 'MaxIter', 100);
asserts.isequal(maxiter, 200);
%=============================================================================
opts = optimoptions('fsolve', 'TolFun', 1e-8);
[x, fval] = fsolve(@(x) x - 3, 0, opts);
asserts.istrue(abs(x - 3) < 1e-6);
%=============================================================================
x = optimvar('x');
prob = optimproblem('Objective', (x - 2) ^ 2);
[sol, fval] = solve(prob);
asserts.istrue(abs(sol.x - 2) < 1e-3);
%=============================================================================
x = optimvar('x', 2, 1, 'LowerBound', 0);
expr = (x(1) - 1) ^ 2 + (x(2) - 2) ^ 2;
asserts.istrue(isa(expr, 'optim.problemdef.OptimizationExpression'));
%=============================================================================
x = optimvar('x');
expr = optimexpr(3) + x ^ 2;
value = evaluate(expr, struct('x', 2));
asserts.isequal(value, 7);
%=============================================================================
x = optimvar('x');
c = optimconstr(x, '<=', 5);
asserts.istrue(evaluate(c, struct('x', 4)));
%=============================================================================
x = optimvar('x');
prob = optimproblem('Objective', (x + 1) ^ 2);
s = prob2struct(prob);
asserts.istrue(isstruct(s));
%=============================================================================
x = optimvar('x');
expr = (x - 4) ^ 2;
value = evaluate(expr, struct('x', 3));
asserts.isequal(value, 1);
%=============================================================================
x = optimvar('x');
prob = optimproblem('Objective', (x - 2) ^ 2);
show(prob);
%=============================================================================
