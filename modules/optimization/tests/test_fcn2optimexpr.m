%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = optimvar('x');
expr = fcn2optimexpr(@(v) sin(v), x);
asserts.istrue(isa(expr, 'optim.problemdef.OptimizationExpression'));
asserts.isapprox(evaluate(expr, struct('x', pi / 2)), 1, 1e-12);
%=============================================================================
x = optimvar('x', 2, 1);
expr = fcn2optimexpr(@(v) sum(v .^ 2), x);
asserts.isapprox(evaluate(expr, struct('x', [3; 4])), 25, 1e-12);
%=============================================================================
x = optimvar('x');
y = optimvar('y');
expr = fcn2optimexpr(@hypot, x, y);
asserts.isequal(sort(expr.Variables), {'x', 'y'});
asserts.isapprox(evaluate(expr, struct('x', 3, 'y', 4)), 5, 1e-12);
%=============================================================================
x = optimvar('x');
expr = fcn2optimexpr(@(v) cos(v), x, 'OutputSize', [1 1]);
asserts.isapprox(evaluate(expr, struct('x', 0)), 1, 1e-12);
%=============================================================================
x = optimvar('x', 'LowerBound', -2, 'UpperBound', 5);
expr = fcn2optimexpr(@(v) exp(v), x);
asserts.isequal(expr.VarLowerBound.x, -2);
asserts.isequal(expr.VarUpperBound.x, 5);
%=============================================================================
x = optimvar('x');
prob = optimproblem('Objective', fcn2optimexpr(@(v)(v - 3) .^ 2 + 1, x));
[sol, fval] = solve(prob, struct('x', 0), optimset('TolX', 1e-8, 'TolFun', 1e-8));
asserts.isapprox(sol.x, 3, 1e-3);
asserts.isapprox(fval, 1, 1e-6);
%=============================================================================
assert_checkerror('fcn2optimexpr(1, 2)', sprintf(_('%s must be a function handle.'), 'First argument'));
%=============================================================================
