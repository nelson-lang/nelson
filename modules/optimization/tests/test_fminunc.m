%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fun = @(x) 3 * x(1) ^ 2 + 2 * x(1) * x(2) + x(2) ^ 2 - 4 * x(1) + 5 * x(2);
[x, fval, exitflag, output, grad, hessian] = fminunc(fun, [1, 1], optimoptions('fminunc', 'Display', 'off'));
asserts.isapprox(x(:), [2.25; -4.75], 2e-4);
asserts.isapprox(fval, -16.375, 1e-6);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
asserts.isequal(output.algorithm, 'quasi-newton');
asserts.isequal(size(grad), [2 1]);
asserts.isequal(size(hessian), [2 2]);
%=============================================================================
legacyOpts = optimset('Display', 'off');
[x, fval, exitflag, output] = fminunc(fun, [1, 1], legacyOpts);
asserts.isapprox(x(:), [2.25; -4.75], 2e-4);
asserts.isapprox(fval, -16.375, 1e-6);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'quasi-newton');
%=============================================================================
opts = optimoptions('fminunc', 'Display', 'off', 'Algorithm', 'trust-region', ...
  'SpecifyObjectiveGradient', true, 'MaxIterations', 100);
[x, fval, exitflag, output] = fminunc(@optimTestFminuncRosenGrad, [-1; 2], opts);
asserts.isapprox(x(:), [1; 1], 2e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'trust-region');
%=============================================================================
opts = optimoptions('fminunc', 'Display', 'off', 'Algorithm', 'trust-region', ...
  'SpecifyObjectiveGradient', true, 'HessianFcn', 'objective', 'MaxIterations', 100);
[x, fval, exitflag, output, grad, hessian] = fminunc(@optimTestFminuncRosenHessian, [-1; 2], opts);
asserts.isapprox(x(:), [1; 1], 2e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(size(hessian), [2 2]);
%=============================================================================
opts = optimoptions('fminunc', 'Display', 'off', 'HessianApproximation', {'lbfgs', 5});
[x, fval, exitflag, output] = fminunc(fun, [1, 1], opts);
asserts.isapprox(x(:), [2.25; -4.75], 2e-4);
asserts.istrue(exitflag > 0);
%=============================================================================
problem.objective = @optimTestFminuncRosenGrad;
problem.x0 = [-1; 2];
problem.solver = 'fminunc';
problem.options = optimoptions('fminunc', 'Display', 'off', 'Algorithm', 'trust-region', ...
  'SpecifyObjectiveGradient', true);
[x, fval, exitflag] = fminunc(problem);
asserts.isapprox(x(:), [1; 1], 2e-3);
asserts.istrue(exitflag > 0);
%=============================================================================
stopOpts = optimoptions('fminunc', 'Display', 'off', 'OutputFcn', @optimTestFminuncStop, 'MaxIterations', 20);
[x, fval, exitflag, output] = fminunc(fun, [1, 1], stopOpts);
asserts.isequal(exitflag, -1);
%=============================================================================
iterOpts = optimoptions('fminunc', 'Display', 'iter-detailed', 'MaxIterations', 3);
[x, fval, exitflag, output] = fminunc(@(x)(x(1) - 1) ^ 2, 0, iterOpts);
asserts.istrue(isstruct(output));
diagOpts = optimoptions('fminunc', 'Display', 'off', 'Diagnostics', 'on', 'MaxIterations', 3);
[x, fval, exitflag, output] = fminunc(@(x)(x(1) - 1) ^ 2, 0, diagOpts);
asserts.istrue(isstruct(output));
%=============================================================================
asserts.checkerror('fminunc(@(x) x, [])', sprintf(_('%s must be nonempty.'), 'Initial point'));
asserts.checkerror('fminunc(@(x) x(1), 1 + i)', sprintf(_('%s must be a real numeric array.'), 'Initial point'));
asserts.checkerror('fminunc(@(x) [x(1); x(1)], 0)', sprintf(_('%s must return a numeric scalar.'), 'Objective function'));
badOpts = optimoptions('fminunc', 'Algorithm', 'active-set');
asserts.checkerror('fminunc(@(x) x(1)^2, 0, badOpts)', sprintf(_('Invalid value for "%s".'), 'Algorithm'));
badProblem.objective = @(x) x(1) ^ 2;
badProblem.x0 = 0;
asserts.checkerror('fminunc(badProblem)', sprintf(_('Problem structure must contain %s fields.'), 'solver and options'));
%=============================================================================
opts = optimoptions('fminunc');
asserts.isequal(opts.Algorithm, 'quasi-newton');
asserts.isequal(opts.MaxIter, 400);
%=============================================================================
