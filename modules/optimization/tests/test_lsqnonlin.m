%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
off = optimoptions('lsqnonlin', 'Display', 'off');
lin = @(x)[x(1) - 2; x(2) + 1];
[x, resnorm, residual, exitflag, output, lambda, jacobian] = lsqnonlin(lin, [0; 0], [], [], off);
asserts.isapprox(x(:), [2; -1], 1e-5);
asserts.istrue(abs(resnorm) < 1e-9);
asserts.isequal(exitflag, 1);
asserts.isequal(output.algorithm, 'trust-region-reflective');
asserts.isequal(output.cgiterations, 0);
asserts.istrue(isfield(output, 'stepsize'));
asserts.istrue(isfield(output, 'bestfeasible'));
asserts.istrue(isfield(output, 'constrviolation'));
asserts.istrue(ischar(output.message) && ~isempty(output.message));
asserts.istrue(isfield(lambda, 'lower') && isfield(lambda, 'upper'));
asserts.istrue(isfield(lambda, 'eqlin') && isfield(lambda, 'ineqlin'));
asserts.istrue(isfield(lambda, 'eqnonlin') && isfield(lambda, 'ineqnonlin'));
asserts.isequal(size(jacobian), [2 2]);
%=============================================================================
% reference defaults
o = optimoptions('lsqnonlin');
asserts.isequal(o.Algorithm, 'trust-region-reflective');
asserts.isequal(o.MaxIterations, 400);
asserts.isequal(o.MaxFunctionEvaluations, '100*numberOfVariables');
asserts.isequal(o.FunctionTolerance, 1e-6);
asserts.isequal(o.StepTolerance, 1e-6);
asserts.isequal(o.Display, 'final');
%=============================================================================
% active bounds: multipliers are the gradient of the squared residual
[x, resnorm, residual, exitflag, output, lambda] = lsqnonlin(lin, [0.5; 0.5], [0; 0], [1; 1], off);
asserts.isapprox(x(:), [1; 0], 1e-6);
asserts.isequal(exitflag, 1);
asserts.isapprox(lambda.upper(1), 2, 1e-5);
asserts.isapprox(lambda.lower(2), 2, 1e-5);
asserts.istrue(output.firstorderopt < 1e-6);
%=============================================================================
% Levenberg-Marquardt accepts bounds
lmOpts = optimoptions('lsqnonlin', 'Display', 'off', 'Algorithm', 'levenberg-marquardt');
[x, resnorm, residual, exitflag, output, lambda] = lsqnonlin(lin, [0.5; 0.5], [0; 0], [1; 1], lmOpts);
asserts.isapprox(x(:), [1; 0], 1e-6);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'levenberg-marquardt');
asserts.isapprox(lambda.upper(1), 2, 1e-5);
%=============================================================================
% inconsistent bounds return exitflag -2 without error
[x, resnorm, residual, exitflag] = lsqnonlin(@(x) x, 0, 1, 0, off);
asserts.isequal(exitflag, -2);
%=============================================================================
% array-valued unknowns and residuals keep their shape
[x, resnorm, residual, exitflag] = lsqnonlin(@(x) x - ones(2, 3), zeros(2, 3), [], [], off);
asserts.isequal(size(x), [2 3]);
asserts.isequal(size(residual), [2 3]);
asserts.isequal(exitflag, 1);
%=============================================================================
% nonzero residual plateau reports the function tolerance stop
samples = (1:8)';
observations = [2; 4; 7; 12; 20; 33; 54; 90];
expfit = @(x) x(1) * exp(x(2) * samples) - observations;
[x, resnorm, residual, exitflag, output] = lsqnonlin(expfit, [1; 0.1], [], [], off);
asserts.isapprox(x(:), [1.5728; 0.5058], 1e-3);
asserts.isapprox(resnorm, 0.7373, 1e-3);
asserts.istrue(exitflag > 0);
%=============================================================================
% Rosenbrock residuals converge with both engines
rosen = @(x)[10 * (x(2) - x(1) ^ 2); 1 - x(1)];
[x, resnorm, residual, exitflag] = lsqnonlin(rosen, [-1.2; 1], [], [], off);
asserts.isapprox(x(:), [1; 1], 1e-6);
asserts.isequal(exitflag, 1);
[x, resnorm, residual, exitflag] = lsqnonlin(rosen, [-1.2; 1], [], [], lmOpts);
asserts.isapprox(x(:), [1; 1], 1e-6);
asserts.isequal(exitflag, 1);
%=============================================================================
% iteration limit
maxOpts = optimoptions('lsqnonlin', 'Display', 'off', 'MaxIterations', 1);
[x, resnorm, residual, exitflag, output] = lsqnonlin(rosen, [-1.2; 1], [], [], maxOpts);
asserts.isequal(exitflag, 0);
%=============================================================================
% linear inequality constraints switch to interior-point
[x, resnorm, residual, exitflag, output, lambda] = lsqnonlin(lin, [0; 0], [], [], [1 0; 0 1], [0.5; 10], [], [], [], off);
asserts.isapprox(x(:), [0.5; -1], 1e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'interior-point');
asserts.istrue(output.constrviolation <= 1e-6);
asserts.isapprox(lambda.ineqlin(1), 3, 1e-3);
%=============================================================================
% nonlinear constraints
circle = @(z) deal(z(1) ^ 2 + z(2) ^ 2 - 1, []);
[x, resnorm, residual, exitflag, output, lambda] = lsqnonlin(lin, [0; 0], [], [], [], [], [], [], circle, off);
asserts.isapprox(x(:), [0.8944; -0.4472], 5e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'interior-point');
asserts.istrue(output.constrviolation <= 1e-6);
asserts.istrue(lambda.ineqnonlin(1) > 0);
%=============================================================================
% output function can stop the solver
stopOpts = optimoptions('lsqnonlin', 'Display', 'off', 'OutputFcn', @(x, values, state) values.iteration >= 1);
[x, resnorm, residual, exitflag] = lsqnonlin(rosen, [-1.2; 1], [], [], stopOpts);
asserts.isequal(exitflag, -1);
%=============================================================================
% iteration display
iterOpts = optimoptions('lsqnonlin', 'Display', 'iter', 'MaxIterations', 3);
displayed = evalc('lsqnonlin(rosen, [-1.2; 1], [], [], iterOpts);');
asserts.istrue(contains(displayed, 'Resnorm'));
%=============================================================================
% problem structure with constraint fields
problem.objective = lin;
problem.x0 = [0; 0];
problem.Aineq = [1 0; 0 1];
problem.bineq = [0.5; 10];
problem.solver = 'lsqnonlin';
problem.options = off;
[x, resnorm, residual, exitflag, output] = lsqnonlin(problem);
asserts.isapprox(x(:), [0.5; -1], 1e-3);
asserts.isequal(output.algorithm, 'interior-point');
badProblem.objective = lin;
badProblem.x0 = [0; 0];
asserts.checkerror('lsqnonlin(badProblem)', sprintf(_('Problem structure must contain %s fields.'), 'solver and options'));
%=============================================================================
badAlgo = optimoptions('lsqnonlin', 'Algorithm', 'dogleg');
asserts.checkerror('lsqnonlin(@(x) x, 0, [], [], badAlgo)', sprintf(_('Invalid value for "%s".'), 'Algorithm'));
%=============================================================================
