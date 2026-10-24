%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opts = optimoptions('fmincon', 'Display', 'off', 'MaxIterations', 100);
fun = @(x) 100 * (x(2) - x(1) ^ 2) ^ 2 + (1 - x(1)) ^ 2;
[x, fval, exitflag, output, lambda, grad, hessian] = fmincon(fun, [-1; 2], [1, 2], 1, [], [], [], [], [], opts);
asserts.isapprox(x(:), [0.5022; 0.2489], 2e-3);
asserts.isapprox(fval, 0.2489, 2e-3);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
asserts.istrue(isstruct(lambda));
asserts.istrue(isfield(lambda, 'ineqlin'));
asserts.isequal(size(grad), [2 1]);
asserts.isequal(size(hessian), [2 2]);
%=============================================================================
[x, fval, exitflag] = fmincon(fun, [0.5; 0], [1, 2], 1, [2, 1], 1, [], [], [], opts);
asserts.isapprox(x(:), [0.4149; 0.1701], 2e-3);
asserts.isapprox(fval, 0.3427, 3e-3);
asserts.istrue(exitflag > 0);
%=============================================================================
activeOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'active-set', 'MaxIterations', 100);
[x, fval, exitflag, output] = fmincon(fun, [-1; 2], [1, 2], 1, [], [], [], [], [], activeOpts);
asserts.isapprox(x(:), [0.5022; 0.2489], 2e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'active-set');
asserts.istrue(isfield(output, 'activeconstraints'));
asserts.isequal(output.activeconstraints, 1);
[x, fval, exitflag, output, lambda] = fmincon(fun, [-1; 2], [1, 2], 1, [], [], [], [], [], activeOpts);
asserts.isequal(size(lambda.ineqlin), [1 1]);
asserts.istrue(lambda.ineqlin >= 0);
legacyOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp-legacy', 'MaxIterations', 100);
[x, fval, exitflag, output] = fmincon(fun, [-1; 2], [1, 2], 1, [], [], [], [], [], legacyOpts);
asserts.isapprox(x(:), [0.5022; 0.2489], 2e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'sqp-legacy');
%=============================================================================
% active-set must reach the global constrained optimum on a mixed
% equality/inequality/bound problem instead of stopping at a vertex whose
% bound multiplier is negative (regression, see negativeMultiplierConstraint).
mixedOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'active-set', 'MaxIterations', 400);
[x, fval, exitflag] = fmincon(@(x)(x(1) - 2) ^ 2 + (x(2) - 1) ^ 2, [0; 3], [], [], [1, 1], 3, [0; 0], [], [], mixedOpts);
asserts.isapprox(x(:), [2; 1], 1e-4);
asserts.istrue(abs(fval) < 1e-6);
asserts.istrue(exitflag > 0);
%=============================================================================
% active-set must handle a problem without any constraint (empty A/Aeq/bounds
% and no nonlcon) without a dimension-mismatch error (regression).
unconstrainedActiveOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'active-set');
[x, fval, exitflag] = fmincon(@(x)(x(1) - 3) ^ 2 + (x(2) + 1) ^ 2, [0; 0], [], [], [], [], [], [], [], unconstrainedActiveOpts);
asserts.isapprox(x(:), [3; -1], 1e-4);
asserts.istrue(abs(fval) < 1e-6);
asserts.istrue(exitflag > 0);
sqpLegacyUnconstrained = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp-legacy');
[x, fval, exitflag] = fmincon(@(x)(x(1) - 3) ^ 2 + (x(2) + 1) ^ 2, [0; 0], [], [], [], [], [], [], [], sqpLegacyUnconstrained);
asserts.isapprox(x(:), [3; -1], 1e-4);
asserts.istrue(exitflag > 0);
%=============================================================================
% active-set must converge at a degenerate vertex where more inequalities are
% active than variables, without cycling on the working-set updates
% (anti-cycling guard). The optimum [1; 1] activates all three constraints.
degenerateOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'active-set', 'MaxIterations', 400);
degenerateObj = @(x)(x(1) - 2) ^ 2 + (x(2) - 2) ^ 2;
[x, fval, exitflag] = fmincon(degenerateObj, [0; 0], [1, 0; 0, 1; 1, 1], [1; 1; 2], [], [], [], [], [], degenerateOpts);
asserts.isapprox(x(:), [1; 1], 1e-4);
asserts.istrue(exitflag > 0);
[x, fval, exitflag] = fmincon(degenerateObj, [1; 1], [1, 0; 0, 1; 1, 1], [1; 1; 2], [], [], [], [], [], degenerateOpts);
asserts.isapprox(x(:), [1; 1], 1e-4);
asserts.istrue(exitflag > 0);
%=============================================================================
boundFun = @(x) 1 + x(1) / (1 + x(2)) - 3 * x(1) * x(2) + x(2) * (1 + x(1));
[x, fval, exitflag] = fmincon(boundFun, [0.5; 1], [], [], [], [], [0; 0], [1; 2], [], opts);
asserts.isapprox(x(:), [1; 2], 2e-3);
asserts.isapprox(fval, -2 / 3, 2e-3);
asserts.istrue(exitflag > 0);
%=============================================================================
[x, fval, exitflag] = fmincon(fun, [1 / 4; 1 / 4], [], [], [], [], [0; 0.2], [0.5; 0.8], @optimTestFminconCircle, opts);
asserts.isapprox(x(:), [0.5; 0.25], 3e-3);
asserts.istrue(exitflag > 0);
%=============================================================================
sqpOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp', 'MaxIterations', 120);
[x, fval, exitflag, output, lambda] = fmincon(fun, [0; 0], [], [], [], [], [], [], @optimTestFminconUnitDisk, sqpOpts);
asserts.isapprox(x(:), [0.7864; 0.6177], 5e-3);
asserts.istrue(abs(fval - 0.0457) < 5e-3);
asserts.istrue(exitflag > 0);
asserts.istrue(isfield(output, 'bestfeasible'));
asserts.isfalse(isfield(output, 'restoration') && output.restoration);
asserts.istrue(isfield(lambda, 'ineqnonlin'));
asserts.istrue(isfield(lambda, 'eqnonlin'));
asserts.isequal(length(lambda.ineqnonlin), 1);
asserts.istrue(lambda.ineqnonlin(1) >= 0);
ipNonlinearOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'interior-point', 'MaxIterations', 120);
[x, fval, exitflag, output] = fmincon(fun, [0; 0], [], [], [], [], [], [], @optimTestFminconUnitDisk, ipNonlinearOpts);
asserts.isapprox(x(:), [0.7864; 0.6177], 8e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'interior-point');
ipBarrierOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'interior-point', ...
  'InitBarrierParam', 1e-2, 'BarrierParamUpdate', 'predictor-corrector', 'MaxIterations', 80);
[x, fval, exitflag, output] = fmincon(fun, [-1; 2], [1, 2], 1, [], [], [], [], [], ipBarrierOpts);
asserts.isapprox(x(:), [0.5022; 0.2489], 4e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'interior-point');
asserts.istrue(isfield(output, 'barrieriterations'));
asserts.istrue(output.barrieriterations >= 1);
activeNonlinearOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'active-set', 'MaxIterations', 120);
[x, fval, exitflag, output] = fmincon(fun, [0; 0], [], [], [], [], [], [], @optimTestFminconUnitDisk, activeNonlinearOpts);
asserts.isapprox(x(:), [0.7864; 0.6177], 8e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'active-set');
illScale = [1e-3; 1; 1e3; 1e5];
illTarget = [2e-3; -3; 2e3; 4e5];
illFun = @(x) sum(((x(:) - illTarget) ./ illScale) .^ 2);
illCon = @(x) deal(sum((x(:) ./ (10 * illScale)) .^ 2) - 2, []);
illOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp', ...
  'ScaleProblem', 'obj-and-constr', 'TypicalX', illScale, 'MaxIterations', 160);
[x, fval, exitflag, output, lambda] = fmincon(illFun, zeros(4, 1), [], [], [], [], ...
  - 10 * illScale, 10 * illScale, illCon, illOpts);
asserts.istrue(exitflag > 0);
asserts.istrue(output.constrviolation < 1e-5);
asserts.istrue(fval < 1e-4);
asserts.istrue(isfield(lambda, 'ineqnonlin'));
%=============================================================================
gradOpts = optimoptions('fmincon', 'Display', 'off', 'MaxIterations', 120, ...
  'SpecifyObjectiveGradient', true, 'SpecifyConstraintGradient', true);
[x, fval, exitflag] = fmincon(@optimTestFminconRosenGrad, [1 / 4; 1 / 4], [], [], [], [], ...
  [0; 0.2], [0.5; 0.8], @optimTestFminconCircleGrad, gradOpts);
asserts.isapprox(x(:), [0.5; 0.25], 3e-3);
asserts.istrue(exitflag > 0);
%=============================================================================
trrOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'trust-region-reflective', ...
  'SpecifyObjectiveGradient', true, 'MaxIterations', 50);
[x, fval, exitflag, output, lambda] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], ...
  [0; 0], [2; 3], [], trrOpts);
asserts.isapprox(x(:), [1; 2], 2e-3);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'trust-region-reflective');
asserts.istrue(output.cgiterations > 0);
asserts.istrue(isfield(output, 'pcgflag'));
asserts.istrue(isfield(output, 'pcgresidual'));
asserts.istrue(isfield(output, 'trustregionradius'));
asserts.istrue(isfield(lambda, 'lower'));
trrHessOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'trust-region-reflective', ...
  'SpecifyObjectiveGradient', true, 'HessianFcn', @optimTestFminconQuadraticHessian, ...
  'MaxPCGIter', 20, 'MaxIterations', 50);
[x, fval, exitflag, output] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], ...
  [0; 0], [2; 3], [], trrHessOpts);
asserts.isapprox(x(:), [1; 2], 2e-3);
asserts.istrue(exitflag > 0);
trrMatrixHessOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'trust-region-reflective', ...
  'SpecifyObjectiveGradient', true, 'Hessian', 2 * eye(2), 'MaxIterations', 50);
[x, fval, exitflag] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], ...
  [0; 0], [2; 3], [], trrMatrixHessOpts);
asserts.isapprox(x(:), [1; 2], 2e-3);
asserts.istrue(exitflag > 0);
trrMultOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'trust-region-reflective', ...
  'SpecifyObjectiveGradient', true, 'HessianMultiplyFcn', @optimTestFminconQuadraticHessMult, ...
  'MaxIterations', 50);
[x, fval, exitflag] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], ...
  [0; 0], [2; 3], [], trrMultOpts);
asserts.isapprox(x(:), [1; 2], 2e-3);
asserts.istrue(exitflag > 0);
trrProjOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'trust-region-reflective', ...
  'SpecifyObjectiveGradient', true, 'Hessian', 2 * eye(2), 'MaxProjCGIter', 1, ...
  'MaxIterations', 10);
[x, fval, exitflag, output] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], ...
  [0; 0], [2; 3], [], trrProjOpts);
asserts.istrue(output.cgiterations <= output.iterations);
asserts.istrue(exitflag > 0);
trrPrecondOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'trust-region-reflective', ...
  'SpecifyObjectiveGradient', true, 'Hessian', diag([2 20]), 'PrecondBandWidth', 0, ...
  'MaxIterations', 50);
[x, fval, exitflag, output] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], ...
  [0; 0], [2; 3], [], trrPrecondOpts);
asserts.istrue(exitflag > 0);
asserts.istrue(output.cgiterations > 0);
trrBandPrecondOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'trust-region-reflective', ...
  'SpecifyObjectiveGradient', true, 'Hessian', [4 1; 1 6], 'PrecondBandWidth', 1, ...
  'MaxIterations', 50);
[x, fval, exitflag, output] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], ...
  [0; 0], [2; 3], [], trrBandPrecondOpts);
asserts.istrue(exitflag > 0);
asserts.istrue(output.pcgresidual >= 0);
%=============================================================================
problem.objective = fun;
problem.x0 = [0; 0];
problem.nonlcon = @optimTestFminconUnitDisk;
problem.solver = 'fmincon';
problem.options = sqpOpts;
[x, fval, exitflag] = fmincon(problem);
asserts.isapprox(x(:), [0.7864; 0.6177], 5e-3);
asserts.istrue(exitflag > 0);
%=============================================================================
asserts.checkerror('fmincon(@(x) x, [], [], [])', sprintf(_('%s must be nonempty.'), 'Initial point'));
asserts.checkerror('fmincon(@(x) x(1), 1 + i, [], [])', sprintf(_('%s must be a real numeric array.'), 'Initial point'));
asserts.checkerror('fmincon(@(x) x(1), [0;0], [1 2 3], 1)', _('A must have one column for each variable.'));
asserts.checkerror('fmincon(@(x) x(1), [0;0], [], [], [], [], [1;2], [0;3])', _('Lower bounds must not exceed upper bounds.'));
asserts.checkerror('fmincon(@(x) [x(1); x(1)], 0)', sprintf(_('%s must return a numeric scalar.'), 'Objective function'));
badOpts = optimoptions('fmincon', 'Algorithm', 'trust-region-reflective');
asserts.checkerror('fmincon(@(x) x(1)^2, 0, 1, 1, [], [], [], [], [], badOpts)', _('Trust-region-reflective requires bound constraints or linear equalities only.'));
badDisplay = optimoptions('fmincon', 'Display', 'verbose');
asserts.checkerror('fmincon(@(x) x(1)^2, 0, [], [], [], [], [], [], [], badDisplay)', sprintf(_('Invalid value for "%s".'), 'Display'));
badProblem.objective = @(x) x(1) ^ 2;
badProblem.x0 = 0;
asserts.checkerror('fmincon(badProblem)', sprintf(_('Problem structure must contain %s fields.'), 'solver and options'));
asserts.checkerror('fmincon(@(x) x(1)^2, 0, [], [], [], [], [], [], @optimTestFminconBadConstraint)', _('Nonlinear constraint function must return real numeric values.'));
badGradOpts = optimoptions('fmincon', 'Display', 'off', 'SpecifyConstraintGradient', true);
asserts.checkerror('fmincon(@(x) x(1)^2, [0;0], [], [], [], [], [], [], @optimTestFminconBadConstraintGrad, badGradOpts)', sprintf(_('Nonlinear %s gradient has invalid dimensions.'), 'inequality'));
%=============================================================================
opts = optimoptions('fmincon');
asserts.isequal(opts.Algorithm, 'interior-point');
asserts.isequal(opts.MaxIter, 1000);
asserts.isequal(opts.MaxFunctionEvaluations, 3000);
asserts.isequal(opts.StepTolerance, 1e-10);
asserts.isequal(opts.SubproblemAlgorithm, 'factorization');
opts = optimoptions('fmincon', 'Algorithm', 'sqp');
asserts.isequal(opts.MaxIterations, 400);
asserts.isequal(opts.MaxFunctionEvaluations, '100*numberOfVariables');
asserts.isequal(opts.StepTolerance, 1e-6);
opts = optimset('fmincon');
asserts.isequal(opts.Algorithm, 'interior-point');
asserts.isequal(optimget(opts, 'ObjectiveLimit'), -1e20);
opts = optimoptions('fmincon', 'EnableFeasibilityMode', true, 'UseCodegenSolver', false);
asserts.istrue(opts.EnableFeasibilityMode);
asserts.isfalse(opts.UseCodegenSolver);
%=============================================================================
% infeasible linear constraints: x(1) <= -1 and -x(1) <= -1 cannot both hold
infOpts = optimoptions('fmincon', 'Display', 'off');
[x, fval, exitflag, output] = fmincon(@(x) sum(x .^ 2), [0; 0], [1 0; -1 0], [-1; -1], [], [], [], [], [], infOpts);
asserts.isequal(exitflag, -2);
asserts.istrue(output.constrviolation > 0);
%=============================================================================
stopOpts = optimoptions('fmincon', 'Display', 'off', 'OutputFcn', @optimTestFminconStop, 'MaxIterations', 20);
[x, fval, exitflag, output] = fmincon(@(x)(x(1) - 1) ^ 2, 0, [], [], [], [], [], [], [], stopOpts);
asserts.isequal(exitflag, -1);
asserts.istrue(isfield(output, 'bestfeasible'));
%=============================================================================
iterOpts = optimoptions('fmincon', 'Display', 'iter', 'MaxIterations', 3);
[x, fval, exitflag, output] = fmincon(@(x)(x(1) - 1) ^ 2, 0, [], [], [], [], [], [], [], iterOpts);
asserts.istrue(isstruct(output));
%=============================================================================
detailOpts = optimoptions('fmincon', 'Display', 'iter-detailed', 'MaxIterations', 3);
[x, fval, exitflag, output] = fmincon(@(x)(x(1) - 1) ^ 2, 0, [], [], [], [], [], [], [], detailOpts);
asserts.istrue(isstruct(output));
detailOpts = optimoptions('fmincon', 'Display', 'final-detailed', 'MaxIterations', 3);
[x, fval, exitflag, output] = fmincon(@(x)(x(1) - 1) ^ 2, 0, [], [], [], [], [], [], [], detailOpts);
asserts.istrue(isstruct(output));
%=============================================================================
notifyOpts = optimoptions('fmincon', 'Display', 'notify-detailed', 'MaxIterations', 3);
[x, fval, exitflag, output] = fmincon(@(x)(x(1) - 1) ^ 2, 0, [], [], [], [], [], [], [], notifyOpts);
asserts.istrue(isstruct(output));
sqpLimitOpts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp', ...
  'MaxIterations', 20, 'MaxSQPIter', 1, 'RelLineSrchBnd', 0.1);
[x, fval, exitflag, output] = fmincon(@(x)(x(1) - 10) ^ 2, 0, [], [], [], [], [], [], [], sqpLimitOpts);
asserts.istrue(output.iterations <= 1);
asserts.istrue(output.stepsize <= 0.1001);
diagOpts = optimoptions('fmincon', 'Display', 'off', 'Diagnostics', 'on', 'MaxIterations', 3);
diagText = evalc('[x, fval, exitflag, output] = fmincon(@(x) (x(1) - 1) ^ 2, 0, [], [], [], [], [], [], [], diagOpts);');
asserts.istrue(isstruct(output));
asserts.istrue(contains(diagText, 'Diagnostic Information'));
diagHessOpts = optimoptions('fmincon', 'Display', 'off', 'Diagnostics', 'on', ...
  'Algorithm', 'trust-region-reflective', 'SpecifyObjectiveGradient', true, ...
  'HessianMultiplyFcn', @optimTestFminconQuadraticHessMult, 'MaxIterations', 3);
diagText = evalc('[x, fval, exitflag, output] = fmincon(@optimTestFminconQuadraticGrad, [0; 0], [], [], [], [], [0; 0], [2; 3], [], diagHessOpts);');
asserts.istrue(contains(diagText, 'user-supplied multiply function'));
%=============================================================================
