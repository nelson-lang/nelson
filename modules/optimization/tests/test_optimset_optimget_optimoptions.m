%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opts = optimset('TolX', 1e-8, 'MaxIter', 12);
asserts.isequal(opts.TolX, 1e-8);
asserts.isequal(opts.MaxIter, 12);
asserts.isequal(optimget(opts, 'TolX'), 1e-8);
asserts.isequal(optimget(opts, 'TolFun', 7), 7);
%=============================================================================
options = optimoptions('fminsearch', 'Display', 'off', 'MaxIterations', 20);
asserts.istrue(isobject(options));
asserts.isequal(options.Display, 'off');
asserts.isequal(options.MaxIter, 20);
st = struct(options);
asserts.isequal(st.Display, 'off');
%=============================================================================
directOptions = optim.options.SolverOptions('fminsearch', 'MaxIterations', 10);
asserts.isequal(directOptions.SolverName, 'fminsearch');
asserts.isequal(directOptions.MaxIter, 10);
directOptions.StepTolerance = 1e-7;
asserts.isequal(directOptions.TolX, 1e-7);
%=============================================================================
opts = optimset('MaxFunEvals', 25, 'FunctionTolerance', 1e-9, 'StepTolerance', 1e-8);
asserts.isequal(opts.MaxFunctionEvaluations, 25);
asserts.isequal(opts.TolFun, 1e-9);
asserts.isequal(opts.TolX, 1e-8);
%=============================================================================
opts = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp', ...
  'EnableFeasibilityMode', true, 'UseCodegenSolver', false);
asserts.isequal(opts.Algorithm, 'sqp');
asserts.istrue(opts.EnableFeasibilityMode);
asserts.isfalse(opts.UseCodegenSolver);
legacy = optimset('HessFcn', @sin, 'TolCon', 1e-5);
asserts.istrue(isa(legacy.HessianFcn, 'function_handle'));
asserts.isequal(legacy.ConstraintTolerance, 1e-5);
%=============================================================================
opts = optimoptions('fminunc', 'Algorithm', 'trust-region', ...
  'SpecifyObjectiveGradient', true, 'HessianApproximation', {'lbfgs', 5});
asserts.isequal(opts.Algorithm, 'trust-region');
asserts.istrue(opts.SpecifyObjectiveGradient);
asserts.istrue(iscell(opts.HessianApproximation));
asserts.isequal(opts.HessianApproximation{2}, 5);
legacy = optimset('HessUpdate', 'dfp', 'TolX', 1e-7);
asserts.isequal(legacy.HessUpdate, 'dfp');
asserts.isequal(legacy.TolX, 1e-7);
%=============================================================================
prob = optimproblem('Objective', optimvar('x') .^ 2);
probOptions = optimoptions(prob, 'MaxIterations', 15);
asserts.istrue(isobject(probOptions));
asserts.isequal(probOptions.MaxIter, 15);
%=============================================================================
asserts.checkerror('optimset(''To'', 1)', sprintf(_('Ambiguous optimization option "%s".'), 'To'));
asserts.checkerror('optimoptions()', _('Solver name expected.'));
asserts.checkerror('optimoptions(''fsolve'', ''UseParallel'', [true false])', _('UseParallel must be a logical scalar.'));
asserts.checkerror('optimoptions(''fsolve'', ''MaxIterations'', NaN)', _('MaxIterations must be a numeric scalar.'));
%=============================================================================
