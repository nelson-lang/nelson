%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
off = optimoptions('fsolve', 'Display', 'off');
[root, fval, exitflag, output, jacobian] = fsolve(@(x)[x(1) - 3; x(2) + 4], [0; 0], off);
asserts.isapprox(root(:), [3; -4], 1e-5);
asserts.istrue(norm(fval) < 1e-7);
asserts.isequal(exitflag, 1);
asserts.isequal(output.algorithm, 'trust-region-dogleg');
asserts.istrue(isfield(output, 'iterations'));
asserts.istrue(isfield(output, 'funcCount'));
asserts.istrue(isfield(output, 'firstorderopt'));
asserts.istrue(ischar(output.message) && ~isempty(output.message));
asserts.isequal(size(jacobian), [2 2]);
%=============================================================================
% reference defaults
o = optimoptions('fsolve');
asserts.isequal(o.Algorithm, 'trust-region-dogleg');
asserts.isequal(o.MaxIterations, 400);
asserts.isequal(o.MaxFunctionEvaluations, '100*numberOfVariables');
asserts.isequal(o.FunctionTolerance, 1e-6);
asserts.isequal(o.StepTolerance, 1e-6);
asserts.isequal(o.Display, 'final');
%=============================================================================
% stationary point that is not a root
[x2, fval2, exitflag2, output2] = fsolve(@(x) x ^ 2 + 1, 0, off);
asserts.isequal(exitflag2, -2);
asserts.istrue(abs(fval2 - 1) < 1e-6);
%=============================================================================
% array-valued unknowns and residuals keep their shape
[X3, F3, exitflag3] = fsolve(@(X) X * X - [1 0; 0 4], eye(2) + 0.1, off);
asserts.isequal(size(X3), [2 2]);
asserts.isequal(size(F3), [2 2]);
asserts.isequal(exitflag3, 1);
%=============================================================================
% all three algorithms solve Rosenbrock as a system
rosen = @(x)[10 * (x(2) - x(1) ^ 2); 1 - x(1)];
[x4, fval4, exitflag4, output4] = fsolve(rosen, [1.2; 1.2], off);
asserts.isapprox(x4(:), [1; 1], 1e-5);
asserts.isequal(exitflag4, 1);
trOpts = optimoptions('fsolve', 'Display', 'off', 'Algorithm', 'trust-region');
[x5, fval5, exitflag5, output5] = fsolve(rosen, [-1.2; 1], trOpts);
asserts.isapprox(x5(:), [1; 1], 1e-5);
asserts.istrue(exitflag5 > 0);
asserts.isequal(output5.algorithm, 'trust-region');
asserts.istrue(isfield(output5, 'cgiterations'));
asserts.istrue(isfield(output5, 'stepsize'));
lmOpts = optimoptions('fsolve', 'Display', 'off', 'Algorithm', 'levenberg-marquardt');
[x6, fval6, exitflag6, output6] = fsolve(rosen, [-1.2; 1], lmOpts);
asserts.isapprox(x6(:), [1; 1], 1e-5);
asserts.istrue(exitflag6 > 0);
asserts.isequal(output6.algorithm, 'levenberg-marquardt');
asserts.istrue(isfield(output6, 'stepsize'));
%=============================================================================
% non-square systems switch to Levenberg-Marquardt with a warning
[x7, fval7, exitflag7, output7] = fsolve(@(x)[x(1) - 1; x(1) - 1.0001; x(2)], [0; 0], off);
asserts.isequal(output7.algorithm, 'levenberg-marquardt');
asserts.istrue(exitflag7 > 0);
%=============================================================================
% iteration limit
maxOpts = optimoptions('fsolve', 'Display', 'off', 'MaxIterations', 2);
[x8, fval8, exitflag8, output8] = fsolve(rosen, [-1.2; 1], maxOpts);
asserts.isequal(exitflag8, 0);
asserts.istrue(output8.iterations <= 2);
%=============================================================================
% output function can stop the solver
stopOpts = optimoptions('fsolve', 'Display', 'off', 'OutputFcn', @(x, values, state) values.iteration >= 1);
[x9, fval9, exitflag9] = fsolve(@(x)[x(1) - 3; x(2) + 4], [0; 0], stopOpts);
asserts.isequal(exitflag9, -1);
%=============================================================================
% iteration display
iterOpts = optimoptions('fsolve', 'Display', 'iter', 'MaxIterations', 3);
displayed = evalc('fsolve(rosen, [-1.2; 1], iterOpts);');
asserts.istrue(contains(displayed, 'Iteration'));
asserts.istrue(contains(displayed, 'Trust-region'));
%=============================================================================
% problem structure requires solver and options fields
problem.objective = @(x)[x(1) - 3; x(2) + 4];
problem.x0 = [0; 0];
problem.solver = 'fsolve';
problem.options = off;
[x10, fval10, exitflag10] = fsolve(problem);
asserts.isapprox(x10(:), [3; -4], 1e-5);
asserts.isequal(exitflag10, 1);
badProblem.objective = @(x) x;
badProblem.x0 = 0;
asserts.checkerror('fsolve(badProblem)', sprintf(_('Problem structure must contain %s fields.'), 'solver and options'));
%=============================================================================
asserts.checkerror('fsolve(@(x) x, [])', sprintf(_('%s must be nonempty.'), 'Initial point'));
badAlgo = optimoptions('fsolve', 'Algorithm', 'dogleg');
asserts.checkerror('fsolve(@(x) x, 0, badAlgo)', sprintf(_('Invalid value for "%s".'), 'Algorithm'));
%=============================================================================
