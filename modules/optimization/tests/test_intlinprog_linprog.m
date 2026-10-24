%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), 'HiGHS backend is not available in the WebAssembly build.');
opts = optimoptions('intlinprog', 'Display', 'off');
asserts.isequal(opts.Algorithm, 'highs');
asserts.isequal(optimget(opts, 'Display'), 'off');
%=============================================================================
f = [8; 1];
intcon = 2;
A = [-1 -2; -4 -1; 2 1];
b = [14; -33; 20];
[x, fval, exitflag, output] = intlinprog(f, intcon, A, b, [], [], [], [], [], opts);
asserts.isapprox(x, [6.5; 7], 1e-10);
asserts.isapprox(fval, 59, 1e-10);
asserts.isequal(exitflag, 1);
asserts.isequal(output.algorithm, 'highs');
asserts.istrue(output.constrviolation < 1e-8);
asserts.isequal(output.status, 'optimal');
asserts.istrue(output.primalstatus >= 0);
%=============================================================================
problem = struct();
problem.solver = 'intlinprog';
problem.f = [-3; -2; -1];
problem.intcon = 3;
problem.Aineq = [1 1 1];
problem.bineq = 7;
problem.Aeq = [4 2 1];
problem.beq = 12;
problem.lb = zeros(3, 1);
problem.ub = [Inf; Inf; 1];
problem.options = opts;
[x, fval, exitflag] = intlinprog(problem);
asserts.isapprox(x, [0; 6; 0], 1e-10);
asserts.isapprox(fval, -12, 1e-10);
asserts.isequal(exitflag, 1);
%=============================================================================
lpopts = optimoptions('linprog', 'Display', 'off');
asserts.isequal(lpopts.Algorithm, 'highs');
[x, fval, exitflag, output, lambda] = linprog([-1; -1], [1 2; 4 2], [4; 12], [], [], [0; 0], [], lpopts);
asserts.isapprox(x, [8 / 3; 2 / 3], 1e-9);
asserts.isapprox(fval, -10 / 3, 1e-9);
asserts.isequal(exitflag, 1);
asserts.isequal(output.algorithm, 'highs');
asserts.istrue(isstruct(lambda));
asserts.istrue(isfield(lambda, 'ineqlin'));
asserts.istrue(isfield(lambda, 'eqlin'));
asserts.istrue(isfield(lambda, 'lower'));
asserts.istrue(isfield(lambda, 'upper'));
asserts.isapprox(lambda.ineqlin, [1 / 3; 1 / 6], 1e-9);
asserts.isapprox(lambda.lower, [0; 0], 1e-12);
asserts.isapprox(lambda.upper, [0; 0], 1e-12);
asserts.istrue(isfield(output, 'iterations'));
asserts.istrue(isfield(output, 'cgiterations'));
asserts.istrue(isfield(output, 'firstorderopt'));
asserts.istrue(output.iterations >= 0);
asserts.istrue(output.firstorderopt < 1e-8);
%=============================================================================
[x, fval, exitflag, output, lambda] = linprog([1; 2], [-1 0; 0 -1; 1 1], [-1; -2; 5], ...
  [1 -1], -1, [0; 0], [3; 4], lpopts);
asserts.isapprox(x, [1; 2], 1e-10);
asserts.isapprox(fval, 5, 1e-10);
asserts.isequal(exitflag, 1);
asserts.isapprox(lambda.ineqlin, [0; 3; 0], 1e-10);
asserts.isapprox(lambda.eqlin, 1, 1e-10);
asserts.isapprox(lambda.lower, [0; 0], 1e-12);
asserts.isapprox(lambda.upper, [0; 0], 1e-12);
%=============================================================================
[x, fval, exitflag, output, lambda] = linprog(1, [], [], [], [], 1, [], lpopts);
asserts.isapprox(x, 1, 1e-12);
asserts.isapprox(fval, 1, 1e-12);
asserts.isequal(exitflag, 1);
asserts.isapprox(lambda.lower, 1, 1e-12);
asserts.isapprox(lambda.upper, 0, 1e-12);
%=============================================================================
[x, fval, exitflag, output, lambda] = linprog(-1, [], [], [], [], [], 2, lpopts);
asserts.isapprox(x, 2, 1e-12);
asserts.isapprox(fval, -2, 1e-12);
asserts.isequal(exitflag, 1);
asserts.isapprox(lambda.lower, 0, 1e-12);
asserts.isapprox(lambda.upper, 1, 1e-12);
%=============================================================================
problem = struct();
problem.solver = 'linprog';
problem.f = [-1; -1];
problem.A = [1 2; 4 2];
problem.b = [4; 12];
problem.lb = [0; 0];
problem.options = lpopts;
[x, fval, exitflag] = linprog(problem);
asserts.isapprox(x, [8 / 3; 2 / 3], 1e-9);
asserts.isapprox(fval, -10 / 3, 1e-9);
asserts.isequal(exitflag, 1);
%=============================================================================
[x, fval, exitflag] = linprog([-1; -1], [1 2; 4 2], [4; 12], [], [], [0; 0], [], [0; 0], lpopts);
asserts.isapprox(x, [8 / 3; 2 / 3], 1e-9);
asserts.isapprox(fval, -10 / 3, 1e-9);
asserts.isequal(exitflag, 1);
%=============================================================================
f = [1; 1];
A = [-1 -1];
b = -1;
problem = struct('solver', 'linprog', 'f', f, 'Aineq', A, 'bineq', b, 'lb', zeros(2, 1), 'options', lpopts);
[x, fval, exitflag] = linprog(problem);
asserts.istrue(sum(x) >= 1 - 1e-9);
asserts.isapprox(fval, 1, 1e-9);
asserts.isequal(exitflag, 1);
%=============================================================================
optsLimit = optimoptions('linprog', 'Display', 'off', 'LPPreprocess', 'none', 'MaxIterations', 0);
[x, fval, exitflag, output] = linprog([-1; -1], [1 2; 4 2], [4; 12], [], [], [0; 0], [], optsLimit);
asserts.isequal(exitflag, 2);
asserts.istrue(contains(output.message, 'iteration limit'));
asserts.isequal(output.status, 'iteration-limit');
asserts.istrue(output.iterations == 0);
asserts.istrue(isempty(fval) || isnumeric(fval));
%=============================================================================
[x, fval, exitflag, output] = linprog(1, [1; -1], [0; -1], [], [], [], [], lpopts);
asserts.istrue(isempty(x));
asserts.istrue(isempty(fval));
asserts.isequal(exitflag, -2);
asserts.istrue(contains(output.message, 'infeasible'));
asserts.isequal(output.status, 'infeasible');
%=============================================================================
[x, fval, exitflag, output] = linprog(-1, [], [], [], [], [], [], lpopts);
asserts.istrue(isempty(x));
asserts.istrue(isempty(fval));
asserts.isequal(exitflag, -3);
asserts.istrue(contains(output.message, 'unbounded'));
asserts.isequal(output.status, 'unbounded');
%=============================================================================
optsTime = optimoptions('intlinprog', 'Display', 'off', 'MaxTime', 0);
[x, fval, exitflag, output] = intlinprog([8; 1], 2, [-1 -2; -4 -1; 2 1], [14; -33; 20], [], [], [], [], [], optsTime);
asserts.istrue(isempty(x));
asserts.istrue(isempty(fval));
asserts.isequal(exitflag, 0);
asserts.istrue(contains(output.message, 'time limit'));
asserts.isequal(output.status, 'time-limit');
%=============================================================================
optsMip = optimoptions('intlinprog', 'Display', 'off', 'Heuristics', 'none', ...
  'CutGeneration', 'none', 'MaxNodes', 1, 'MaxNumFeasPoints', 1, ...
  'AbsoluteGapTolerance', 1e-9, 'RelativeGapTolerance', 1e-9);
fMip = [8; 1];
[x, fval, exitflag, output] = intlinprog(fMip, 2, [-1 -2; -4 -1; 2 1], [14; -33; 20], [], [], [], [], [], optsMip);
asserts.isapprox(x, [6.5; 7], 1e-10);
asserts.isapprox(fval, 59, 1e-10);
asserts.isequal(exitflag, 1);
asserts.istrue(output.numnodes >= 0);
%=============================================================================
optsIgnored = optimoptions('intlinprog', 'Display', 'off', 'NodeSelection', 'mininfeas', ...
  'ObjectiveImprovementThreshold', 0);
[x, fval, exitflag] = intlinprog(fMip, 2, [-1 -2; -4 -1; 2 1], [14; -33; 20], [], [], [], [], [], optsIgnored);
asserts.isapprox(x, [6.5; 7], 1e-10);
asserts.isapprox(fval, 59, 1e-10);
asserts.isequal(exitflag, 1);
%=============================================================================
[x, fval, exitflag, output] = intlinprog(1, 1, [1; -1], [0; -1], [], [], [], [], [], opts);
asserts.istrue(isempty(x));
asserts.istrue(isempty(fval));
asserts.isequal(exitflag, -2);
asserts.istrue(contains(output.message, 'infeasible'));
%=============================================================================
asserts.checkerror('linprog(1, [], [], [], [], 1, 0, lpopts)', _('Lower bounds must not exceed upper bounds.'));
% the bounds condition also carries a stable identifier
asserts.checkerror('linprog(1, [], [], [], [], 1, 0, lpopts)', _('Lower bounds must not exceed upper bounds.'), 'Nelson:optimization:boundsInconsistent');
asserts.checkerror('linprog(Inf, [], [], [], [], [], [], lpopts)', _('Objective coefficients must be finite.'));
badLPPreprocess = optimoptions('linprog', 'Display', 'off', 'LPPreprocess', 'invalid');
asserts.checkerror('linprog(1, [], [], [], [], 0, 1, badLPPreprocess)', sprintf(_('Invalid value for "%s".'), 'LPPreprocess'));
badRoot = optimoptions('linprog', 'Display', 'off', 'RootLPAlgorithm', 'invalid');
asserts.checkerror('linprog(1, [], [], [], [], 0, 1, badRoot)', sprintf(_('Invalid value for "%s".'), 'RootLPAlgorithm'));
badHeuristics = optimoptions('intlinprog', 'Display', 'off', 'Heuristics', 'invalid');
asserts.checkerror('intlinprog(1, 1, [], [], [], [], 0, 1, [], badHeuristics)', sprintf(_('Invalid value for "%s".'), 'Heuristics'));
badCut = optimoptions('intlinprog', 'Display', 'off', 'CutGeneration', 'invalid');
asserts.checkerror('intlinprog(1, 1, [], [], [], [], 0, 1, [], badCut)', sprintf(_('Invalid value for "%s".'), 'CutGeneration'));
badDisplay = optimoptions('linprog', 'Display', 'verbose');
asserts.checkerror('linprog(1, [], [], [], [], 0, 1, badDisplay)', sprintf(_('Invalid value for "%s".'), 'Display'));
badTime = optimoptions('linprog', 'Display', 'off', 'MaxTime', -1);
asserts.checkerror('linprog(1, [], [], [], [], 0, 1, badTime)', sprintf(_('%s must be nonnegative.'), 'MaxTime'));
%=============================================================================
