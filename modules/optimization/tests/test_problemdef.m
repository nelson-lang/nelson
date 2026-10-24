%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
skip_testsuite(iswasm(), 'HiGHS backend is not available in the WebAssembly build.');
x = optimvar('x');
expr = (x - 2) .^ 2;
value = evaluate(expr, struct('x', 5));
asserts.isequal(value, 9);
prob = optimproblem('Objective', expr);
problem = prob2struct(prob, struct('x', 0));
asserts.istrue(isstruct(problem));
asserts.isequal(problem.solver, 'quadprog');
[sol, fval, exitflag, output] = solve(prob, struct('x', 0), optimset('TolX', 1e-7, 'TolFun', 1e-7));
asserts.isapprox(sol.x, 2, 1e-3);
asserts.isapprox(fval, 0, 1e-6);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
%=============================================================================
y = optimvar('y', 2, 1);
txt = evalc('show(y)');
asserts.istrue(contains(txt, '[ y(1) ]'));
asserts.istrue(contains(txt, '[ y(2) ]'));
asserts.isfalse(contains(txt, 'OptimizationVariable: y'));
xdoc = optimvar('x', 2);
showtxt = evalc('show(xdoc)');
asserts.istrue(contains(showtxt, '[ x(1) ]'));
asserts.istrue(contains(showtxt, '[ x(2) ]'));
docobj = log(1 + 100 * (xdoc(2) - xdoc(1) ^ 2) ^ 2 + (1 - xdoc(1)) ^ 2);
doctxt = evalc('show(docobj)');
asserts.istrue(contains(doctxt, 'log('));
asserts.istrue(contains(doctxt, 'x(1)'));
asserts.istrue(contains(doctxt, 'x(2)'));
rosen = log(1 + 100 * (y(2) - y(1) ^ 2) ^ 2 + (1 - y(1)) ^ 2);
rosentxt = evalc('show(rosen)');
asserts.istrue(contains(rosentxt, 'log('));
asserts.istrue(contains(rosentxt, 'y(1)'));
asserts.istrue(contains(rosentxt, 'y(2)'));
displayExpr = (y(1) - 1) ^ 2 + (y(2) + 3) ^ 2;
objtxt = evalc('show(displayExpr)');
asserts.istrue(contains(objtxt, 'y(1)'));
asserts.istrue(contains(objtxt, 'y(2)'));
asserts.isfalse(contains(objtxt, 'OptimizationExpression'));
vectorExpr = y + [1; 2];
asserts.isequal(evaluate(vectorExpr(2), struct('y', [3; 4])), 6);
asserts.isequal(evaluate(y(end), struct('y', [3; 4])), 4);
asserts.isequal(evaluate(y(:), struct('y', [3; 4])), [3; 4]);
matVar = optimvar('matVar', 2, 2);
colSum = sum(matVar, 1);
asserts.isequal(evaluate(colSum(2), struct('matVar', [1 2; 3 4])), 6);
displayConstraint = y(1) ^ 2 + y(2) ^ 2 <= 1;
constxt = evalc('show(displayConstraint)');
asserts.istrue(contains(constxt, '<= 1'));
asserts.istrue(contains(constxt, 'y(1)'));
asserts.istrue(contains(constxt, 'y(2)'));
asserts.isfalse(contains(constxt, 'OptimizationConstraint'));
xcons = xdoc(1) ^ 2 + xdoc(2) ^ 2 <= 1;
xconstxt = evalc('show(xcons)');
asserts.istrue(contains(xconstxt, '(x(1).^2 + x(2).^2) <= 1'));
xprob = optimproblem("Objective", docobj, "Constraints", xcons);
x0.x = [0 0];
xstruct = prob2struct(xprob, x0);
asserts.isequal(xstruct.solver, 'fmincon');
[xsol, xfval, xexitflag] = solve(xprob, x0);
asserts.isapprox(xsol.x(:), [0.7864; 0.6177], 5e-4);
asserts.istrue(abs(xfval - 0.0447) < 5e-4);
asserts.istrue(xexitflag > 0);
expr = (y(1) - 1) .^ 2 + (y(2) + 3) .^ 2;
value = evaluate(expr, struct('y', [2; -1]));
asserts.isequal(value, 5);
prob = optimproblem('Objective', expr);
probtxt = evalc('show(prob)');
asserts.istrue(contains(probtxt, 'OptimizationProblem'));
asserts.istrue(contains(probtxt, 'Solve for:'));
asserts.istrue(contains(probtxt, 'minimize :'));
asserts.istrue(contains(probtxt, 'y'));
asserts.isfalse(contains(probtxt, 'OptimizationProblem: minimize'));
problem = prob2struct(prob, struct('y', [0; 0]));
asserts.isequal(problem.solver, 'quadprog');
asserts.isequal(size(problem.x0), [2 1]);
[sol, fval, exitflag] = solve(prob, struct('y', [0; 0]), optimset('TolX', 1e-7, 'TolFun', 1e-7));
asserts.isapprox(sol.y, [1; -3], 1e-3);
asserts.istrue(abs(fval) < 1e-6);
asserts.istrue(exitflag > 0);
%=============================================================================
nlLinear = optimvar('nlLinear', 2, 1, 'LowerBound', 0);
nlLinearProb = optimproblem('Objective', log(1 + nlLinear(1) ^ 2) + (nlLinear(2) - 2) ^ 2);
nlLinearProb.Constraints.cap = nlLinear(1) + nlLinear(2) <= 3;
nlLinearStruct = prob2struct(nlLinearProb, struct('nlLinear', [0; 0]));
asserts.isequal(nlLinearStruct.solver, 'fmincon');
asserts.isequal(size(nlLinearStruct.A), [1 2]);
asserts.istrue(isempty(nlLinearStruct.nonlcon));
[nlLinearSol, nlLinearFval, nlLinearExitflag] = solve(nlLinearProb, struct('nlLinear', [0; 0]), ...
  optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp'));
asserts.istrue(nlLinearExitflag > 0);
asserts.istrue(sum(nlLinearSol.nlLinear) <= 3 + 1e-6);
asserts.istrue(nlLinearFval < 1.1);
%=============================================================================
a = optimvar('ingots');
b = optimvar('alloys');
orderProb = optimproblem('Objective', a + b, 'Constraints', a + b >= 1);
asserts.isequal(objectiveVariables(orderProb), {'ingots', 'alloys'});
nestedProb = optimproblem('Objective', a + b);
nestedProb.Constraints.group.lower = a + b >= 1;
nestedProb.Constraints.group.upper = a <= 2;
nestedStruct = prob2struct(nestedProb);
asserts.isequal(nestedStruct.solver, 'linprog');
asserts.isequal(size(nestedStruct.A), [2 2]);
asserts.isequal(nestedStruct.b, [-1; 2]);
%=============================================================================
z = optimvar('z');
constraint = z >= 1;
prob = optimproblem('Objective', (z - 0) .^ 2, 'Constraints', struct('c1', constraint));
[sol, fval, exitflag] = solve(prob, struct('z', -2), optimset('TolX', 1e-7, 'TolFun', 1e-7));
asserts.istrue(sol.z >= 0.99);
asserts.istrue(exitflag > 0);
%=============================================================================
b = optimvar('b', 'LowerBound', 1, 'UpperBound', 3);
prob = optimproblem('Objective', (b - 0) .^ 2);
[sol, fval, exitflag] = solve(prob, struct('b', 0), optimset('TolX', 1e-7, 'TolFun', 1e-7));
asserts.istrue(sol.b >= 0.99);
asserts.istrue(sol.b <= 3.01);
asserts.istrue(exitflag > 0);
%=============================================================================
i = optimvar('i', 'Type', 'integer');
asserts.isequal(i.Type, 'integer');
bin = optimvar('bin', 'Type', 'binary');
asserts.isequal(bin.Type, 'binary');
asserts.isequal(bin.LowerBound, 0);
asserts.isequal(bin.UpperBound, 1);
asserts.checkerror('optimvar(''badType'', ''Type'', ''semiinteger'')', sprintf(_('Invalid value for "%s".'), 'Type'));
asserts.checkerror('optimvar(''badSize'', 0)', _('Optimization variable dimensions must be positive integers.'));
asserts.checkerror('optimvar(''badBound'', 2, 1, ''LowerBound'', [0; 1; 2])', _('Variable bounds must be scalar or match the variable size.'));
asserts.checkerror('optimvar(''badOrder'', ''LowerBound'', 2, ''UpperBound'', 1)', _('Lower bounds must not exceed upper bounds.'));
%=============================================================================
xb = optimvar('xb', 2, 1, 'Type', 'binary');
probBin = optimproblem('Objective', -sum(xb));
binStruct = prob2struct(probBin);
asserts.isequal(binStruct.solver, 'intlinprog');
asserts.isequal(binStruct.lb, [0; 0]);
asserts.isequal(binStruct.ub, [1; 1]);
asserts.isequal(binStruct.intcon, 1:2);
nonlinInt = optimvar('nonlinInt', 'Type', 'integer');
nonlinIntProb = optimproblem('Objective', nonlinInt .^ 2);
asserts.checkerror('prob2struct(nonlinIntProb)', _('Nonlinear problems with integer variables are not supported.'));
%=============================================================================
boundedOnly = optimvar('boundedOnly', 2, 1, 'LowerBound', [1; 2], 'UpperBound', [3; 4]);
freeObj = optimvar('freeObj');
boundProb = optimproblem('Objective', freeObj, 'Constraints', sum(boundedOnly) >= 3);
boundStruct = prob2struct(boundProb);
asserts.isequal(boundStruct.solver, 'linprog');
asserts.isequal(boundStruct.lb(1), -Inf);
asserts.isequal(boundStruct.lb(2:3), [1; 2]);
asserts.isequal(boundStruct.ub(2:3), [3; 4]);
%=============================================================================
maxVar = optimvar('maxVar', 2, 1, 'LowerBound', 0);
maxProb = optimproblem('Objective', 3 * maxVar(1) + maxVar(2), 'ObjectiveSense', 'maximize');
maxProb.Constraints.cap = maxVar(1) + maxVar(2) <= 2;
maxStruct = prob2struct(maxProb);
asserts.isequal(maxStruct.solver, 'linprog');
asserts.isequal(maxStruct.f, [-3; -1]);
[maxSol, maxFval, maxExitflag] = solve(maxProb, struct(), optimoptions('linprog', 'Display', 'off'));
asserts.isapprox(maxSol.maxVar, [2; 0], 1e-9);
asserts.isapprox(maxFval, 6, 1e-9);
asserts.isequal(maxExitflag, 1);
%=============================================================================
infVar = optimvar('infVar', 'LowerBound', 0);
infProb = optimproblem('Objective', infVar);
infProb.Constraints.bad = infVar <= -1;
[infSol, infFval, infExitflag, infOutput] = solve(infProb, struct(), optimoptions('linprog', 'Display', 'off'));
asserts.istrue(isstruct(infSol));
asserts.istrue(isfield(infSol, 'infVar'));
asserts.istrue(isempty(infSol.infVar));
asserts.istrue(isempty(infFval));
asserts.isequal(infExitflag, -2);
asserts.istrue(contains(infOutput.message, 'infeasible'));
%=============================================================================
ingots = optimvar('ingots', 4, 1, 'Type', 'integer', 'LowerBound', 0, 'UpperBound', 1);
alloys = optimvar('alloys', 4, 1, 'LowerBound', 0);
weightIngots = [5, 3, 4, 6];
costIngots = weightIngots .* [350, 330, 310, 280];
costAlloys = [500, 450, 400, 100];
steelprob = optimproblem;
steelprob.Objective = costIngots * ingots + costAlloys * alloys;
totalweight = weightIngots * ingots + sum(alloys);
carbonIngots = [5, 4, 5, 3] / 100;
molybIngots = [3, 3, 4, 4,] / 100;
carbonAlloys = [8, 7, 6, 3] / 100;
molybAlloys = [6, 7, 8, 9] / 100;
totalCarbon = (weightIngots .* carbonIngots) * ingots + carbonAlloys * alloys;
totalMolyb = (weightIngots .* molybIngots) * ingots + molybAlloys * alloys;
steelprob.Constraints.conswt = totalweight == 25;
steelprob.Constraints.conscarb = totalCarbon == 1.25;
steelprob.Constraints.consmolyb = totalMolyb == 1.25;
steelStruct = prob2struct(steelprob);
refAeq = [5 3 4 6 1 1 1 1; ...
  0.25 0.12 0.20 0.18 0.08 0.07 0.06 0.03; ...
  0.15 0.09 0.16 0.24 0.06 0.07 0.08 0.09];
asserts.isequal(steelStruct.solver, 'intlinprog');
asserts.isequal(size(steelStruct.Aeq), [3 8]);
asserts.istrue(nnz(steelStruct.Aeq) == 24);
diffAeq = abs(full(steelStruct.Aeq) - refAeq);
asserts.istrue(max(diffAeq(:)) < 1e-12);
asserts.isequal(steelStruct.beq, [25; 1.25; 1.25]);
asserts.isequal(steelStruct.f, [1750; 990; 1240; 1680; 500; 450; 400; 100]);
asserts.isequal(steelStruct.lb, zeros(8, 1));
asserts.isequal(steelStruct.ub, [1; 1; 1; 1; Inf; Inf; Inf; Inf]);
asserts.isequal(steelStruct.intcon, 1:4);
steelOpts = optimoptions('intlinprog', 'Display', 'off');
[steelSol, steelFval, steelExitflag] = solve(steelprob, struct(), steelOpts);
asserts.isequal(steelSol.ingots, [1; 1; 0; 1]);
asserts.isapprox(steelSol.alloys, [7.25; 0; 0.25; 3.5], 1e-10);
asserts.isapprox(steelFval, 8495, 1e-8);
asserts.isequal(steelExitflag, 1);
%=============================================================================
