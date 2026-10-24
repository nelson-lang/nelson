%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
opts = optimoptions('fsolve', 'Jacobian', 'on', 'FunctionTolerance', 1e-10);
[root, fval, exitflag, output, jacobian] = fsolve(@optimTestResidualWithJacobian, [0; 0], opts);
asserts.istrue(max(abs(root(:) - [4; -3])) < 1e-7);
asserts.istrue(norm(fval) < 1e-8);
asserts.istrue(exitflag > 0);
asserts.isequal(jacobian, [1 0; 0 2]);
asserts.istrue(isfield(output, 'firstorderopt'));
%=============================================================================
problem = struct();
problem.objective = @optimTestResidualWithJacobian;
problem.x0 = [0; 0];
problem.solver = 'fsolve';
problem.options = opts;
[root, fval] = fsolve(problem);
asserts.istrue(max(abs(root(:) - [4; -3])) < 1e-7);
asserts.istrue(norm(fval) < 1e-8);
%=============================================================================
opts = optimoptions('lsqnonlin', 'Jacobian', 'on', 'StepTolerance', 1e-10);
[x, resnorm, residual, exitflag, output, lambda, jacobian] = lsqnonlin(@optimTestResidualWithJacobian, [0; 0], [3; -4], [5; -2], opts);
asserts.istrue(max(abs(x(:) - [4; -3])) < 1e-6);
asserts.istrue(abs(resnorm) < 1e-8);
asserts.istrue(exitflag > 0);
asserts.isequal(jacobian, [1 0; 0 2]);
asserts.istrue(isfield(lambda, 'lower'));
asserts.istrue(isfield(lambda, 'upper'));
%=============================================================================
problem = struct();
problem.objective = @optimTestResidualWithJacobian;
problem.x0 = [0; 0];
problem.lb = [3; -4];
problem.ub = [5; -2];
problem.solver = 'lsqnonlin';
problem.options = opts;
[x, resnorm] = lsqnonlin(problem);
asserts.istrue(max(abs(x(:) - [4; -3])) < 1e-6);
asserts.istrue(abs(resnorm) < 1e-8);
%=============================================================================
