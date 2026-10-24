%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
H = 2 * eye(2);
f = [-2; -4];
[x, fval, exitflag, output, lambda] = quadprog(H, f, [], [], [], [], [0; 0], [10; 10]);
asserts.isapprox(x, [1; 2], 1e-7);
asserts.isapprox(fval, -5, 1e-7);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
asserts.istrue(isstruct(lambda));
%=============================================================================
H = [2 0; 0 2];
f = [-2; -4];
A = [1 1];
b = 2;
lb = [0; 0];
[x, fval, exitflag, output, lambda] = quadprog(H, f, A, b, [], [], lb, []);
asserts.isapprox(x(:), [0.5; 1.5], 1e-5);
asserts.istrue(exitflag > 0);
asserts.istrue(output.constrviolation < 1e-8);
asserts.istrue(isfield(lambda, 'ineqlin'));
asserts.istrue(isfield(lambda, 'lower'));
asserts.istrue(isfield(lambda, 'upper'));
%=============================================================================
problem = struct();
problem.H = H;
problem.f = f;
problem.Aineq = A;
problem.bineq = b;
problem.lb = lb;
problem.options = optimoptions('quadprog', 'ConstraintTolerance', 1e-8);
[x, fval, exitflag] = quadprog(problem);
asserts.isapprox(x(:), [0.5; 1.5], 1e-5);
asserts.istrue(exitflag > 0);
%=============================================================================
problem = struct();
problem.H = H;
problem.f = f;
problem.A = A;
problem.b = b;
problem.lb = lb;
[x, fval, exitflag] = quadprog(problem);
asserts.isapprox(x(:), [0.5; 1.5], 1e-5);
asserts.istrue(exitflag > 0);
%=============================================================================
