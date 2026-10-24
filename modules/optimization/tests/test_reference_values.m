%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opts = optimset('Display', 'off', 'TolX', 1e-10, 'TolFun', 1e-10);
%=============================================================================
[x, fval, exitflag, output] = fminbnd(@(x)(x - 1.5) .^ 2 + 0.25, -2, 4, opts);
asserts.isapprox(x, 1.5, 1e-3);
asserts.isapprox(fval, 0.25, 1e-9);
asserts.istrue(exitflag > 0);
asserts.istrue(output.funcCount > 0);
%=============================================================================
[x, fval, exitflag, output] = fzero(@(x) x .^ 2 - 4, [0 5], opts);
asserts.isapprox(x, 2, 1e-8);
asserts.istrue(abs(fval) < 1e-8);
asserts.istrue(exitflag > 0);
asserts.isequal(output.algorithm, 'Brent-Dekker zero search');
%=============================================================================
C = [1 0; 0 1; 1 1];
d = [1; 2; 3];
[x, resnorm, residual, exitflag, output, lambda] = lsqnonneg(C, d, opts);
asserts.isapprox(x(:), [1; 2], 1e-9);
asserts.istrue(abs(resnorm) < 1e-10);
asserts.istrue(max(abs(residual(:))) < 1e-10);
asserts.istrue(exitflag > 0);
asserts.isequal(size(lambda), [2 1]);
asserts.istrue(isfield(output, 'iterations'));
%=============================================================================
H = 2 * eye(2);
f = [-2; -4];
[x, fval, exitflag, output, lambda] = quadprog(H, f, [], [], [], [], [0; 0], [10; 10], [], optimoptions('quadprog', 'Display', 'off'));
asserts.isapprox(x(:), [1; 2], 1e-9);
asserts.isapprox(fval, -5, 1e-9);
asserts.istrue(exitflag > 0);
asserts.istrue(output.constrviolation < 1e-10);
asserts.isequal(size(lambda.lower), [2 1]);
asserts.isequal(size(lambda.upper), [2 1]);
%=============================================================================
[x, fval, exitflag, output] = fminsearch(@(x)(x(1) - 1) .^ 2 + (x(2) + 2) .^ 2, [0; 0], opts);
asserts.isapprox(x(:), [1; -2], 1e-3);
asserts.isapprox(fval, 0, 1e-6, 1e-8);
asserts.istrue(exitflag > 0);
asserts.istrue(output.funcCount > 0);
%=============================================================================
