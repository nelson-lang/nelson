%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
n = 20;
H = 2 * eye(n);
f = -(1:n)';
A = ones(1, n);
b = n;
lb = zeros(n, 1);
opts = optimoptions('quadprog', 'ConstraintTolerance', 1e-8);
[x, fval, exitflag, output] = quadprog(H, f, A, b, [], [], lb, [], [], opts);
asserts.istrue(exitflag > 0);
asserts.istrue(output.constrviolation < 1e-7);
asserts.istrue(all(x >= -1e-8));
%=============================================================================
