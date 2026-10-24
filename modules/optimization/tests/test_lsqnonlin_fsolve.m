%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[x, resnorm, residual, exitflag, output, lambda, jacobian] = lsqnonlin(@(x)[x(1) - 2; x(2) + 1], [0; 0]);
asserts.isapprox(x(:), [2; -1], 1e-5);
asserts.istrue(abs(resnorm) < 1e-9);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
asserts.istrue(isstruct(lambda));
asserts.isequal(size(jacobian), [2 2]);
%=============================================================================
[root, fval, exitflag, output, jacobian] = fsolve(@(x)[x(1) - 3; x(2) + 4], [0; 0]);
asserts.isapprox(root(:), [3; -4], 1e-5);
asserts.istrue(norm(fval) < 1e-7);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
asserts.isequal(size(jacobian), [2 2]);
%=============================================================================
