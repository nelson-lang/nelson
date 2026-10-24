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
opts = optimoptions('linprog', 'Display', 'off');
[x, fval, exitflag, output, lambda] = linprog([-1; -1], [1 2; 4 2], [4; 12], ...
  [], [], [0; 0], [], opts);
asserts.isapprox(x, [8 / 3; 2 / 3], 1e-9);
asserts.isapprox(fval, -10 / 3, 1e-9);
asserts.isequal(exitflag, 1);
asserts.isequal(output.algorithm, 'highs');
asserts.istrue(isfield(lambda, 'ineqlin'));
%=============================================================================
