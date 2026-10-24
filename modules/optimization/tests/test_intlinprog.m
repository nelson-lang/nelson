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
[x, fval, exitflag, output] = intlinprog([8; 1], 2, [-1 -2; -4 -1; 2 1], ...
  [14; -33; 20], [], [], [], [], [], opts);
asserts.isapprox(x, [6.5; 7], 1e-10);
asserts.isapprox(fval, 59, 1e-10);
asserts.isequal(exitflag, 1);
asserts.isequal(output.algorithm, 'highs');
%=============================================================================
