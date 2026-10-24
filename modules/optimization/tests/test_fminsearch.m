%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = @(x)(x(1) - 2) .^ 2 + (x(2) + 3) .^ 2;
[x, fval, exitflag, output] = fminsearch(f, [0; 0], optimset('Display', 'off', 'TolX', 1e-7, 'TolFun', 1e-7));
asserts.isapprox(x(:), [2; -3], 1e-3);
asserts.isapprox(fval, 0, 1e-6, 1e-8);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
%=============================================================================
