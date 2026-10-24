%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[x, fval, exitflag, output] = fminbnd(@(x)(x - 1.5) .^ 2, -2, 4, optimset('TolX', 1e-8));
asserts.isapprox(x, 1.5, 1e-3);
asserts.isapprox(fval, 0, 1e-6);
asserts.istrue(exitflag > 0);
asserts.istrue(isfield(output, 'funcCount'));
%=============================================================================
