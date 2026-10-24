%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
C = [1 0; 0 1; 1 1];
d = [1; 2; 3];
[x, resnorm, residual, exitflag, output, lambda] = lsqnonneg(C, d);
asserts.isapprox(x, [1; 2], 1e-7);
asserts.istrue(abs(resnorm) < 1e-10);
asserts.istrue(max(abs(residual)) < 1e-10);
asserts.istrue(exitflag > 0);
asserts.istrue(isstruct(output));
asserts.isequal(size(lambda), [2 1]);
%=============================================================================
