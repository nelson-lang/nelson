%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function odeCheckSolverForTest(solver)
  [t, y] = solver(@(tt, yy) - yy, [0 1], 1, odeset('MaxStep', 0.1));
  asserts.istrue(abs(t(length(t)) - 1) < 1e-12);
  asserts.istrue(abs(y(size(y, 1), 1) - exp(-1)) < 1e-3);
end
%=============================================================================
