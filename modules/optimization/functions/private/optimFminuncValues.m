%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function values = optimFminuncValues(state, iter, funcCount, stepSize)
  values = struct();
  values.fval = state.f;
  values.iteration = iter;
  values.funccount = funcCount;
  values.stepsize = stepSize;
  values.firstorderopt = norm(state.grad, Inf);
end
%=============================================================================
