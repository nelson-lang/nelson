%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function output = optimFminuncOutput(iter, funcCount, state, algorithm, message, stepSize)
  output.iterations = iter;
  output.funcCount = funcCount;
  output.stepsize = stepSize;
  output.algorithm = algorithm;
  output.firstorderopt = norm(state.grad, Inf);
  output.message = message;
end
%=============================================================================
