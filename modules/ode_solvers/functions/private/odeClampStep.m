%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = odeClampStep(problem, t, h)
  hmax = problem.options.MaxStep;
  if abs(h) > hmax
    h = problem.direction * hmax;
  end
  remaining = problem.tfinal - t;
  if abs(h) > abs(remaining)
    h = remaining;
  end
end
%=============================================================================
