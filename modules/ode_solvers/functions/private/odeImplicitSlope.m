%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function yp = odeImplicitSlope(problem, t, y)
  if ~isempty(problem.yp0)
    yp = problem.yp0(:);
  elseif ~isempty(problem.options.InitialSlope)
    yp = problem.options.InitialSlope(:);
  else
    yp = zeros(size(y));
  end
  for k = 1:8
    r = odeCallFcn(problem.odefun, t, y, problem.options, yp);
    if norm(r, inf) < 1e-10
      return
    end
    jac = odeFiniteDifferenceSlopeJacobian(problem, t, y, yp, r);
    delta = jac \ r(:);
    yp = yp - delta;
    if norm(delta, inf) < 1e-10 * (1 + norm(yp, inf))
      return
    end
  end
end
%=============================================================================
