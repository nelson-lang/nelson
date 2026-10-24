%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function jac = odeFiniteDifferenceSlopeJacobian(problem, t, y, yp, r0)
  n = length(yp);
  jac = zeros(n, n);
  pattern = odeSlopeJacobianPattern(problem, n);
  activeColumns = any(pattern, 1);
  for j = 1:n
    if activeColumns(j)
      step = sqrt(eps) * max(abs(yp(j)), 1);
      trial = yp;
      trial(j) = trial(j) + step;
      r = odeCallFcn(problem.odefun, t, y, problem.options, trial);
      jac(:, j) = (r(:) - r0(:)) / step;
    end
  end
  jac(~pattern) = 0;
end

function pattern = odeSlopeJacobianPattern(problem, n)
  pattern = problem.options.JPattern;
  if isempty(pattern)
    pattern = true(n, n);
  else
    pattern = pattern ~= 0;
  end
end
%=============================================================================
