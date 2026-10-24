%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function hnext = odeNextStep(problem, h, err, accepted, acceptedSteps, solverState)
  exponent = problem.errorExponent;
  if nargin >= 6 && strcmp(problem.solver, 'ode15s')
    % The BDF kernel runs at a history-dependent order k: keep the step
    % size controller consistent with the O(h^(k+1)) local error.
    k = odeBDFOrderFromState(problem, solverState);
    exponent = -1 / (k + 1);
  end
  safety = 0.9;
  if strcmp(problem.solver, 'ode89')
    % At order eight a step barely inside the tolerance leaves little
    % headroom; a more conservative growth keeps the global error well
    % below the requested tolerance.
    safety = 0.7;
  end
  if err <= 0
    factor = 5;
  else
    factor = min(5, max(0.2, safety * err ^ exponent));
  end
  factor = odeLimitStepGrowth(problem, factor, accepted, acceptedSteps);
  if ~accepted
    factor = min(factor, 0.5);
  end
  hnext = h * factor;
  minStep = problem.options.MinStep;
  if minStep > 0 && abs(hnext) < minStep
    hnext = problem.direction * minStep;
  end
end
%=============================================================================
function factor = odeLimitStepGrowth(problem, factor, accepted, acceptedSteps)
  if ~accepted || ~strcmp(problem.solver, 'ode15i')
    return
  end
  startupGrowth = [1 2 1 1 2];
  if acceptedSteps <= length(startupGrowth)
    factor = min(factor, startupGrowth(acceptedSteps));
  else
    factor = min(factor, 2);
  end
end
%=============================================================================
