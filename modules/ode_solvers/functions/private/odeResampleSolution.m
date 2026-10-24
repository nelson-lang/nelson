%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeResampleSolution(sol, tspan)
  if length(tspan) <= 2
    refine = sol.extdata.options.Refine;
    if isempty(refine) || refine <= 1
      return
    end
    tspan = odeRefinedTimeVector(sol.x, refine);
  else
    tspan = odeReachedTimeVector(sol.x, tspan);
  end
  if length(tspan) < 2
    return
  end
  internal = sol;
  values = zeros(size(sol.y, 1), length(tspan));
  slopes = values;
  internalSlopes = [];
  if isfield(sol, 'extdata') && isfield(sol.extdata, 'slopes')
    internalSlopes = sol.extdata.slopes;
  end
  [values, slopes] = odeDevalInterpolate(sol.x, sol.y, tspan(:).', internalSlopes);
  values = odeApplyResampledNonNegative(sol.extdata.options, values);
  sol.x = tspan(:).';
  sol.y = values;
  sol.extdata.internal = internal;
  sol.extdata.slopes = slopes;
end

function output = odeReachedTimeVector(x, tspan)
  direction = sign(x(length(x)) - x(1));
  if direction == 0
    output = x(1);
    return
  end
  finalTime = x(length(x));
  keep = direction * (finalTime - tspan) >= -eps * (1 + abs(finalTime));
  output = tspan(keep);
  if isempty(output) || abs(output(length(output)) - finalTime) > eps * (1 + abs(finalTime))
    output(end + 1) = finalTime;
  end
end

function refined = odeRefinedTimeVector(x, refine)
  refined = x(1);
  for k = 1:(length(x) - 1)
    for j = 1:refine
      refined(end + 1) = x(k) + (j / refine) * (x(k + 1) - x(k));
    end
  end
end

function values = odeApplyResampledNonNegative(options, values)
  idx = options.NonNegative;
  if isempty(idx)
    return
  end
  values(idx(:), :) = max(values(idx(:), :), 0);
end
%=============================================================================
