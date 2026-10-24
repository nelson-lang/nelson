%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = resolveAlgorithmDefaults(solver, opts)
  % Fill algorithm-dependent fmincon defaults that the user did not set.
  if ~strcmpi(solver, 'fmincon')
    return
  end
  isInteriorPoint = strcmpi(char(opts.Algorithm), 'interior-point');
  if isempty(opts.MaxIterations) && isempty(opts.MaxIter)
    if isInteriorPoint
      value = 1000;
    else
      value = 400;
    end
    opts.MaxIterations = value;
    opts.MaxIter = value;
  end
  if isempty(opts.MaxFunctionEvaluations) && isempty(opts.MaxFunEvals)
    if isInteriorPoint
      value = 3000;
    else
      value = '100*numberOfVariables';
    end
    opts.MaxFunctionEvaluations = value;
    opts.MaxFunEvals = value;
  end
  if isempty(opts.StepTolerance) && isempty(opts.TolX)
    if isInteriorPoint
      value = 1e-10;
    else
      value = 1e-6;
    end
    opts.StepTolerance = value;
    opts.TolX = value;
  end
  if isempty(opts.SubproblemAlgorithm)
    if isInteriorPoint
      opts.SubproblemAlgorithm = 'factorization';
    else
      opts.SubproblemAlgorithm = 'cg';
    end
  end
end
%=============================================================================
