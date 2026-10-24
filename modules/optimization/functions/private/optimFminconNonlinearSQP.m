%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = optimFminconNonlinearSQP(data, opts, coreSolver)
  trialOpts = quietOptions(opts);
  sqpResult = coreSolver(data, trialOpts);
  if acceptSQP(sqpResult, opts)
    result = sqpResult;
    result.output.algorithm = data.algorithm;
    return
  end
  penaltyResult = optimFminconPenaltySearch(data, opts);
  polishedResult = polishPenaltyPoint(data, opts, coreSolver, penaltyResult);
  result = chooseNonlinearResult(sqpResult, chooseNonlinearResult(penaltyResult, polishedResult, opts), opts);
end
%=============================================================================
function opts = quietOptions(opts)
  opts.Display = 'off';
  opts.OutputFcn = [];
  opts.MaxIter = min(opts.MaxIter, 80);
  opts.MaxIterations = opts.MaxIter;
  opts.MaxFunEvals = min(opts.MaxFunEvals, 1200);
  opts.MaxFunctionEvaluations = opts.MaxFunEvals;
end
%=============================================================================
function tf = acceptSQP(result, opts)
  feasible = result.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5);
  stationary = result.output.firstorderopt <= max(1e-3, 20 * sqrt(opts.OptimalityTolerance));
  tf = result.exitflag > 0 && feasible && stationary;
end
%=============================================================================
function result = polishPenaltyPoint(data, opts, coreSolver, penaltyResult)
  if isempty(penaltyResult.x)
    result = penaltyResult;
    return
  end
  localData = data;
  localData.x0 = penaltyResult.x;
  localOpts = quietOptions(opts);
  localOpts.MaxIter = min(opts.MaxIter, 60);
  localOpts.MaxIterations = localOpts.MaxIter;
  result = coreSolver(localData, localOpts);
  result.output.algorithm = data.algorithm;
end
%=============================================================================
function result = chooseNonlinearResult(sqpResult, penaltyResult, opts)
  sqpFeasible = sqpResult.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5);
  penaltyFeasible = penaltyResult.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5);
  if sqpFeasible && (~penaltyFeasible || sqpResult.fval <= penaltyResult.fval)
    result = sqpResult;
  else
    result = penaltyResult;
  end
  if result.exitflag == 0 && result.output.constrviolation <= max(opts.ConstraintTolerance, 1e-5)
    result.exitflag = 1;
    result.output.message = _('Local minimum found that satisfies the constraints.');
  end
end
%=============================================================================
