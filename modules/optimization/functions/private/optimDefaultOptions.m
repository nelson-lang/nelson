%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = optimDefaultOptions(solver)
  opts = struct();
  opts.Display = 'notify';
  opts.MaxFunEvals = [];
  opts.MaxFunctionEvaluations = [];
  opts.MaxIter = [];
  opts.MaxIterations = [];
  opts.TolX = 1e-4;
  opts.TolFun = 1e-4;
  opts.FunctionTolerance = [];
  opts.StepTolerance = [];
  opts.OptimalityTolerance = 1e-6;
  opts.ConstraintTolerance = 1e-6;
  opts.FiniteDifferenceType = 'forward';
  opts.FiniteDifferenceStepSize = [];
  opts.TypicalX = [];
  opts.FunValCheck = 'off';
  opts.OutputFcn = [];
  opts.PlotFcns = [];
  opts.Jacobian = 'off';
  opts.GradObj = 'off';
  opts.GradConstr = 'off';
  opts.HessianFcn = [];
  opts.HessianMultiplyFcn = [];
  opts.SpecifyObjectiveGradient = false;
  opts.SpecifyConstraintGradient = false;
  opts.Algorithm = defaultAlgorithm(solver);
  opts.BarrierParamUpdate = 'monotone';
  opts.EnableFeasibilityMode = false;
  opts.Hessian = [];
  opts.HessianApproximation = 'bfgs';
  opts.HessUpdate = 'bfgs';
  opts.SubproblemAlgorithm = 'cg';
  opts.ScaleProblem = 'none';
  opts.Diagnostics = 'off';
  opts.HonorBounds = true;
  opts.UseParallel = false;
  opts.UseCodegenSolver = false;
  opts.HessFcn = [];
  opts.HessMult = [];
  opts.MaxTime = Inf;
  opts.MaxNodes = [];
  opts.MaxFeasiblePoints = [];
  opts.MaxNumFeasPoints = [];
  opts.AbsoluteGapTolerance = 1e-6;
  opts.RelativeGapTolerance = 1e-4;
  opts.TolGapAbs = [];
  opts.TolGapRel = [];
  opts.IntegerTolerance = 1e-6;
  opts.TolInteger = [];
  opts.LPMaxIterations = [];
  opts.LPOptimalityTolerance = 1e-7;
  opts.LPPreprocess = 'basic';
  opts.NodeSelection = 'simplebestproj';
  opts.RootLPAlgorithm = 'dual-simplex';
  opts.Heuristics = 'basic';
  opts.CutGeneration = 'basic';
  opts.ObjectiveImprovementThreshold = 0;
  opts.MaxPCGIter = [];
  opts.MaxProjCGIter = [];
  opts.PrecondBandWidth = 0;
  opts.TolPCG = 0.1;
  opts.TolConSQP = 1e-6;
  opts.TolProjCG = 0.01;
  opts.TolProjCGAbs = 1e-10;
  switch lower(solver)
    case {'fminsearch', 'fminbnd', 'fzero'}
      opts.MaxFunEvals = 500;
      opts.MaxIter = 500;
      opts.FunctionTolerance = opts.TolFun;
      opts.StepTolerance = opts.TolX;
    case 'lsqnonneg'
      opts.MaxIter = [];
      opts.TolX = 10 * eps;
      opts.Display = 'off';
    case 'quadprog'
      opts.MaxIterations = 200;
      opts.MaxIter = 200;
      opts.StepTolerance = 1e-6;
      opts.Display = 'final';
    case 'fmincon'
      opts.Algorithm = 'interior-point';
      % Algorithm-dependent defaults (MaxIterations, MaxFunctionEvaluations,
      % StepTolerance, SubproblemAlgorithm) stay empty here and are resolved
      % against the selected algorithm by optimResolveAlgorithmDefaults.
      opts.MaxIterations = [];
      opts.MaxIter = [];
      opts.MaxFunctionEvaluations = [];
      opts.MaxFunEvals = [];
      opts.FunctionTolerance = 1e-6;
      opts.TolFun = 1e-6;
      opts.StepTolerance = [];
      opts.TolX = [];
      opts.SubproblemAlgorithm = [];
      opts.ObjectiveLimit = -1e20;
      opts.Display = 'final';
    case {'fsolve', 'lsqnonlin'}
      opts.MaxIterations = 400;
      opts.MaxIter = 400;
      opts.MaxFunctionEvaluations = '100*numberOfVariables';
      opts.MaxFunEvals = '100*numberOfVariables';
      opts.FunctionTolerance = 1e-6;
      opts.TolFun = 1e-6;
      opts.StepTolerance = 1e-6;
      opts.TolX = 1e-6;
      opts.InitDamping = 0.01;
      opts.Display = 'final';
    case 'fminunc'
      opts.Algorithm = 'quasi-newton';
      opts.MaxIterations = 400;
      opts.MaxIter = 400;
      opts.MaxFunctionEvaluations = 1000;
      opts.MaxFunEvals = 1000;
      opts.FunctionTolerance = 1e-6;
      opts.TolFun = 1e-6;
      opts.StepTolerance = 1e-6;
      opts.TolX = 1e-6;
      opts.ObjectiveLimit = -1e20;
      opts.Display = 'final';
    case 'intlinprog'
      opts.Algorithm = 'highs';
      opts.MaxNodes = 1e4;
      opts.MaxNumFeasPoints = Inf;
      opts.MaxFeasiblePoints = Inf;
      opts.MaxIterations = 30000;
      opts.MaxIter = 30000;
      opts.Display = 'final';
    case 'linprog'
      opts.Algorithm = 'highs';
      opts.MaxIterations = 30000;
      opts.MaxIter = 30000;
      opts.Display = 'final';
    case 'default'
      % optimset (name/value form, no solver): every option the
      % caller did not set is left empty ([]). Solvers re-merge this structure
      % against their own defaults (see optimMergeOptions), so empty fields fall
      % back to the correct solver default instead of leaking a placeholder value
      % (e.g. Algorithm 'default' or TolX 1e-4) that would override it.
      opts = blankOptionValues(opts);
    otherwise
      opts.MaxIterations = 400;
      opts.MaxFunctionEvaluations = 1000;
      opts.MaxFunEvals = 1000;
      opts.FunctionTolerance = 1e-6;
      opts.StepTolerance = 1e-6;
      opts.Display = 'final';
  end
end
%=============================================================================
function opts = blankOptionValues(opts)
  % Reset every option value to empty while preserving the field set;
  % optimset leaves unset options as [].
  names = fieldnames(opts);
  for k = 1:numel(names)
    opts.(names{k}) = [];
  end
end
%=============================================================================
function algorithm = defaultAlgorithm(solver)
  switch lower(solver)
    case 'fminsearch'
      algorithm = 'nelder-mead';
    case 'fminbnd'
      algorithm = 'golden-section';
    case 'fzero'
      algorithm = 'brent';
    case 'lsqnonneg'
      algorithm = 'active-set';
    case 'quadprog'
      algorithm = 'active-set';
    case 'fmincon'
      algorithm = 'interior-point';
    case 'fminunc'
      algorithm = 'quasi-newton';
    case {'intlinprog', 'linprog'}
      algorithm = 'highs';
    case 'lsqnonlin'
      algorithm = 'trust-region-reflective';
    case 'fsolve'
      algorithm = 'trust-region-dogleg';
    otherwise
      algorithm = 'default';
  end
end
%=============================================================================
