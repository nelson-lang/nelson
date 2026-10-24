%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, resnorm, residual, exitflag, output, lambda, jacobian] = lsqnonlin(varargin)
  [fun, x0, lb, ub, A, b, Aeq, beq, nonlcon, options] = parseInputs(varargin{:});
  if isempty(x0)
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Initial point');
  end
  if ~isnumeric(x0) || ~isreal(x0)
    error('Nelson:optimization:invalidInitialPoint', '%s must be a real numeric array.', 'Initial point');
  end
  opts = optimMergeOptions('lsqnonlin', options);
  xShape = size(x0);
  x = double(x0(:));
  n = length(x);
  lb = normalizeBound(lb, n, -Inf);
  ub = normalizeBound(ub, n, Inf);
  columnFun = fun;
  if ~isequal(xShape, [n, 1])
    columnFun = @(z) optimCall(fun, reshape(z, xShape));
  end
  hasConstraints = ~isempty(A) || ~isempty(Aeq) || ~isempty(nonlcon);
  algorithm = validateAlgorithm(opts, hasConstraints);
  validateDisplay(opts);
  if any(lb > ub)
    [x, resnorm, residual, exitflag, output, lambda, jacobian] = ...
      infeasibleResult(columnFun, x, xShape, n, algorithm, opts);
    return
  end
  ctx = struct();
  ctx.maxIter = opts.MaxIter;
  ctx.maxFunEvals = optimResolveMaxFunEvals(opts, n);
  opts.MaxFunEvals = ctx.maxFunEvals;
  opts.MaxFunctionEvaluations = ctx.maxFunEvals;
  if strcmp(algorithm, 'interior-point')
    [x, resnorm, residual, exitflag, output, lambda, jacobian] = ...
      solveInteriorPoint(fun, columnFun, x, xShape, lb, ub, A, b, Aeq, beq, ...
      nonlcon, options, opts);
    return
  end
  % Probe the residual once to keep its user shape on output.
  initialResidual = double(optimCall(columnFun, x));
  fShape = size(initialResidual);
  ctx.initialResidual = initialResidual(:);
  if strcmp(algorithm, 'trust-region-reflective')
    result = optimLsqTrustRegion(columnFun, x, lb, ub, opts, ctx);
    style = 'trr';
  else
    result = optimLsqLevenbergMarquardt(columnFun, x, lb, ub, opts, ctx);
    style = 'lm';
  end
  [exitflag, messageKind, info] = mapExitFlag(result, opts);
  [message, messageDetailed] = optimLsqExitMessage('lsqnonlin', messageKind, info, opts);
  x = reshape(result.x, xShape);
  resnorm = result.resnorm;
  residual = reshape(result.residual, fShape);
  jacobian = result.J;
  lambda = boundMultipliers(result.x, lb, ub, jacobian, result.residual, opts);
  output = solverOutput(style, algorithm, result, messageDetailed);
  optimLsqDisplay(opts, 'final', style, struct('message', message, ...
    'messageDetailed', messageDetailed, 'exitflag', exitflag));
end
%=============================================================================
function [fun, x0, lb, ub, A, b, Aeq, beq, nonlcon, options] = parseInputs(varargin)
  if nargin == 1 && isstruct(varargin{1})
    [fun, x0, lb, ub, A, b, Aeq, beq, nonlcon, options] = parseProblem(varargin{1});
    return
  end
  if nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin > 10
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  if nargin == 5 && ~(isstruct(varargin{5}) || isobject(varargin{5}) || isempty(varargin{5}))
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  padded = varargin;
  padded(end + 1:10) = {[]};
  if nargin == 5
    padded{10} = padded{5};
    padded{5} = [];
  end
  [fun, x0, lb, ub, A, b, Aeq, beq, nonlcon, options] = deal(padded{:});
end
%=============================================================================
function [fun, x0, lb, ub, A, b, Aeq, beq, nonlcon, options] = parseProblem(problem)
  if ~isfield(problem, 'objective') || ~isfield(problem, 'x0') || ...
      ~ isfield(problem, 'solver') || ~isfield(problem, 'options')
    error('Nelson:optimization:invalidProblemStructure', 'Problem structure must contain %s fields.', 'solver and options');
  end
  fun = problem.objective;
  x0 = problem.x0;
  lb = getFieldOrEmpty(problem, 'lb');
  ub = getFieldOrEmpty(problem, 'ub');
  A = getFieldOrEmpty(problem, 'Aineq');
  b = getFieldOrEmpty(problem, 'bineq');
  Aeq = getFieldOrEmpty(problem, 'Aeq');
  beq = getFieldOrEmpty(problem, 'beq');
  nonlcon = getFieldOrEmpty(problem, 'nonlcon');
  options = problem.options;
end
%=============================================================================
function value = getFieldOrEmpty(st, name)
  if isfield(st, name)
    value = st.(name);
  else
    value = [];
  end
end
%=============================================================================
function bound = normalizeBound(bound, n, defaultValue)
  if isempty(bound)
    bound = defaultValue * ones(n, 1);
  elseif isscalar(bound)
    bound = double(bound) * ones(n, 1);
  else
    bound = double(bound(:));
  end
  if length(bound) ~= n
    error('Nelson:dimagree:boundsNumberElementsX0', 'Bounds must have the same number of elements as x0.');
  end
end
%=============================================================================
function algorithm = validateAlgorithm(opts, hasConstraints)
  algorithm = lower(char(opts.Algorithm));
  valid = {'trust-region-reflective', 'levenberg-marquardt', 'interior-point'};
  if ~any(strcmp(algorithm, valid))
    error('Nelson:optim:invalidOptionValue', 'Invalid value for "%s".', 'Algorithm');
  end
  if hasConstraints && ~strcmp(algorithm, 'interior-point')
    % Linear and nonlinear constraints are only supported by interior-point.
    algorithm = 'interior-point';
  end
end
%=============================================================================
function validateDisplay(opts)
  value = lower(char(opts.Display));
  valid = {'off', 'none', 'final', 'final-detailed', 'notify', ...
    'notify-detailed', 'iter', 'iter-detailed'};
  if ~any(strcmp(value, valid))
    error('Nelson:optim:invalidOptionValue', 'Invalid value for "%s".', 'Display');
  end
end
%=============================================================================
function [x, resnorm, residual, exitflag, output, lambda, jacobian] = ...
    infeasibleResult(columnFun, x, xShape, n, algorithm, opts)
  exitflag = -2;
  shapedResidual = double(optimCall(columnFun, x));
  [jacobian, residual] = optimFiniteDiffJacobian(columnFun, x, opts, shapedResidual(:));
  resnorm = residual' * residual;
  residual = reshape(residual, size(shapedResidual));
  [message, messageDetailed] = optimLsqExitMessage('lsqnonlin', 'infeasible', ...
    struct('value', 0, 'tolerance', 0), opts);
  lambda = emptyLambda(n);
  output = struct();
  output.firstorderopt = [];
  output.iterations = 0;
  output.funcCount = 0;
  output.cgiterations = [];
  output.algorithm = algorithm;
  output.stepsize = [];
  output.message = messageDetailed;
  output.bestfeasible = [];
  output.constrviolation = [];
  x = reshape(x, xShape);
  optimLsqDisplay(opts, 'final', 'trr', struct('message', message, ...
    'messageDetailed', messageDetailed, 'exitflag', exitflag));
end
%=============================================================================
function lambda = emptyLambda(n)
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lambda.eqlin = [];
  lambda.ineqlin = [];
  lambda.eqnonlin = [];
  lambda.ineqnonlin = [];
end
%=============================================================================
function [exitflag, messageKind, info] = mapExitFlag(result, opts)
  kind = result.stop.kind;
  messageKind = kind;
  info = result.stop;
  switch kind
    case 'optimality'
      exitflag = 1;
    case 'optimality-relative'
      exitflag = 1;
    case 'stationary'
      exitflag = 1;
      messageKind = 'optimality';
      info = struct('kind', 'optimality', 'value', result.firstorderopt, ...
        'tolerance', opts.OptimalityTolerance);
    case 'step'
      exitflag = 2;
    case 'funchange'
      exitflag = 3;
    case {'direction', 'regularization'}
      exitflag = 4;
      messageKind = 'direction';
      info = struct('kind', 'direction', 'value', result.stop.value, ...
        'tolerance', opts.StepTolerance);
    case 'outputfcn'
      exitflag = -1;
    otherwise
      % 'maxiter' or 'maxfeval'
      exitflag = 0;
  end
end
%=============================================================================
function lambda = boundMultipliers(x, lb, ub, jacobian, residual, opts)
  n = length(x);
  lambda = emptyLambda(n);
  gradient = 2 * (jacobian' * residual);
  tolerance = sqrt(eps) * max(1, abs(x));
  atLower = isfinite(lb) & ((x - lb) <= tolerance);
  atUpper = isfinite(ub) & ((ub - x) <= tolerance);
  lambda.lower(atLower) = max(0, gradient(atLower));
  lambda.upper(atUpper) = max(0, -gradient(atUpper));
end
%=============================================================================
function output = solverOutput(style, algorithm, result, messageDetailed)
  output = struct();
  if strcmp(style, 'trr')
    output.firstorderopt = result.firstorderopt;
    output.iterations = result.iterations;
    output.funcCount = result.funcCount;
    output.cgiterations = result.cgiterations;
    output.algorithm = algorithm;
    output.stepsize = result.stepsize;
  else
    output.iterations = result.iterations;
    output.funcCount = result.funcCount;
    output.stepsize = result.stepsize;
    output.cgiterations = result.cgiterations;
    output.firstorderopt = result.firstorderopt;
    output.algorithm = algorithm;
  end
  output.message = messageDetailed;
  output.bestfeasible = [];
  output.constrviolation = [];
end
%=============================================================================
function [x, resnorm, residual, exitflag, output, lambda, jacobian] = ...
    solveInteriorPoint(fun, columnFun, x0, xShape, lb, ub, A, b, Aeq, beq, ...
    nonlcon, options, lsqOpts)
  % Constrained (or explicitly requested interior-point) problems reuse the
  % fmincon machinery on the smooth objective sum(F(x).^2).
  ipOpts = optimMergeOptions('fmincon', options);
  ipOpts.Algorithm = 'interior-point';
  ipOpts.SpecifyObjectiveGradient = true;
  ipOpts.Display = 'off';
  ipOpts.MaxFunEvals = optimResolveMaxFunEvals(ipOpts, length(x0));
  ipOpts.MaxFunctionEvaluations = ipOpts.MaxFunEvals;
  objective = @(z) squaredResidualObjective(columnFun, z, lsqOpts);
  wrappedNonlcon = nonlcon;
  if ~isempty(nonlcon) && ~isequal(xShape, [length(x0), 1])
    wrappedNonlcon = @(z) optimCall(nonlcon, reshape(z, xShape));
  end
  data = optimFminconPrepare(objective, x0, A, b, Aeq, beq, lb, ub, ...
    wrappedNonlcon, ipOpts);
  data.xShape = xShape;
  result = optimFminconSQP(data, ipOpts);
  x = reshape(result.x, xShape);
  shapedResidual = double(optimCall(columnFun, result.x));
  [jacobian, residual] = optimFiniteDiffJacobian(columnFun, result.x, lsqOpts, shapedResidual(:));
  resnorm = residual' * residual;
  residual = reshape(residual, size(shapedResidual));
  exitflag = result.exitflag;
  lambda = result.lambda;
  output = struct();
  output.iterations = result.output.iterations;
  output.funcCount = result.output.funcCount;
  output.stepsize = result.output.stepsize;
  output.cgiterations = result.output.cgiterations;
  output.firstorderopt = result.output.firstorderopt;
  output.algorithm = 'interior-point';
  output.message = result.output.message;
  output.bestfeasible = result.output.bestfeasible;
  output.constrviolation = result.output.constrviolation;
  values = struct('message', result.output.message, ...
    'messageDetailed', result.output.message, 'exitflag', exitflag);
  optimLsqDisplay(lsqOpts, 'final', 'trr', values);
end
%=============================================================================
function [f, g] = squaredResidualObjective(columnFun, z, lsqOpts)
  [J, r] = optimFiniteDiffJacobian(columnFun, z, lsqOpts);
  f = r' * r;
  g = 2 * (J' * r);
end
%=============================================================================
