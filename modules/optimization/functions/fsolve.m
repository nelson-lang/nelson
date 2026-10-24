%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, fval, exitflag, output, jacobian] = fsolve(fun, x0, options)
  fromProblem = false;
  if nargin == 1 && isstruct(fun)
    problem = fun;
    if ~isfield(problem, 'objective') || ~isfield(problem, 'x0') || ...
        ~ isfield(problem, 'solver') || ~isfield(problem, 'options')
      error('Nelson:optimization:invalidProblemStructure', 'Problem structure must contain %s fields.', 'solver and options');
    end
    fun = problem.objective;
    x0 = problem.x0;
    options = problem.options;
    fromProblem = true;
  end
  if ~fromProblem && nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if ~fromProblem && nargin > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  if nargin < 3 && ~fromProblem
    options = [];
  end
  if isempty(x0)
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Initial point');
  end
  if ~isnumeric(x0) || ~isreal(x0)
    error('Nelson:optimization:invalidInitialPoint', '%s must be a real numeric array.', 'Initial point');
  end
  opts = optimMergeOptions('fsolve', options);
  algorithm = validateAlgorithm(opts);
  validateDisplay(opts);
  xShape = size(x0);
  x = double(x0(:));
  n = length(x);
  ctx = struct();
  ctx.maxIter = opts.MaxIter;
  ctx.maxFunEvals = optimResolveMaxFunEvals(opts, n);
  opts.MaxFunEvals = ctx.maxFunEvals;
  opts.MaxFunctionEvaluations = ctx.maxFunEvals;
  % Engines work on column vectors; the user function receives x with the
  % shape of x0.
  columnFun = fun;
  if ~isequal(xShape, [n, 1])
    columnFun = @(z) optimCall(fun, reshape(z, xShape));
  end
  % Probe the residual once: needed for the output shape and to detect
  % non-square systems before choosing the engine.
  initialResidual = double(optimCall(columnFun, x));
  if ~isnumeric(initialResidual)
    error('Nelson:fsolve:invalidObjectiveValue', 'Objective must return a numeric array.');
  end
  fShape = size(initialResidual);
  m = numel(initialResidual);
  if strcmp(algorithm, 'trust-region-dogleg') && m ~= n
    warning('Nelson:fsolve:nonSquareSystem', 'Trust-region-dogleg algorithm of fsolve cannot handle non-square systems; using Levenberg-Marquardt algorithm instead.');
    algorithm = 'levenberg-marquardt';
  end
  ctx.initialResidual = initialResidual(:);
  unbounded = Inf(n, 1);
  switch algorithm
    case 'trust-region-dogleg'
      result = optimFsolveDogleg(columnFun, x, opts, ctx);
      style = 'dogleg';
    case 'trust-region'
      result = optimLsqTrustRegion(columnFun, x, -unbounded, unbounded, opts, ctx);
      style = 'trr';
    otherwise
      result = optimLsqLevenbergMarquardt(columnFun, x, -unbounded, unbounded, opts, ctx);
      style = 'lm';
  end
  [exitflag, messageKind] = mapExitFlag(result, opts);
  info = result.stop;
  if ~strcmp(messageKind, result.stop.kind)
    info = struct('kind', messageKind, 'value', result.resnorm, ...
      'tolerance', opts.FunctionTolerance);
  end
  [message, messageDetailed] = optimLsqExitMessage('fsolve', messageKind, info, opts);
  x = reshape(result.x, xShape);
  fval = reshape(result.residual, fShape);
  jacobian = result.J;
  output = struct();
  output.iterations = result.iterations;
  output.funcCount = result.funcCount;
  output.algorithm = algorithm;
  if ~strcmp(algorithm, 'trust-region-dogleg')
    output.stepsize = result.stepsize;
  end
  if strcmp(algorithm, 'trust-region')
    output.cgiterations = result.cgiterations;
  end
  output.firstorderopt = result.firstorderopt;
  output.message = messageDetailed;
  optimLsqDisplay(opts, 'final', style, struct('message', message, ...
    'messageDetailed', messageDetailed, 'exitflag', exitflag));
end
%=============================================================================
function algorithm = validateAlgorithm(opts)
  algorithm = lower(char(opts.Algorithm));
  valid = {'trust-region-dogleg', 'trust-region', 'levenberg-marquardt'};
  if ~any(strcmp(algorithm, valid))
    error('Nelson:optim:invalidOptionValue', 'Invalid value for "%s".', 'Algorithm');
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
function [exitflag, messageKind] = mapExitFlag(result, opts)
  % fsolve considers the system solved when the sum of squared function
  % values is small compared to the function tolerance.
  nearZero = result.resnorm <= sqrt(opts.FunctionTolerance);
  kind = result.stop.kind;
  messageKind = kind;
  switch kind
    case {'optimality', 'optimality-relative', 'stationary'}
      if nearZero
        exitflag = 1;
        messageKind = 'optimality';
      else
        exitflag = -2;
        messageKind = 'notsolved';
      end
    case 'step'
      if nearZero
        exitflag = 2;
      else
        exitflag = -2;
        messageKind = 'notsolved';
      end
    case 'funchange'
      if nearZero
        exitflag = 3;
      else
        exitflag = -2;
        messageKind = 'notsolved';
      end
    case 'direction'
      if nearZero
        exitflag = 4;
      else
        exitflag = -2;
        messageKind = 'notsolved';
      end
    case 'regularization'
      if nearZero
        exitflag = 2;
        messageKind = 'step';
      else
        exitflag = -3;
      end
    case 'outputfcn'
      exitflag = -1;
    otherwise
      % 'maxiter' or 'maxfeval'
      exitflag = 0;
  end
end
%=============================================================================
