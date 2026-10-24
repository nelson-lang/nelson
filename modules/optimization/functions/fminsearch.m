%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, fval, exitflag, output] = fminsearch(fun, x0, options)
  fromProblem = false;
  if nargin == 1 && isstruct(fun)
    problem = fun;
    fun = problem.objective;
    x0 = problem.x0;
    if isfield(problem, 'options')
      options = problem.options;
    else
      options = [];
    end
    fromProblem = true;
  end
  if ~fromProblem && nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if ~fromProblem && nargin > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  if ~fromProblem && nargin < 3, options = []; end
  opts = optimMergeOptions('fminsearch', options);
  xShape = size(x0);
  x0 = double(x0(:));
  n = length(x0);
  maxIter = opts.MaxIter;
  maxFun = opts.MaxFunEvals;
  simplex = zeros(n, n + 1);
  simplex(:, 1) = x0;
  usualDelta = 0.05;
  zeroDelta = 0.00025;
  for j = 1:n
    y = x0;
    if y(j) ~= 0
      y(j) = (1 + usualDelta) * y(j);
    else
      y(j) = zeroDelta;
    end
    simplex(:, j + 1) = y;
  end
  values = zeros(1, n + 1);
  for j = 1:n + 1
    values(j) = scalarObjective(fun, simplex(:, j), opts);
  end
  funcCount = n + 1;
  exitflag = 0;
  message = _('Maximum number of iterations or function evaluations reached.');
  optimOutputStop(opts.OutputFcn, x0, makeValues(values(1), 0, funcCount), 'init');
  for iter = 1:maxIter
    [values, order] = sort(values);
    simplex = simplex(:, order);
    best = simplex(:, 1);
    if max(max(abs(simplex(:, 2:end) - best))) <= opts.TolX ...
        && max(abs(values(1) - values(2:end))) <= opts.TolFun
      exitflag = 1;
      message = _('Optimization converged.');
      break
    end
    if funcCount >= maxFun
      break
    end
    centroid = mean(simplex(:, 1:n), 2);
    xr = centroid + (centroid - simplex(:, end));
    fr = scalarObjective(fun, xr, opts); funcCount = funcCount + 1;
    if fr < values(1)
      xe = centroid + 2 * (xr - centroid);
      fe = scalarObjective(fun, xe, opts); funcCount = funcCount + 1;
      if fe < fr
        simplex(:, end) = xe; values(end) = fe;
      else
        simplex(:, end) = xr; values(end) = fr;
      end
    elseif fr < values(n)
      simplex(:, end) = xr; values(end) = fr;
    else
      if fr < values(end)
        xc = centroid + 0.5 * (xr - centroid);
      else
        xc = centroid - 0.5 * (centroid - simplex(:, end));
      end
      fc = scalarObjective(fun, xc, opts); funcCount = funcCount + 1;
      if fc < min(fr, values(end))
        simplex(:, end) = xc; values(end) = fc;
      else
        for j = 2:n + 1
          simplex(:, j) = simplex(:, 1) + 0.5 * (simplex(:, j) - simplex(:, 1));
          values(j) = scalarObjective(fun, simplex(:, j), opts);
        end
        funcCount = funcCount + n;
      end
    end
    if optimOutputStop(opts.OutputFcn, simplex(:, 1), makeValues(values(1), iter, funcCount), 'iter')
      exitflag = -1; message = _('Stopped by output function.'); break
    end
  end
  [values, order] = sort(values);
  simplex = simplex(:, order);
  x = reshape(simplex(:, 1), xShape);
  fval = values(1);
  optimOutputStop(opts.OutputFcn, x, makeValues(fval, iter, funcCount), 'done');
  output.iterations = iter;
  output.funcCount = funcCount;
  output.algorithm = 'Nelder-Mead simplex direct search';
  output.message = message;
end
%=============================================================================
function f = scalarObjective(fun, x, opts)
  f = optimCall(fun, x);
  f = optimCheckFunValue(f, opts);
  if ~isnumeric(f) || ~isscalar(f)
    error('Nelson:optimization:invalidObjectiveScalar', '%s must return a numeric scalar.', 'Objective function');
  end
  f = double(f);
end
%=============================================================================
function values = makeValues(fval, iter, funcCount)
  values = struct();
  values.fval = fval;
  values.iteration = iter;
  values.funccount = funcCount;
end
%=============================================================================
