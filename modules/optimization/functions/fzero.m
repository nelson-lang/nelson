%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, fval, exitflag, output] = fzero(fun, x0, options)
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
  if ~fromProblem && nargin < 3
    options = [];
  end
  opts = optimMergeOptions('fzero', options);
  [a, b, fa, fb, funcCount] = bracketRoot(fun, x0, opts);
  c = a; fc = fa; d = b - a; e = d;
  exitflag = 0; message = _('Maximum number of iterations or function evaluations reached.');
  for iter = 1:opts.MaxIter
    if fb == 0 || abs(b - a) <= opts.TolX * max(abs(b), 1)
      exitflag = 1; message = _('Zero found.'); break
    end
    if sign(fa) == sign(fb)
      a = c; fa = fc; d = b - a; e = d;
    end
    if abs(fa) < abs(fb)
      c = b; b = a; a = c; fc = fb; fb = fa; fa = fc;
    end
    tol = 2 * eps * abs(b) + 0.5 * opts.TolX;
    m = 0.5 * (a - b);
    if abs(m) <= tol || fb == 0
      exitflag = 1; message = _('Zero found.'); break
    end
    if abs(e) >= tol && abs(fc) > abs(fb)
      s = fb / fc;
      if a == c
        p = 2 * m * s; q = 1 - s;
      else
        q = fc / fa; r = fb / fa;
        p = s * (2 * m * q * (q - r) - (b - c) * (r - 1));
        q = (q - 1) * (r - 1) * (s - 1);
      end
      if p > 0
        q = -q;
      else
        p = -p;
      end
      if 2 * p < min(3 * m * q - abs(tol * q), abs(e * q))
        e = d; d = p / q;
      else
        d = m; e = m;
      end
    else
      d = m; e = m;
    end
    c = b; fc = fb;
    if abs(d) > tol
      b = b + d;
    else
      b = b + sign(m) * tol;
    end
    fb = scalarValue(fun, b, opts); funcCount = funcCount + 1;
    if funcCount >= opts.MaxFunEvals || optimOutputStop(opts.OutputFcn, b, makeValues(fb, iter, funcCount), 'iter')
      if funcCount < opts.MaxFunEvals
        exitflag = -1;
        message = _('Stopped by output function.');
      end
      break
    end
  end
  x = b; fval = fb;
  output.iterations = iter; output.funcCount = funcCount;
  output.algorithm = 'Brent-Dekker zero search'; output.message = message;
end
%=============================================================================
function [a, b, fa, fb, count] = bracketRoot(fun, x0, opts)
  count = 0;
  if numel(x0) == 2
    a = double(x0(1)); b = double(x0(2));
    fa = scalarValue(fun, a, opts); fb = scalarValue(fun, b, opts); count = 2;
    if sign(fa) == sign(fb)
      error('Nelson:fzero:endpointsSameSign', 'Function values at interval endpoints must differ in sign.');
    end
    return
  end
  center = double(x0);
  step = 0.01 * max(abs(center), 1);
  a = center; b = center; fa = scalarValue(fun, center, opts); fb = fa; count = 1;
  for k = 1:50
    a = center - step; b = center + step;
    fa = scalarValue(fun, a, opts); fb = scalarValue(fun, b, opts); count = count + 2;
    if sign(fa) ~= sign(fb)
      return
    end
    step = 2 * step;
  end
  error('Nelson:fzero:cannotBracket', 'Unable to bracket a zero.');
end
%=============================================================================
function f = scalarValue(fun, x, opts)
  f = optimCall(fun, x);
  f = optimCheckFunValue(f, opts);
  if ~isnumeric(f) || ~isscalar(f)
    error('Nelson:fzero:invalidFunctionValue', '%s must return a numeric scalar.', 'Function');
  end
  f = double(f);
end
%=============================================================================
function values = makeValues(fval, iter, funcCount)
  values = struct();
  values.fval = fval; values.iteration = iter; values.funccount = funcCount;
end
%=============================================================================
