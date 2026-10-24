%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, fval, exitflag, output] = fminbnd(fun, x1, x2, options)
  fromProblem = false;
  if nargin == 1 && isstruct(fun)
    problem = fun;
    fun = problem.objective;
    x1 = problem.x1;
    x2 = problem.x2;
    if isfield(problem, 'options')
      options = problem.options;
    else
      options = [];
    end
    fromProblem = true;
  end
  if ~fromProblem && nargin < 3
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if ~fromProblem && nargin > 4
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  if ~fromProblem && nargin < 4, options = []; end
  opts = optimMergeOptions('fminbnd', options);
  if ~isscalar(x1) || ~isscalar(x2) || ~(x1 < x2)
    error('Nelson:fminbnd:invalidBounds', 'Bounds must be finite scalars with x1 < x2.');
  end
  a = double(x1); b = double(x2);
  c = 0.5 * (3 - sqrt(5));
  x = a + c * (b - a); w = x; v = x;
  e = 0; fx = scalarObjective(fun, x, opts); fw = fx; fv = fx;
  funcCount = 1; exitflag = 0; message = _('Maximum number of iterations or function evaluations reached.');
  for iter = 1:opts.MaxIter
    xm = 0.5 * (a + b);
    tol1 = opts.TolX * abs(x) + eps;
    tol2 = 2 * tol1;
    if abs(x - xm) <= (tol2 - 0.5 * (b - a))
      exitflag = 1; message = _('Optimization converged.'); break
    end
    if abs(e) > tol1
      r = (x - w) * (fx - fv);
      q = (x - v) * (fx - fw);
      p = (x - v) * q - (x - w) * r;
      q = 2 * (q - r);
      if q > 0, p = -p; end
      q = abs(q);
      olde = e; e = 0;
      if abs(p) < abs(0.5 * q * olde) && p > q * (a - x) && p < q * (b - x)
        d = p / q;
        u = x + d;
        if (u - a) < tol2 || (b - u) < tol2
          d = sign(xm - x) * tol1;
        end
      else
        e = chooseInterval(x, xm, a, b);
        d = c * e;
      end
    else
      e = chooseInterval(x, xm, a, b);
      d = c * e;
    end
    u = x + signedMax(d, tol1);
    fu = scalarObjective(fun, u, opts); funcCount = funcCount + 1;
    if fu <= fx
      if u < x, b = x; else, a = x; end
      v = w; fv = fw; w = x; fw = fx; x = u; fx = fu;
    else
      if u < x, a = u; else, b = u; end
      if fu <= fw || w == x
        v = w; fv = fw; w = u; fw = fu;
      elseif fu <= fv || v == x || v == w
        v = u; fv = fu;
      end
    end
    if funcCount >= opts.MaxFunEvals || optimOutputStop(opts.OutputFcn, x, makeValues(fx, iter, funcCount), 'iter')
      if funcCount < opts.MaxFunEvals
        exitflag = -1;
        message = _('Stopped by output function.');
      end
      break
    end
  end
  fval = fx;
  output.iterations = iter;
  output.funcCount = funcCount;
  output.algorithm = 'bounded scalar minimization';
  output.message = message;
end
%=============================================================================
function e = chooseInterval(x, xm, a, b)
  if x >= xm, e = a - x; else, e = b - x; end
end
%=============================================================================
function y = signedMax(d, tol)
  if d >= 0, y = max(d, tol); else, y = -max(abs(d), tol); end
end
%=============================================================================
function f = scalarObjective(fun, x, opts)
  f = optimCall(fun, x);
  f = optimCheckFunValue(f, opts);
  if ~isnumeric(f) || ~isscalar(f), error('Nelson:optimization:invalidObjectiveScalar', '%s must return a numeric scalar.', 'Objective function'); end
  f = double(f);
end
%=============================================================================
function values = makeValues(fval, iter, funcCount)
  values = struct();
  values.fval = fval; values.iteration = iter; values.funccount = funcCount;
end
%=============================================================================
