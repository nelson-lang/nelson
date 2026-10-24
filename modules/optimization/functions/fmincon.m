%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, fval, exitflag, output, lambda, grad, hessian] = fmincon(fun, x0, A, b, Aeq, beq, lb, ub, nonlcon, options)
  if nargin == 1 && isstruct(fun)
    [fun, x0, A, b, Aeq, beq, lb, ub, nonlcon, options] = optimFminconProblem(fun);
  else
    if nargin < 2
      error('Nelson:notEnoughInputs', 'Not enough input arguments.');
    end
    if nargin > 10
      error('Nelson:tooManyInputs', 'Too many input arguments.');
    end
    if nargin < 3, A = []; end
    if nargin < 4, b = []; end
    if nargin < 5, Aeq = []; end
    if nargin < 6, beq = []; end
    if nargin < 7, lb = []; end
    if nargin < 8, ub = []; end
    if nargin < 9, nonlcon = []; end
    if nargin < 10, options = []; end
  end
  opts = optimMergeOptions('fmincon', options);
  opts.MaxFunEvals = optimResolveMaxFunEvals(opts, numel(x0));
  opts.MaxFunctionEvaluations = opts.MaxFunEvals;
  data = optimFminconPrepare(fun, x0, A, b, Aeq, beq, lb, ub, nonlcon, opts);
  optimFminconDiagnostics(data, opts);
  result = optimFminconSQP(data, opts);
  x = reshape(result.x, data.xShape);
  fval = result.fval;
  exitflag = result.exitflag;
  output = result.output;
  lambda = result.lambda;
  grad = result.grad;
  hessian = result.hessian;
end
%=============================================================================
