%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, fval, exitflag, output, grad, hessian] = fminunc(fun, x0, options)
  if nargin == 1 && isstruct(fun)
    [fun, x0, options] = optimFminuncProblem(fun);
  elseif nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  elseif nargin > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  elseif nargin < 3
    options = [];
  end
  opts = optimMergeOptions('fminunc', options);
  data = optimFminuncPrepare(fun, x0, opts);
  optimFminuncDiagnostics(data, opts);
  if strcmp(data.algorithm, 'trust-region')
    result = optimFminuncTrustRegion(data, opts);
  else
    result = optimFminuncQuasiNewton(data, opts);
  end
  x = reshape(result.x, data.xShape);
  fval = result.fval;
  exitflag = result.exitflag;
  output = result.output;
  grad = result.grad;
  hessian = result.hessian;
end
%=============================================================================
