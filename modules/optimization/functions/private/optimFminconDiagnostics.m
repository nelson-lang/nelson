%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function optimFminconDiagnostics(data, opts)
  if ~strcmpi(opts.Diagnostics, 'on')
    return
  end
  disp('____________________________________________________________')
  disp('   Diagnostic Information')
  disp(' ')
  disp(['Number of variables: ', int2str(length(data.x0))])
  disp(' ')
  disp('Functions')
  disp(['Objective:                            ', functionName(data.fun)])
  disp(['Gradient:                             ', gradientMode(opts)])
  disp(['Hessian:                              ', hessianMode(opts)])
  disp(' ')
  disp('Constraints')
  disp(['Nonlinear constraints:                ', nonlinearMode(data)])
  disp(' ')
  disp(['Number of linear inequality constraints:    ', int2str(size(data.A, 1))])
  disp(['Number of linear equality constraints:      ', int2str(size(data.Aeq, 1))])
  disp(['Number of lower bound constraints:          ', int2str(sum(isfinite(data.lb)))])
  disp(['Number of upper bound constraints:          ', int2str(sum(isfinite(data.ub)))])
  disp(' ')
  disp('Algorithm selected')
  disp(['   ', data.algorithm])
  disp(' ')
  disp('____________________________________________________________')
  disp('   End diagnostic information')
end
%=============================================================================
function name = functionName(fun)
  if isa(fun, 'function_handle')
    name = func2str(fun);
  elseif isstring(fun)
    name = char(fun);
  else
    name = fun;
  end
end
%=============================================================================
function mode = gradientMode(opts)
  if opts.SpecifyObjectiveGradient || strcmpi(opts.GradObj, 'on')
    mode = 'user-supplied';
  else
    mode = 'finite-differencing';
  end
end
%=============================================================================
function mode = hessianMode(opts)
  if ~isempty(opts.HessianFcn) || ~isempty(opts.HessFcn)
    mode = 'user-supplied';
  elseif ~isempty(opts.HessianMultiplyFcn) || ~isempty(opts.HessMult)
    mode = 'user-supplied multiply function';
  elseif isnumeric(opts.Hessian) && ~isempty(opts.Hessian)
    mode = 'user-supplied matrix';
  else
    mode = opts.HessianApproximation;
  end
end
%=============================================================================
function mode = nonlinearMode(data)
  if isempty(data.nonlcon)
    mode = 'do not exist';
  else
    mode = 'exist';
  end
end
%=============================================================================
