%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function optimFminuncDiagnostics(data, opts)
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
  if isObjectiveHessian(opts)
    mode = 'objective';
  elseif ~isempty(opts.HessianMultiplyFcn) || ~isempty(opts.HessMult)
    mode = 'multiply function';
  else
    mode = opts.HessianApproximation;
  end
end
%=============================================================================
function tf = isObjectiveHessian(opts)
  tf = (ischar(opts.HessianFcn) || isstring(opts.HessianFcn)) && strcmpi(char(opts.HessianFcn), 'objective');
end
%=============================================================================
