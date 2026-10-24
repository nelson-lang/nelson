%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function err = odeErrorNorm(problem, y, yhigh, ylow)
  if odeCanUseNativeErrorNorm(problem, y, yhigh, ylow)
    err = __ode_error_norm__(y, yhigh, ylow, problem.options.AbsTol, ...
      problem.options.RelTol, double(strcmpi(problem.options.NormControl, 'on')));
  else
    err = odeErrorNormMacro(problem, y, yhigh, ylow);
  end
  if isempty(err) || err == 0
    err = eps;
  end
  err = err * odeErrorScale(problem);
end
%=============================================================================
function scale = odeErrorScale(problem)
  switch problem.solver
    case 'ode23t'
      scale = 0.01;
    otherwise
      scale = 1;
  end
end
%=============================================================================
function tf = odeCanUseNativeErrorNorm(problem, y, yhigh, ylow)
  persistent hasNative;
  if isempty(hasNative)
    hasNative = exist('__ode_error_norm__') == 5;
  end
  tf = hasNative && isa(y, 'double') && isa(yhigh, 'double') && isa(ylow, 'double') ...
    && isa(problem.options.AbsTol, 'double') && isa(problem.options.RelTol, 'double') ...
    && isreal(y) && isreal(yhigh) && isreal(ylow) && isreal(problem.options.AbsTol) ...
    && isreal(problem.options.RelTol);
end
%=============================================================================
function err = odeErrorNormMacro(problem, y, yhigh, ylow)
  scale = problem.options.AbsTol + max(abs(y), abs(yhigh)) * problem.options.RelTol;
  scaledError = abs(yhigh - ylow) ./ scale;
  if strcmpi(problem.options.NormControl, 'on')
    err = norm(scaledError) / sqrt(length(scaledError));
  else
    err = max(scaledError);
  end
end
%=============================================================================
