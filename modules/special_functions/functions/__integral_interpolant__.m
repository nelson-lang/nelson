%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Internal entry point of the integralInterpolant class:
% data = __integral_interpolant__('build', fun, lower, upper, name, value, ...)
% Fq = __integral_interpolant__('evaluate', data, xq)
%=============================================================================
function result = __integral_interpolant__(action, varargin)
  if strcmp(action, 'evaluate')
    result = integral_interpolant_eval(varargin{:});
  else
    result = integralInterpolantBuild(varargin{:});
  end
end
%=============================================================================
function data = integralInterpolantBuild(fun, lower, upper, varargin)
  if ~isa(fun, 'function_handle')
    error('Nelson:integralInterpolant:funArgNotHandle', _('First argument must be a function handle.'));
  end
  if ~isIntegralLimit(lower) || ~isIntegralLimit(upper)
    error('Nelson:integralInterpolant:invalidLimits', _('Integration limits must be real numeric scalars.'));
  end
  if lower == upper
    error('Nelson:integralInterpolant:equalLimits', _('Upper and lower limits must be different.'));
  end
  opts = integral_parse_options('integralInterpolant', varargin{:});
  if ~isreal(opts.Waypoints)
    error('Nelson:integralInterpolant:invalidWaypoints', _('''Waypoints'' value must be a vector with all finite real values.'));
  end
  data = integral_interpolant_build(fun, double(lower), double(upper), opts);
  data.integrand = fun;
  data.lower = double(lower);
  data.upper = double(upper);
  data.absTol = opts.AbsTol;
  data.relTol = opts.RelTol;
end
%=============================================================================
function tf = isIntegralLimit(value)
  tf = isnumeric(value) && isscalar(value) && isreal(value) && ~isnan(value);
end
%=============================================================================
