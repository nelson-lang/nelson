%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function q = integral(fun, a, b, varargin)
  narginchk(3, inf);
  nargoutchk(0, 1);
  if ~isa(fun, 'function_handle')
    error('Nelson:special_functions:funArgNotHandle', 'First argument must be a function handle.');
  end
  if ~(isnumeric(a) && isscalar(a)) || ~(isnumeric(b) && isscalar(b))
    error('Nelson:special_functions:limitsNumericScalars', 'Integration limits must be numeric scalars.');
  end
  opts = integral_parse_options('integral', varargin{:});
  if ~opts.Vectorized && ~opts.ArrayValued
    % integrand written for scalar inputs: evaluate it element by element
    fun = @(x) arrayfun(fun, x);
  end
  if isempty(opts.Waypoints) && isreal(a) && isreal(b)
    q = integral_gk(fun, a, b, opts.RelTol, opts.AbsTol, opts.ArrayValued);
  else
    q = integral_contour(fun, integral_path(a, b, opts.Waypoints), opts.RelTol, opts.AbsTol, opts.ArrayValued);
  end
end
