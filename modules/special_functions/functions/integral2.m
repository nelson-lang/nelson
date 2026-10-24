%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function q = integral2(fun, xmin, xmax, ymin, ymax, varargin)
  narginchk(5, inf);
  nargoutchk(0, 1);
  if ~isa(fun, 'function_handle')
    error('Nelson:special_functions:funArgNotHandle', 'First argument must be a function handle.');
  end
  if ~(isnumeric(xmin) && isscalar(xmin)) || ~(isnumeric(xmax) && isscalar(xmax))
    error('Nelson:special_functions:limitsNumericScalars', 'Integration limits must be numeric scalars.');
  end
  opts = integral_parse_options('integral2', varargin{:});
  if ~opts.Vectorized
    % integrand written for scalar inputs: evaluate it element by element
    fun = @(x, y) arrayfun(fun, x + zeros(size(y)), y);
  end
  reltol = opts.RelTol;
  abstol = opts.AbsTol;
  ylo = integral2_bound(ymin);
  yhi = integral2_bound(ymax);
  if isempty(opts.Waypoints)
    outer = @(x) integral2_outer(fun, x, ylo, yhi, reltol, abstol);
    q = integral_gk(outer, double(xmin), double(xmax), reltol, abstol, false);
  else
    % waypoints {xw, yw}: split the x interval at xw and each y interval at yw
    yw = opts.Waypoints{2};
    outer = @(x) integral2_outer(fun, x, ylo, yhi, reltol, abstol, yw);
    q = integral_contour(outer, integral_path(xmin, xmax, opts.Waypoints{1}), reltol, abstol, false);
  end
end
%=============================================================================
function h = integral2_bound(v)
  if isa(v, 'function_handle')
    h = v;
  else
    if ~(isnumeric(v) && isscalar(v))
      error('Nelson:special_functions:invalidLimits', 'Integration limits must be numeric scalars or function handles.');
    end
    val = double(v);
    h = @(x) val + 0 * x;
  end
end
%=============================================================================
function v = integral2_outer(fun, X, ylo, yhi, reltol, abstol, yw)
  v = zeros(size(X));
  for i = 1:numel(X)
    xi = X(i);
    ya = ylo(xi);
    yb = yhi(xi);
    if nargin < 7 || isempty(yw)
      inner = integral_gk(@(y) fun(xi, y), double(ya), double(yb), reltol, abstol, false);
    else
      inner = integral_contour(@(y) fun(xi, y), integral_path(ya, yb, yw), reltol, abstol, false);
    end
    if ~isreal(inner) && isreal(v)
      v = complex(v);
    end
    v(i) = inner;
  end
end
%=============================================================================
