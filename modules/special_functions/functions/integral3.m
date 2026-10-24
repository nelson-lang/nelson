%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function q = integral3(fun, xmin, xmax, ymin, ymax, zmin, zmax, varargin)
  narginchk(7, inf);
  nargoutchk(0, 1);
  if ~isa(fun, 'function_handle')
    error('Nelson:special_functions:funArgNotHandle', 'First argument must be a function handle.');
  end
  if ~(isnumeric(xmin) && isscalar(xmin)) || ~(isnumeric(xmax) && isscalar(xmax))
    error('Nelson:special_functions:limitsNumericScalars', 'Integration limits must be numeric scalars.');
  end
  opts = integral_parse_options('integral3', varargin{:});
  ylo = integral3Bound1(ymin);
  yhi = integral3Bound1(ymax);
  zlo = integral3Bound2(zmin);
  zhi = integral3Bound2(zmax);
  finiteScalarBounds = integral3FiniteScalarBounds(ymin, ymax, zmin, zmax);
  if isfinite(xmin) && isfinite(xmax) && finiteScalarBounds && opts.Vectorized && ~strcmp(opts.Method, 'iterated')
    if isempty(opts.Waypoints)
      q = integral3Tiled(fun, double(xmin), double(xmax), ylo, yhi, zlo, zhi);
    else
      q = integral3TiledWaypoints(fun, double(xmin), double(xmax), ylo, yhi, zlo, zhi, opts.Waypoints);
    end
    return
  elseif strcmp(opts.Method, 'tiled')
    error('Nelson:integral3:tiledInfinite', 'The tiled method requires finite limits.');
  end
  if isempty(opts.Waypoints)
    opts.Waypoints = {[], [], []};
  end
  outer = @(x) integral3Outer(fun, x, ylo, yhi, zlo, zhi, opts);
  q = integral3Split(outer, xmin, xmax, opts.Waypoints{1}, opts);
end
%=============================================================================
function q = integral3Split(fun, a, b, waypoints, opts)
  if isempty(waypoints)
    q = integral_gk(fun, double(a), double(b), opts.RelTol, opts.AbsTol, false);
  else
    q = integral_contour(fun, integral_path(a, b, waypoints), opts.RelTol, opts.AbsTol, false);
  end
end
%=============================================================================
function tf = integral3FiniteScalarBounds(varargin)
  tf = true;
  for k = 1:nargin
    value = varargin{k};
    if isnumeric(value) && isscalar(value) && ~isfinite(value)
      tf = false;
      return
    end
  end
end
%=============================================================================
function h = integral3Bound1(v)
  if isa(v, 'function_handle')
    h = v;
  else
    h = integral3ScalarBound(v);
  end
end
%=============================================================================
function h = integral3Bound2(v)
  if isa(v, 'function_handle')
    h = v;
  else
    h = @(x, y) double(v) + 0 * x + 0 * y;
  end
end
%=============================================================================
function h = integral3ScalarBound(v)
  if ~(isnumeric(v) && isscalar(v))
    error('Nelson:special_functions:invalidLimits', 'Integration limits must be numeric scalars or function handles.');
  end
  val = double(v);
  h = @(x) val + 0 * x;
end
%=============================================================================
function v = integral3Outer(fun, X, ylo, yhi, zlo, zhi, opts)
  v = zeros(size(X));
  for i = 1:numel(X)
    xi = X(i);
    inner = @(y) integral3Middle(fun, xi, y, zlo, zhi, opts);
    v(i) = integral3Split(inner, ylo(xi), yhi(xi), opts.Waypoints{2}, opts);
  end
end
%=============================================================================
function q = integral3Tiled(fun, xmin, xmax, ylo, yhi, zlo, zhi)
  [nodes, weights] = integral3Rule();
  ax = 0.5 * (xmax - xmin);
  bx = 0.5 * (xmin + xmax);
  q = 0;
  for ix = 1:numel(nodes)
    x = ax .* nodes(ix) + bx;
    ya = ylo(x);
    yb = yhi(x);
    ay = 0.5 .* (yb - ya);
    by = 0.5 .* (ya + yb);
    y = ay .* nodes + by;
    for iy = 1:numel(nodes)
      zi0 = zlo(x + 0 * y(iy), y(iy));
      zi1 = zhi(x + 0 * y(iy), y(iy));
      az = 0.5 .* (zi1 - zi0);
      bz = 0.5 .* (zi0 + zi1);
      z = az .* nodes + bz;
      xvec = x + 0 * z;
      yvec = y(iy) + 0 * z;
      values = fun(xvec, yvec, z);
      q = q + weights(ix) .* weights(iy) .* ay .* sum(weights(:) .* az .* values(:));
    end
  end
  q = ax .* q;
end
%=============================================================================
function q = integral3TiledWaypoints(fun, xmin, xmax, ylo, yhi, zlo, zhi, waypoints)
  % composite tiled rule: each interval is split at its waypoints
  [nodes, weights] = integral3Rule();
  [x, wx] = integral3Composite(integral_path(xmin, xmax, waypoints{1}), nodes, weights);
  q = 0;
  for ix = 1:numel(x)
    [y, wy] = integral3Composite(integral_path(ylo(x(ix)), yhi(x(ix)), waypoints{2}), nodes, weights);
    for iy = 1:numel(y)
      [z, wz] = integral3Composite(integral_path(zlo(x(ix), y(iy)), zhi(x(ix), y(iy)), waypoints{3}), nodes, weights);
      values = fun(x(ix) + 0 * z, y(iy) + 0 * z, z);
      q = q + wx(ix) .* wy(iy) .* sum(wz(:) .* values(:));
    end
  end
end
%=============================================================================
function [t, w] = integral3Composite(points, nodes, weights)
  a = points(1:end - 1).';
  half = 0.5 * (points(2:end).' - a);
  t = reshape((a + half + half .* nodes).', 1, []);
  w = reshape((half .* weights).', 1, []);
end
%=============================================================================
function [nodes, weights] = integral3Rule()
  xgk = [0.991455371120813, 0.949107912342759, 0.864864423359769, ...
    0.741531185599394, 0.586087235467691, 0.405845151377397, ...
    0.207784955007898, 0.000000000000000];
  wgk = [0.022935322010529, 0.063092092629979, 0.104790010322250, ...
    0.140653259715525, 0.169004726639267, 0.190350578064785, ...
    0.204432940075298, 0.209482141084728];
  nodes = [-xgk(1:7), xgk(8), fliplr(xgk(1:7))];
  weights = [wgk(1:7), wgk(8), fliplr(wgk(1:7))];
end
%=============================================================================
function v = integral3Middle(fun, x, Y, zlo, zhi, opts)
  v = zeros(size(Y));
  for i = 1:numel(Y)
    yi = Y(i);
    inner = @(z) integral3Eval(fun, x, yi, z, opts.Vectorized);
    v(i) = integral3Split(inner, zlo(x, yi), zhi(x, yi), opts.Waypoints{3}, opts);
  end
end
%=============================================================================
function w = integral3Eval(fun, x, y, z, vectorized)
  if vectorized
    w = fun(x + 0 * z, y + 0 * z, z);
  else
    w = zeros(size(z));
    for k = 1:numel(z)
      w(k) = fun(x, y, z(k));
    end
  end
end
%=============================================================================
