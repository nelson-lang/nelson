%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function vq = evaluate(obj, varargin)
  if isempty(obj.GridVectors)
    error('Nelson:griddedInterpolant:noSampleData', 'The interpolant has no sample data.');
  end
  nd = numel(obj.GridVectors);
  method = obj.Method;
  extrap = obj.ExtrapolationMethod;
  if nd == 1
    [xq, outSize] = griddedInterpolantQuery1D(varargin{:});
    x = obj.GridVectors{1}(:);
    v = obj.Values(:);
    vq = griddedInterpolantEval1D(x, v, xq(:), method, extrap);
    vq = reshape(vq, outSize);
  else
    [queries, outSize] = griddedInterpolantQueryND(nd, varargin{:});
    vq = griddedInterpolantEvalND(obj.GridVectors, obj.Values, queries, method, extrap);
    vq = reshape(vq, outSize);
  end
end
%=============================================================================
function [xq, outSize] = griddedInterpolantQuery1D(varargin)
  if numel(varargin) ~= 1
    error('Nelson:griddedInterpolant:singleQueryExpected', 'A single query argument is expected for a 1-D interpolant.');
  end
  q = varargin{1};
  if iscell(q)
    if numel(q) ~= 1
      error('Nelson:griddedInterpolant:queryGridDimensionMismatch', 'The number of query grid vectors must match the interpolant dimensions.');
    end
    xq = q{1}(:);
    outSize = [numel(xq), 1];
  else
    if ~isnumeric(q) || ~isreal(q)
      error('Nelson:griddedInterpolant:invalidQueryPoints', 'Query points must be real numeric arrays.');
    end
    xq = q;
    outSize = size(q);
  end
end
%=============================================================================
function vq = griddedInterpolantEval1D(x, v, xq, method, extrap)
  vq = interp1(x, v, xq, method);
  if strcmp(extrap, 'none')
    return
  end
  outside = ~(xq >= x(1) & xq <= x(end));
  if ~any(outside)
    return
  end
  vq(outside) = griddedInterpolantExtrap1D(x, v, xq(outside), extrap);
end
%=============================================================================
function vqEx = griddedInterpolantExtrap1D(x, v, xq, extrap)
  if strcmp(extrap, 'cubic')
    % Cubic convolution extrapolation: interp1 itself does not extrapolate for
    % 'cubic', so evaluate the cubic-convolution scheme here on the outside
    % query points using the clamped end interval and the extended endpoint
    % values.
    vqEx = griddedInterpolantCubicExtrap1D(x, v, xq);
    return
  end
  vqEx = interp1(x, v, xq, extrap, 'extrap');
  if strcmp(extrap, 'next')
    vqEx(xq > x(end)) = NaN;
  elseif strcmp(extrap, 'previous')
    vqEx(xq < x(1)) = NaN;
  end
end
%=============================================================================
function vqEx = griddedInterpolantCubicExtrap1D(x, v, xq)
  x = x(:);
  v = v(:);
  n = numel(x);
  dh = diff(x);
  if n < 2 || ~all(abs(dh - dh(1)) <= 1e-10 * max(abs(dh(1)), 1))
    % On a non-uniform grid the cubic interior falls back to a spline, so keep
    % the extrapolation consistent with that fallback.
    vqEx = interp1(x, v, xq, 'spline', 'extrap');
    vqEx = reshape(vqEx, size(xq));
    return
  end
  h = dh(1);
  q = xq(:);
  yq = zeros(numel(q), 1);
  for k = 1:numel(q)
    idx = floor((q(k) - x(1)) / h) + 1;
    if idx < 1
      idx = 1;
    elseif idx > n - 1
      idx = n - 1;
    end
    t = (q(k) - x(idx)) / h;
    yi = v(idx);
    yp1 = v(idx + 1);
    if idx - 1 >= 1
      ym1 = v(idx - 1);
    else
      ym1 = 3 * v(1) - 3 * v(2) + v(3);
    end
    if idx + 2 <= n
      yp2 = v(idx + 2);
    else
      yp2 = 3 * v(n) - 3 * v(n - 1) + v(n - 2);
    end
    yq(k) = yi + 0.5 * t * ((yp1 - ym1) ...
      + t * ((2 * ym1 - 5 * yi + 4 * yp1 - yp2) ...
      + t * (-ym1 + 3 * yi - 3 * yp1 + yp2)));
  end
  vqEx = reshape(yq, size(xq));
end
%=============================================================================
function [queries, outSize] = griddedInterpolantQueryND(nd, varargin)
  if numel(varargin) == 1 && iscell(varargin{1})
    q = varargin{1};
    if numel(q) ~= nd
      error('Nelson:griddedInterpolant:queryGridDimensionMismatch', 'The number of query grid vectors must match the interpolant dimensions.');
    end
    vectors = cell(1, nd);
    lengths = zeros(1, nd);
    for dim = 1:nd
      vectors{dim} = q{dim}(:);
      lengths(dim) = numel(vectors{dim});
    end
    grids = cell(1, nd);
    [grids{:}] = ndgrid(vectors{:});
    queries = grids;
    outSize = lengths;
    return
  end
  if numel(varargin) ~= nd
    error('Nelson:griddedInterpolant:queryArrayDimensionMismatch', 'The number of query arrays must match the interpolant dimensions.');
  end
  outSize = [1, 1];
  for dim = 1:nd
    sz = size(varargin{dim});
    if prod(sz) > 1
      outSize = sz;
      break
    end
  end
  queries = cell(1, nd);
  for dim = 1:nd
    q = varargin{dim};
    if ~isnumeric(q) || ~isreal(q)
      error('Nelson:griddedInterpolant:invalidQueryPoints', 'Query points must be real numeric arrays.');
    end
    if isscalar(q)
      queries{dim} = repmat(double(q), outSize);
    else
      if ~isequal(size(q), outSize)
        error('Nelson:dimagree:valueArraysSize', '%s arrays must have the same size.', 'Query');
      end
      queries{dim} = double(q);
    end
  end
end
%=============================================================================
function vq = griddedInterpolantEvalND(gridVectors, values, queries, method, extrap)
  nd = numel(gridVectors);
  if any(strcmp(method, {'previous', 'next', 'pchip'}))
    error('Nelson:griddedInterpolant:methodOnly1D', 'The ''previous'', ''next'', and ''pchip'' methods are supported only for 1-D interpolation.');
  end
  outside = false(size(queries{1}));
  for dim = 1:nd
    g = gridVectors{dim}(:);
    outside = outside | (queries{dim} < g(1)) | (queries{dim} > g(end)) | isnan(queries{dim});
  end
  if strcmp(method, 'nearest')
    vq = griddedInterpolantNearestND(gridVectors, values, queries);
  else
    vq = interpn(gridVectors{:}, values, queries{:}, method);
  end
  if ~any(outside(:))
    return
  end
  if strcmp(extrap, 'none')
    vq(outside) = NaN;
    return
  end
  if strcmp(extrap, method) && any(strcmp(method, {'spline', 'makima', 'nearest'}))
    return
  end
  vq(outside) = griddedInterpolantExtrapND(gridVectors, values, queries, extrap, outside);
end
%=============================================================================
function vqEx = griddedInterpolantExtrapND(gridVectors, values, queries, extrap, outside)
  nd = numel(gridVectors);
  idx = find(outside);
  subset = cell(1, nd);
  for dim = 1:nd
    q = queries{dim};
    subset{dim} = q(idx);
  end
  if strcmp(extrap, 'nearest')
    vqEx = griddedInterpolantNearestND(gridVectors, values, subset);
  elseif strcmp(extrap, 'linear')
    vqEx = griddedInterpolantLinearND(gridVectors, values, subset);
  elseif strcmp(extrap, 'cubic')
    vqEx = griddedInterpolantCubicExtrapND(gridVectors, values, subset);
  else
    vqEx = interpn(gridVectors{:}, values, subset{:}, extrap);
  end
  vqEx = vqEx(:);
end
%=============================================================================
function vqEx = griddedInterpolantCubicExtrapND(gridVectors, values, subset)
  % Tensor-product cubic-convolution extrapolation: interpn does not
  % extrapolate for 'cubic', so evaluate the cubic-convolution scheme on the
  % outside query points using, per dimension, the clamped end interval and the
  % extended endpoint values. On a non-uniform grid the cubic interior falls
  % back to a spline, so keep the extrapolation consistent with that fallback.
  nd = numel(gridVectors);
  n = zeros(1, nd);
  h = zeros(1, nd);
  x0 = zeros(1, nd);
  uniform = true;
  for dim = 1:nd
    g = gridVectors{dim}(:);
    n(dim) = numel(g);
    if n(dim) < 3
      uniform = false;
      break
    end
    dh = diff(g);
    if ~all(abs(dh - dh(1)) <= 1e-10 * max(abs(dh(1)), 1))
      uniform = false;
      break
    end
    h(dim) = dh(1);
    x0(dim) = g(1);
  end
  if ~uniform
    vqEx = interpn(gridVectors{:}, values, subset{:}, 'spline');
    vqEx = vqEx(:);
    return
  end
  Vext = padCubicGhostND(values, nd);
  paddedSize = n + 2;
  strides = [1, cumprod(paddedSize(1:end - 1))];
  vv = Vext(:);
  npts = numel(subset{1});
  idxDim = zeros(npts, nd);
  wDim = cell(1, nd);
  for dim = 1:nd
    q = subset{dim}(:);
    id = floor((q - x0(dim)) / h(dim)) + 1;
    id = min(max(id, 1), n(dim) - 1);
    t = (q - (x0(dim) + (id - 1) * h(dim))) / h(dim);
    idxDim(:, dim) = id;
    t2 = t .* t;
    t3 = t2 .* t;
    w = zeros(npts, 4);
    w(:, 1) = 0.5 * (-t3 + 2 * t2 - t);
    w(:, 2) = 0.5 * (3 * t3 - 5 * t2 + 2);
    w(:, 3) = 0.5 * (-3 * t3 + 4 * t2 + t);
    w(:, 4) = 0.5 * (t3 - t2);
    wDim{dim} = w;
  end
  yq = zeros(npts, 1);
  ncombo = 4 ^ nd;
  for c = 0:(ncombo - 1)
    rest = c;
    weight = ones(npts, 1);
    lin = ones(npts, 1);
    for dim = 1:nd
      off = mod(rest, 4);
      rest = floor(rest / 4);
      weight = weight .* wDim{dim}(:, off + 1);
      sampleIdx = idxDim(:, dim) + off;
      lin = lin + (sampleIdx - 1) * strides(dim);
    end
    yq = yq + weight .* vv(lin);
  end
  vqEx = yq;
end
%=============================================================================
function Vext = padCubicGhostND(V, nd)
  Vext = V;
  for dim = 1:nd
    sz = size(Vext);
    if numel(sz) < nd
      sz(numel(sz) + 1:nd) = 1;
    end
    m = sz(dim);
    low1 = subsAlong(nd, dim, 1);
    low2 = subsAlong(nd, dim, 2);
    low3 = subsAlong(nd, dim, 3);
    ghostLow = 3 * Vext(low1{:}) - 3 * Vext(low2{:}) + Vext(low3{:});
    high1 = subsAlong(nd, dim, m);
    high2 = subsAlong(nd, dim, m - 1);
    high3 = subsAlong(nd, dim, m - 2);
    ghostHigh = 3 * Vext(high1{:}) - 3 * Vext(high2{:}) + Vext(high3{:});
    Vext = cat(dim, ghostLow, Vext, ghostHigh);
  end
end
%=============================================================================
function s = subsAlong(nd, dim, k)
  s = repmat({':'}, 1, nd);
  s{dim} = k;
end
%=============================================================================
function out = griddedInterpolantNearestND(gridVectors, values, queries)
  nd = numel(gridVectors);
  sz = size(queries{1});
  gridSize = zeros(1, nd);
  for dim = 1:nd
    gridSize(dim) = numel(gridVectors{dim});
  end
  strides = [1, cumprod(gridSize(1:end - 1))];
  linearIndex = ones(numel(queries{1}), 1);
  badMask = false(numel(queries{1}), 1);
  for dim = 1:nd
    g = gridVectors{dim}(:);
    n = numel(g);
    q = queries{dim}(:);
    badMask = badMask | isnan(q);
    clamped = min(max(q, g(1)), g(end));
    fractionalIndex = interp1(g, (1:n)', clamped, 'linear');
    idx = round(fractionalIndex);
    idx = min(max(idx, 1), n);
    linearIndex = linearIndex + (idx - 1) * strides(dim);
  end
  vv = values(:);
  out = vv(linearIndex);
  out(badMask) = NaN;
  out = reshape(out, sz);
end
%=============================================================================
function out = griddedInterpolantLinearND(gridVectors, values, queries)
  nd = numel(gridVectors);
  npts = numel(queries{1});
  gridSize = zeros(1, nd);
  grids = cell(1, nd);
  for dim = 1:nd
    grids{dim} = gridVectors{dim}(:);
    gridSize(dim) = numel(grids{dim});
  end
  strides = [1, cumprod(gridSize(1:end - 1))];
  vv = values(:);
  out = zeros(npts, 1);
  for p = 1:npts
    left = ones(1, nd);
    frac = zeros(1, nd);
    isBad = false;
    for dim = 1:nd
      g = grids{dim};
      n = numel(g);
      qd = queries{dim}(p);
      if isnan(qd)
        isBad = true;
        break
      end
      if qd <= g(1)
        left(dim) = 1;
      elseif qd >= g(end)
        left(dim) = n - 1;
      else
        left(dim) = find(g <= qd, 1, 'last');
        if left(dim) >= n
          left(dim) = n - 1;
        end
      end
      frac(dim) = (qd - g(left(dim))) / (g(left(dim) + 1) - g(left(dim)));
    end
    if isBad
      out(p) = NaN;
      continue
    end
    value = 0;
    for mask = 0:(2 ^ nd - 1)
      cornerIdx = left;
      weight = 1;
      for dim = 1:nd
        if bitand(mask, 2 ^ (dim - 1))
          cornerIdx(dim) = left(dim) + 1;
          weight = weight * frac(dim);
        else
          weight = weight * (1 - frac(dim));
        end
      end
      lin = 1 + sum((cornerIdx - 1) .* strides);
      value = value + weight * vv(lin);
    end
    out(p) = value;
  end
end
%=============================================================================
