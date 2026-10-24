%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Vq = interpn_core(grids, V, queries, method, extrapMode, extrapValue)
  if nargin < 4 || isempty(method)
    method = 'linear';
  end
  if nargin < 5 || isempty(extrapMode)
    extrapMode = 'default';
  end
  if nargin < 6
    extrapValue = NaN;
  end
  nativeMethod = method;
  nativeExtrapMode = extrapMode;
  if strcmp(nativeExtrapMode, 'default') && any(strcmp(nativeMethod, {'spline', 'makima'}))
    nativeExtrapMode = 'extrap';
  end
  if ~any(strcmp(method, {'linear', 'nearest', 'cubic', 'makima', 'spline', 'pchip'}))
    error('Nelson:interp:unsupportedMethod', 'Unsupported interpolation method.');
  end
  if ~isnumeric(V)
    error('Nelson:interpn:numericSampleValuesExpected', 'Numeric sample values expected.');
  end
  if issparse(V)
    error('Nelson:special_functions:denseExpected', 'dense type for all input arguments expected.');
  end

  nd = numel(grids);
  if numel(queries) ~= nd
    error('Nelson:interpn:invalidQueryGridCount', 'Wrong number of query grids.');
  end
  dataSize = size(V);
  if numel(dataSize) < nd
    dataSize(end + 1:nd) = 1;
  end
  for dim = 1:nd
    g = full(grids{dim}(:));
    if ~isnumeric(g) || ~isreal(g) || issparse(g)
      error('Nelson:interpn:invalidGridVectors', 'Grid vectors must be real dense numeric vectors.');
    end
    if numel(g) ~= dataSize(dim)
      error('Nelson:interp:gridSizeMismatch', 'Grid vector length and sample value size are incompatible.');
    end
    dg = g(2:end) - g(1:end - 1);
    if any(dg == 0) || ~(all(dg > 0) || all(dg < 0))
      error('Nelson:interpn:gridVectorsNotMonotonic', 'Grid vectors must be strictly monotonic.');
    end
    if dg(1) < 0
      g = flipud(g);
      V = flipdim(V, dim);
    end
    grids{dim} = g;
  end

  if strcmp(method, 'makima')
    % The native builtin evaluates 'makima' with a value-independent Lagrange
    % stencil (identical to 'cubic'), which is not the modified-Akima result
    % because the Akima slopes depend non-linearly on the sample values.
    % Evaluate a genuine tensor-product modified-Akima interpolant here.
    effectiveExtrap = extrapMode;
    if strcmp(effectiveExtrap, 'default')
      effectiveExtrap = 'extrap';
    end
    Vq = interpn_makima_core(grids, V, queries, effectiveExtrap, extrapValue, nd);
    return
  end

  if any(strcmp(nativeMethod, {'linear', 'nearest', 'cubic', 'makima', 'spline', 'pchip'}))
    try
      Vq = __interpn__(V, nativeMethod, nativeExtrapMode, extrapValue, grids{:}, queries{:});
      return
    catch
    end
  end
  if any(strcmp(method, {'cubic', 'makima', 'spline', 'pchip'}))
    if strcmp(extrapMode, 'default') && any(strcmp(method, {'spline', 'makima'}))
      extrapMode = 'extrap';
    end
    method = 'linear';
  end

  dataSize = size(V);
  if numel(dataSize) < nd
    dataSize(end + 1:nd) = 1;
  end
  gridSize = dataSize(1:nd);
  extraSize = dataSize(nd + 1:end);
  if isempty(extraSize)
    extraSize = 1;
  end
  pageCount = prod(extraSize);
  Vmat = reshape(V, prod(gridSize), pageCount);

  sameSize = true;
  qSize = size(queries{1});
  for dim = 1:nd
    if ~isnumeric(queries{dim}) || ~isreal(queries{dim}) || issparse(queries{dim})
      error('Nelson:interpn:invalidQueryPoints', 'Query points must be real dense numeric arrays.');
    end
    sameSize = sameSize && isequal(size(queries{dim}), qSize);
  end

  queryAsGrid = false;
  if ~sameSize
    queryAsGrid = true;
    qSize = zeros(1, nd);
    for dim = 1:nd
      if ~interpn_is_query_vector(queries{dim})
        error('Nelson:dimagree:valueArraysSize', '%s arrays must have the same size.', 'Query');
      end
      qSize(dim) = numel(queries{dim});
      queries{dim} = full(queries{dim}(:));
    end
  else
    for dim = 1:nd
      queries{dim} = full(queries{dim});
    end
  end

  totalQueries = prod(qSize);
  out = zeros(totalQueries, pageCount) + 0 * Vmat(1, 1);
  strides = [1, cumprod(gridSize(1:end - 1))];
  defaultExtrap = any(strcmp(method, {'spline', 'makima'}));

  for qi = 1:totalQueries
    q = zeros(1, nd);
    if queryAsGrid
      remIndex = qi - 1;
      for dim = 1:nd
        sub = mod(remIndex, qSize(dim)) + 1;
        remIndex = floor(remIndex / qSize(dim));
        q(dim) = queries{dim}(sub);
      end
    else
      for dim = 1:nd
        qd = queries{dim};
        q(dim) = qd(qi);
      end
    end

    left = ones(1, nd);
    frac = zeros(1, nd);
    nearest = ones(1, nd);
    outside = false;
    for dim = 1:nd
      g = grids{dim};
      if isnan(q(dim)) || q(dim) < g(1) || q(dim) > g(end)
        outside = true;
      end
      if q(dim) <= g(1)
        left(dim) = 1;
      elseif q(dim) >= g(end)
        left(dim) = numel(g) - 1;
      else
        left(dim) = find(g <= q(dim), 1, 'last');
        if left(dim) >= numel(g)
          left(dim) = numel(g) - 1;
        end
      end
      frac(dim) = (q(dim) - g(left(dim))) / (g(left(dim) + 1) - g(left(dim)));
      if abs(q(dim) - g(left(dim))) <= abs(g(left(dim) + 1) - q(dim))
        nearest(dim) = left(dim);
      else
        nearest(dim) = left(dim) + 1;
      end
    end

    if outside && strcmp(extrapMode, 'constant')
      out(qi, :) = extrapValue;
      continue
    elseif outside && strcmp(extrapMode, 'default') && ~defaultExtrap
      out(qi, :) = NaN;
      continue
    end

    if strcmp(method, 'nearest')
      lin = 1 + sum((nearest - 1) .* strides);
      out(qi, :) = Vmat(lin, :);
    else
      value = zeros(1, pageCount) + 0 * Vmat(1, 1);
      for mask = 0:(2 ^ nd - 1)
        idx = left;
        weight = 1;
        for dim = 1:nd
          if bitand(mask, 2 ^ (dim - 1))
            idx(dim) = left(dim) + 1;
            weight = weight * frac(dim);
          else
            weight = weight * (1 - frac(dim));
          end
        end
        lin = 1 + sum((idx - 1) .* strides);
        value = value + weight * Vmat(lin, :);
      end
      out(qi, :) = value;
    end
  end

  if isequal(extraSize, 1)
    Vq = reshape(out(:, 1), qSize);
  else
    Vq = reshape(out, [qSize, extraSize]);
  end
end
%=============================================================================
function tf = interpn_is_query_vector(q)
  sz = size(q);
  tf = nnz(sz > 1) <= 1;
end
%=============================================================================
function Vq = interpn_makima_core(grids, V, queries, extrapMode, extrapValue, nd)
  % Tensor-product modified-Akima evaluation. The interpolant is separable:
  % along each dimension it is the 1-D modified-Akima piecewise cubic Hermite
  % spline, applied dimension by dimension. Because the Akima slopes depend on
  % the sample values, this cannot be written as a value-independent stencil.
  dataSize = size(V);
  if numel(dataSize) < nd
    dataSize(end + 1:nd) = 1;
  end
  gridSize = dataSize(1:nd);
  extraSize = dataSize(nd + 1:end);
  if isempty(extraSize)
    extraSize = 1;
  end
  pageCount = prod(extraSize);
  Vmat = reshape(V, prod(gridSize), pageCount);
  strides = [1, cumprod(gridSize(1:end - 1))];

  sameSize = true;
  qSize = size(queries{1});
  for dim = 1:nd
    if ~isnumeric(queries{dim}) || ~isreal(queries{dim}) || issparse(queries{dim})
      error('Nelson:interpn:invalidQueryPoints', 'Query points must be real dense numeric arrays.');
    end
    sameSize = sameSize && isequal(size(queries{dim}), qSize);
  end

  queryAsGrid = false;
  if ~sameSize
    queryAsGrid = true;
    qSize = zeros(1, nd);
    for dim = 1:nd
      if ~interpn_is_query_vector(queries{dim})
        error('Nelson:dimagree:valueArraysSize', '%s arrays must have the same size.', 'Query');
      end
      qSize(dim) = numel(queries{dim});
      queries{dim} = full(queries{dim}(:));
    end
  else
    for dim = 1:nd
      queries{dim} = full(queries{dim});
    end
  end

  totalQueries = prod(qSize);
  out = zeros(totalQueries, pageCount) + 0 * Vmat(1, 1);
  constantMode = strcmp(extrapMode, 'constant');

  for qi = 1:totalQueries
    q = zeros(1, nd);
    if queryAsGrid
      remIndex = qi - 1;
      for dim = 1:nd
        sub = mod(remIndex, qSize(dim)) + 1;
        remIndex = floor(remIndex / qSize(dim));
        q(dim) = queries{dim}(sub);
      end
    else
      for dim = 1:nd
        qd = queries{dim};
        q(dim) = qd(qi);
      end
    end

    outside = false;
    hasNaN = false;
    for dim = 1:nd
      g = grids{dim};
      if isnan(q(dim))
        hasNaN = true;
      elseif q(dim) < g(1) || q(dim) > g(end)
        outside = true;
      end
    end
    if hasNaN
      out(qi, :) = NaN;
      continue
    end
    if outside && constantMode
      out(qi, :) = extrapValue;
      continue
    end

    out(qi, :) = interpn_makima_recurse(1, 0, grids, Vmat, gridSize, strides, q, nd);
  end

  if isequal(extraSize, 1)
    Vq = reshape(out(:, 1), qSize);
  else
    Vq = reshape(out, [qSize, extraSize]);
  end
end
%=============================================================================
function row = interpn_makima_recurse(dim, baseOffset, grids, Vmat, gridSize, strides, q, nd)
  % Collapse dimension 'dim' by evaluating a 1-D modified-Akima interpolant at
  % q(dim) over the node values gathered along that dimension. Dimensions
  % 1..dim-1 are already fixed through baseOffset (a 0-based linear offset).
  n = gridSize(dim);
  if dim == nd
    rows = baseOffset + (0:n - 1)' * strides(dim) + 1;
    nodeVals = Vmat(rows, :);
  else
    pageCount = size(Vmat, 2);
    nodeVals = zeros(n, pageCount) + 0 * Vmat(1, 1);
    for j = 1:n
      nodeVals(j, :) = interpn_makima_recurse(dim + 1, ...
        baseOffset + (j - 1) * strides(dim), grids, Vmat, gridSize, strides, q, nd);
    end
  end
  row = interpn_makima_1d(grids{dim}, nodeVals, q(dim));
end
%=============================================================================
function row = interpn_makima_1d(g, nodeVals, xq)
  % 1-D modified-Akima piecewise cubic Hermite evaluation of the columns of
  % nodeVals (one column per output page) at the scalar query xq. Points
  % outside the grid are extrapolated using the boundary segment.
  n = numel(g);
  if n == 1
    row = nodeVals(1, :);
    return
  end
  if xq <= g(1)
    left = 1;
  elseif xq >= g(n)
    left = n - 1;
  else
    left = find(g <= xq, 1, 'last');
    if left >= n
      left = n - 1;
    end
  end
  d = interpn_makima_slopes(g, nodeVals);
  h = g(left + 1) - g(left);
  t = (xq - g(left)) / h;
  h00 = (2 * t ^ 3) - (3 * t ^ 2) + 1;
  h10 = (t ^ 3) - (2 * t ^ 2) + t;
  h01 = (-2 * t ^ 3) + (3 * t ^ 2);
  h11 = (t ^ 3) - (t ^ 2);
  row = h00 * nodeVals(left, :) + h10 * h * d(left, :) ...
    + h01 * nodeVals(left + 1, :) + h11 * h * d(left + 1, :);
end
%=============================================================================
function d = interpn_makima_slopes(g, y)
  % Modified-Akima slopes at the nodes for every column of y. The classic
  % Akima weights are augmented with the '+ |sum| / 2' term so that regions of
  % equal but non-zero consecutive slopes do not produce the 0/0 form of the
  % original Akima scheme.
  n = numel(g);
  p = size(y, 2);
  d = zeros(n, p) + 0 * y(1);
  if n == 2
    delta = (y(2, :) - y(1, :)) / (g(2) - g(1));
    d(1, :) = delta;
    d(2, :) = delta;
    return
  end
  h = diff(g);
  delta = diff(y, 1, 1) ./ repmat(h, 1, p);
  m = zeros(n + 3, p) + 0 * y(1);
  m(3:n + 1, :) = delta;
  m(2, :) = 2 * m(3, :) - m(4, :);
  m(1, :) = 2 * m(2, :) - m(3, :);
  m(n + 2, :) = 2 * m(n + 1, :) - m(n, :);
  m(n + 3, :) = 2 * m(n + 2, :) - m(n + 1, :);
  for i = 1:n
    w1 = abs(m(i + 3, :) - m(i + 2, :)) + abs(m(i + 3, :) + m(i + 2, :)) / 2;
    w2 = abs(m(i + 1, :) - m(i, :)) + abs(m(i + 1, :) + m(i, :)) / 2;
    denom = w1 + w2;
    di = (w1 .* m(i + 1, :) + w2 .* m(i + 2, :)) ./ denom;
    di(denom == 0) = 0;
    d(i, :) = di;
  end
end
%=============================================================================
