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
    error(_('Unsupported interpolation method.'));
  end
  if ~isnumeric(V)
    error(_('Numeric sample values expected.'));
  end
  if issparse(V)
    error(_('dense type for all input arguments expected.'));
  end

  nd = numel(grids);
  if numel(queries) ~= nd
    error(_('Wrong number of query grids.'));
  end
  dataSize = size(V);
  if numel(dataSize) < nd
    dataSize(end + 1:nd) = 1;
  end
  for dim = 1:nd
    g = full(grids{dim}(:));
    if ~isnumeric(g) || ~isreal(g) || issparse(g)
      error(_('Grid vectors must be real dense numeric vectors.'));
    end
    if numel(g) ~= dataSize(dim)
      error(_('Grid vector length and sample value size are incompatible.'));
    end
    dg = diff(g);
    if any(dg == 0) || ~(all(dg > 0) || all(dg < 0))
      error(_('Grid vectors must be strictly monotonic.'));
    end
    if dg(1) < 0
      g = flipud(g);
      V = flipdim(V, dim);
    end
    grids{dim} = g;
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
      error(_('Query points must be real dense numeric arrays.'));
    end
    sameSize = sameSize && isequal(size(queries{dim}), qSize);
  end

  queryAsGrid = false;
  if ~sameSize
    queryAsGrid = true;
    qSize = zeros(1, nd);
    for dim = 1:nd
      if ~interpn_is_query_vector(queries{dim})
        error(_('Query arrays must have the same size.'));
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
      for mask = 0:(2^nd - 1)
        idx = left;
        weight = 1;
        for dim = 1:nd
          if bitand(mask, 2^(dim - 1))
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
