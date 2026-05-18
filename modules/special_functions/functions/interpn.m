%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Vq = interpn(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 1);
  [args, method, extrapMode, extrapValue] = interp_parse_tail(varargin, true);
  if strcmp(method, 'pchip') && ~(numel(args) == 3 && isvector(args{1}) && isvector(args{2}))
    error(_('PCHIP method is supported only for 1-D interpolation.'));
  end

  if numel(args) == 1 || (numel(args) == 2 && isnumeric(args{2}) && isscalar(args{2}))
    V = args{1};
    if numel(args) == 1
      k = 1;
    else
      k = args{2};
    end
    if ~isscalar(k) || k < 0 || fix(k) ~= k
      error(_('Refinement factor must be a nonnegative integer scalar.'));
    end
    nd = ndims(V);
    grids = cell(1, nd);
    queries = cell(1, nd);
    step = 1 / (2^k);
    for dim = 1:nd
      grids{dim} = 1:size(V, dim);
      q = 1:step:size(V, dim);
      qShape = ones(1, nd);
      qShape(dim) = numel(q);
      queries{dim} = reshape(q, qShape);
    end
    Vq = interpn_core(grids, V, queries, method, extrapMode, extrapValue);
    return
  end

  if numel(args) == 3 && isvector(args{1}) && isvector(args{2})
    grids = {args{1}};
    V = args{2}(:);
    queries = {args{3}};
    Vq = interpn_core(grids, V, queries, method, extrapMode, extrapValue);
    return
  end

  Vdefault = args{1};
  ndDefault = ndims(Vdefault);
  if numel(args) == ndDefault + 1
    grids = cell(1, ndDefault);
    for dim = 1:ndDefault
      grids{dim} = 1:size(Vdefault, dim);
    end
    queries = args(2:end);
    Vq = interpn_core(grids, Vdefault, queries, method, extrapMode, extrapValue);
    return
  end

  if mod(numel(args), 2) == 1 && numel(args) >= 5
    nd = (numel(args) - 1) / 2;
    grids = args(1:nd);
    V = args{nd + 1};
    queries = args(nd + 2:end);
    grids = interpn_grid_vectors(grids);
    Vq = interpn_core(grids, V, queries, method, extrapMode, extrapValue);
  else
    error(_('Wrong number of input arguments.'));
  end
end
%=============================================================================
function grids = interpn_grid_vectors(gridArgs)
  nd = numel(gridArgs);
  grids = cell(1, nd);
  allVectors = true;
  for dim = 1:nd
    allVectors = allVectors && (numel(gridArgs{dim}) == length(gridArgs{dim}));
  end
  if allVectors
    for dim = 1:nd
      grids{dim} = gridArgs{dim}(:);
    end
  else
    baseSize = size(gridArgs{1});
    for dim = 1:nd
      if ~isequal(size(gridArgs{dim}), baseSize)
        error(_('Grid arrays must have the same size.'));
      end
      subs = repmat({1}, 1, nd);
      subs{dim} = ':';
      g = gridArgs{dim}(subs{:});
      grids{dim} = g(:);
    end
  end
end
%=============================================================================
