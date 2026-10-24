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
  [query, outSize] = parseQuery(varargin{:});
  if strcmp(obj.Method, 'nearest')
    ids = dsearchn(obj.Points, query);
    vq = reshapeResult(obj.Values(ids), outSize);
    return
  end
  if isempty(obj.Triangulation)
    vq = [];
    return
  end
  [simplexId, bary] = tsearchn(obj.Points, obj.Triangulation, query);
  vq = NaN(size(query, 1), 1);
  for k = 1:size(query, 1)
    if isnan(simplexId(k))
      if strcmp(obj.ExtrapolationMethod, 'nearest')
        id = dsearchn(obj.Points, query(k, :));
        vq(k) = obj.Values(id);
      end
    else
      ids = obj.Triangulation(simplexId(k), :);
      vq(k) = bary(k, :) * obj.Values(ids(:));
    end
  end
  vq = reshapeResult(vq, outSize);
end
%=============================================================================
function [query, outSize] = parseQuery(varargin)
  if length(varargin) == 1
    % Single point-matrix form F(P): P is Npts-by-ndim, result stays a column.
    query = double(varargin{1});
    outSize = [];
  else
    % One array per coordinate: every coordinate argument shares the query
    % shape, and the result is returned with that same shape.
    outSize = size(varargin{1});
    query = zeros(numel(varargin{1}), length(varargin));
    for k = 1:length(varargin)
      coord = double(varargin{k});
      if ~isequal(size(coord), outSize)
        error('Nelson:scatteredInterpolant:sizeMismatch', ...
          _('The query coordinate arrays must all have the same size.'));
      end
      query(:, k) = coord(:);
    end
  end
end
%=============================================================================
function vq = reshapeResult(vq, outSize)
  if ~isempty(outSize)
    vq = reshape(vq, outSize);
  end
end
%=============================================================================
