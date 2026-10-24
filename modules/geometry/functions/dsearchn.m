%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [idx, dist] = dsearchn(points, varargin)
  narginchk(2, 4);
  points = double(points);
  if length(varargin) == 1
    query = double(varargin{1});
    tri = [];
    useOutside = false;
    outind = NaN;
  else
    tri = double(varargin{1});
    query = double(varargin{2});
    useOutside = length(varargin) > 2;
    if useOutside
      outind = varargin{3};
    else
      outind = NaN;
    end
  end
  if size(query, 2) ~= size(points, 2) && size(query, 1) == size(points, 2)
    query = query.';
  end
  idx = NaN(size(query, 1), 1);
  dist = NaN(size(query, 1), 1);
  outside = false(size(query, 1), 1);
  if ~isempty(tri)
    outside = isnan(tsearchn(points, tri, query));
  end
  for k = 1:size(query, 1)
    if useOutside && outside(k)
      idx(k) = outind;
      continue
    end
    delta = points - query(k, :);
    d = sqrt(sum(delta .* delta, 2));
    [dist(k), idx(k)] = min(d);
  end
end
%=============================================================================
