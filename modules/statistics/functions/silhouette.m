%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [s, h] = silhouette(X, clust, distance)
  narginchk(2, 3);
  nargoutchk(0, 2);

  if nargin < 3
    distance = 'sqeuclidean';
  end
  if size(X, 1) ~= length(clust)
    error('Nelson:statistics:CLUSTOneValueRowX', 'CLUST must have one value per row of X.');
  end
  clust = clust(:);
  labels = unique(clust);
  D = pdist2(X, X, distance);
  s = zeros(size(X, 1), 1);
  for i = 1:size(X, 1)
    own = clust == clust(i);
    own(i) = false;
    if ~any(own)
      s(i) = 0;
      continue
    end
    a = meanDistance(D(i, own));
    b = Inf;
    for j = 1:length(labels)
      if labels(j) ~= clust(i)
        b = min(b, meanDistance(D(i, clust == labels(j))));
      end
    end
    if isinf(b) || (a == 0 && b == 0)
      s(i) = 0;
    else
      s(i) = (b - a) / max(a, b);
    end
  end
  h = [];
  if nargout == 0
    disp(s);
    clear s h;
  end
end
%=============================================================================
function value = meanDistance(values)
  if isempty(values)
    value = 0;
  else
    value = mean(values);
  end
end
%=============================================================================
