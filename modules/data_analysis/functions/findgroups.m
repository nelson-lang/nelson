%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [G, varargout] = findgroups(varargin)
  narginchk(1, inf);
  nargoutchk(0, nargin + 1);
  varargout = {};
  nv = nargin;
  sz = size(varargin{1});
  n = numel(varargin{1});
  missingMask = false(n, 1);
  idxCols = zeros(n, nv);
  for j = 1:nv
    xj = varargin{j}(:);
    if numel(xj) ~= n
      error('Nelson:data_analysis:allGroupingVariablesSameNumber', 'All grouping variables must have the same number of rows.');
    end
    missingMask = missingMask | ismissing(xj);
    [~, ~, ic] = unique(xj);
    idxCols(:, j) = ic;
  end
  [~, ~, groupOfRow] = unique(idxCols, 'rows');
  G = double(groupOfRow);
  G(missingMask) = NaN;
  valid = ~isnan(G);
  validGroups = unique(G(valid));
  if isempty(validGroups)
    G = reshape(G, sz);
  else
    remap = zeros(max(validGroups), 1);
    remap(validGroups) = 1:numel(validGroups);
    G(valid) = remap(G(valid));
    G = reshape(G, sz);
  end
  if nargout > 1
    Gv = G(:);
    validIdx = find(~isnan(Gv));
    % First row of each group in group-id order: a single stable unique of the
    % valid group ids instead of a find() scan per group (which was O(n^2)).
    [~, ia] = unique(Gv(validIdx), 'first');
    firstRows = validIdx(ia);
    for j = 1:nv
      xj = varargin{j}(:);
      varargout{j} = xj(firstRows);
    end
  end
end
%=============================================================================
