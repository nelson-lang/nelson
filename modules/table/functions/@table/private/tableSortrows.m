%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [R, order] = tableSortrows(T, idx, direction)
  % Each sort variable is replaced by its sorted-order group codes, then the
  % numeric sortrows of the code matrix gives a stable lexicographic order in
  % which every variable follows its own type order. A code column holding
  % missing values is preceded by an always-ascending missing flag, so missing
  % values stay last in both directions.
  st = struct(T);
  nRows = height(T);
  names = st.Properties.VariableNames;
  idx = idx(:)';
  descending = parseDirection(direction, length(idx));
  codes = zeros(nRows, 0);
  signs = zeros(1, 0);
  for k = 1:length(idx)
    [varCodes, varMissing] = tableVariableCodes(st.data.(names{idx(k)}), nRows, false);
    for j = 1:size(varCodes, 2)
      if any(varMissing(:, j))
        codes = [codes, double(varMissing(:, j))];
        signs = [signs, 1];
      end
      codes = [codes, varCodes(:, j)];
      signs = [signs, 1 - 2 * descending(k)];
    end
  end
  if isempty(codes)
    order = (1:nRows)';
  else
    [~, order] = sortrows(codes, signs .* (1:size(codes, 2)));
    order = order(:);
  end
  R = T(order, :);
end
%=============================================================================
function descending = parseDirection(direction, nVars)
  if ischar(direction)
    direction = {direction};
  elseif isstring(direction)
    direction = cellstr(direction);
  end
  if ~iscellstr(direction) || (length(direction) ~= 1 && length(direction) ~= nVars)
    error('Nelson:table:sortrows:invalidDirection', 'Sorting direction must be ''ascend'' or ''descend'', or a cell array with one direction per sorting variable.');
  end
  descending = false(1, length(direction));
  for k = 1:length(direction)
    if strcmpi(direction{k}, 'descend')
      descending(k) = true;
    elseif ~strcmpi(direction{k}, 'ascend')
      error('Nelson:table:sortrows:invalidDirection', 'Sorting direction must be ''ascend'' or ''descend'', or a cell array with one direction per sorting variable.');
    end
  end
  if length(descending) == 1
    descending = repmat(descending, 1, nVars);
  end
end
%=============================================================================
