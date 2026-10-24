%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [codes, missing] = tableVariableCodes(value, nRows, distinctMissing)
  % Replaces each column of a table variable by its sorted-order group codes
  % (one code column per variable column). Missing values get the largest
  % codes, so the numeric sortrows/unique of the code matrix follow the order
  % of each variable in its own type. missing flags the missing elements (an
  % empty char in a cellstr variable is a plain value, not a missing one).
  if ndims(value) > 2 || size(value, 2) ~= 1
    value = reshape(value, nRows, []);
  end
  nCols = size(value, 2);
  codes = zeros(nRows, nCols);
  for j = 1:nCols
    codes(:, j) = columnCodes(value(:, j), distinctMissing);
  end
  if nargout > 1
    missing = missingElements(value);
  end
end
%=============================================================================
function tf = missingElements(value)
  if isdatetime(value)
    tf = isnat(value);
  elseif isduration(value) || isfloat(value)
    tf = isnan(value);
  elseif iscategorical(value)
    tf = isundefined(value);
  elseif isstring(value)
    tf = ismissing(value);
  else
    tf = false(size(value));
  end
end
%=============================================================================
function g = columnCodes(column, distinctMissing)
  if iscell(column) && ~iscellstr(column)
    keys = cell(size(column, 1), 1);
    for r = 1:size(column, 1)
      keys{r} = tableValueKey(column(r, :));
    end
    [~, ~, g] = unique(keys);
  elseif iscell(column) || distinctMissing
    % an empty char in a cellstr variable is a plain value, not a missing one
    [~, ~, g] = unique(column);
  else
    [~, ~, g] = unique(column, 'TreatMissingAsDistinct', false);
  end
  g = double(g(:));
end
%=============================================================================
