%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function labels = graphicsLabelsFromValue(value, expectedCount)
  if nargin < 2
    expectedCount = numel(value);
  end
  if iscellstr(value)
    labels = value(:);
  elseif isstring(value)
    labels = cellstr(value(:));
  elseif ischar(value)
    labels = cellstr(value);
  elseif isnumeric(value) || islogical(value)
    value = value(:);
    labels = cell(numel(value), 1);
    for k = 1:numel(value)
      labels{k} = num2str(value(k));
    end
  elseif exist('iscategorical', 'file') && iscategorical(value)
    labels = cellstr(value(:));
  else
    error('Nelson:graphicsLabelsFromValue:invalidLabels', 'Labels must be character, string, cell string, categorical, or numeric values.');
  end
  if expectedCount >= 0 && numel(labels) ~= expectedCount
    error('Nelson:graphicsLabelsFromValue:invalidNumberOfLabels', 'Number of labels must match data.');
  end
end
%=============================================================================
