%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function D = dummyvar(group)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if iscell(group) && ~(iscellstr(group) && isvector(group))
    D = dummyvarCellGroupLocal(group);
  else
    D = dummyvarOneGroupLocal(group);
  end
end
%=============================================================================
function D = dummyvarCellGroupLocal(group)
  if isempty(group)
    D = zeros(0, 0);
    return
  end
  D = [];
  n = [];
  for k = 1:numel(group)
    block = dummyvarOneGroupLocal(group{k});
    if isempty(n)
      n = size(block, 1);
    elseif size(block, 1) ~= n
      error('Nelson:statistics:groupingVariablesSameNumberRows', '%s must have the same number of rows.', 'Grouping variables');
    end
    D = [D, block];
  end
end
%=============================================================================
function D = dummyvarOneGroupLocal(group)
  if isnumeric(group) || islogical(group)
    D = dummyvarNumericLocal(group);
  elseif iscategorical(group)
    D = dummyvarCategoricalLocal(group);
  elseif isstring(group)
    D = dummyvarTextLocal(cellstr(group(:)));
  elseif ischar(group)
    D = dummyvarTextLocal(cellstr(group));
  elseif iscellstr(group)
    D = dummyvarTextLocal(group(:));
  else
    error('Nelson:statistics:GROUPGroupingVariable', '%s must be a grouping variable.', 'GROUP');
  end
end
%=============================================================================
function D = dummyvarNumericLocal(group)
  if ~(isvector(group) || ndims(group) == 2)
    error('Nelson:statistics:GROUPVectorMatrix', '%s must be a vector or matrix.', 'GROUP');
  end
  outType = classForOutputLocal(group);
  values = double(group);
  if any(values(~isnan(values)) < 1) || any(values(~isnan(values)) ~= fix(values(~isnan(values))))
    error('Nelson:statistics:numericGROUPValuesPositiveIntegers', 'Numeric GROUP values must be positive integers or NaN.');
  end
  if isvector(values)
    values = values(:);
  end
  D = zeros(size(values, 1), 0, outType);
  for col = 1:size(values, 2)
    column = values(:, col);
    if all(isnan(column))
      levels = 0;
    else
      levels = max(column(~isnan(column)));
    end
    block = zeros(size(values, 1), levels, outType);
    missing = isnan(column);
    if any(missing)
      block(missing, :) = NaN;
    end
    for level = 1:levels
      rows = column == level;
      if any(rows)
        block(rows, level) = 1;
      end
    end
    D = [D, block];
  end
end
%=============================================================================
function D = dummyvarCategoricalLocal(group)
  names = categories(group);
  labels = cellstr(group(:));
  D = labelsToDummyLocal(labels, names(:), true);
end
%=============================================================================
function D = dummyvarTextLocal(labels)
  labels = labels(:);
  missing = strcmp(labels, '');
  names = unique(labels(~missing), 'stable');
  D = labelsToDummyLocal(labels, names(:), true);
end
%=============================================================================
function D = labelsToDummyLocal(labels, names, emptyIsMissing)
  labels = labels(:);
  D = zeros(length(labels), length(names));
  missing = false(length(labels), 1);
  if emptyIsMissing
    missing = strcmp(labels, '') | strcmp(labels, '<undefined>');
  end
  if any(missing)
    D(missing, :) = NaN;
  end
  for k = 1:length(names)
    rows = strcmp(labels, names{k});
    if any(rows)
      D(rows, k) = 1;
    end
  end
end
%=============================================================================
function outType = classForOutputLocal(group)
  if isa(group, 'single')
    outType = 'single';
  else
    outType = 'double';
  end
end
%=============================================================================
