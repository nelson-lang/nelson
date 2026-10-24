%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = categoricalConcatenate(dim, args)
  % Concatenate categorical arrays with text or missing values along dim.
  % The result keeps the categories of the first categorical operand and adds
  % the new ones in operand order; empty text and missing are undefined.
  C = args{find(cellfun(@(v) isa(v, 'categorical'), args), 1)};
  names = C.categoryNames;
  codes = cell(1, numel(args));
  for k = 1:numel(args)
    value = args{k};
    if isa(value, 'categorical')
      C.categoryNames = names;
      [codes{k}, names] = categorical.convertCodesForAssignment(C, value);
    elseif (isnumeric(value) || iscell(value)) && isequal(size(value), [0 0])
      codes{k} = zeros(0, 0);
    else
      [codes{k}, names] = labelsToCodes(textLabels(value), names, C);
    end
  end
  C.categoryNames = names;
  C.codes = builtin('cat', dim, codes{:});
end
%=============================================================================
function labels = textLabels(value)
  % Text of each value, '' where the value is undefined.
  if isa(value, 'missing')
    labels = repmat({''}, size(value));
  elseif ischar(value)
    labels = cellstr(value);
  elseif isstring(value)
    if ~isscalar(value)
      error('Nelson:categorical:concatenateStringArray', ...
        'Unable to concatenate a string array and a categorical array. Multiple categorical values represented as a string array are not converted automatically. Convert using the categorical function.');
    end
    if ismissing(value)
      labels = {''};
    else
      labels = {char(value)};
    end
  elseif iscell(value)
    if ~iscellstr(value)
      error('Nelson:categorical:concatenateCellNotText', ...
        'Unable to concatenate cell array and a categorical array unless the cell array contains only character vectors.');
    end
    labels = value;
  else
    error('Nelson:categorical:concatenateTypeMismatch', ...
      'Unable to concatenate a %s array and a categorical array.', class(value));
  end
end
%=============================================================================
function [codes, names] = labelsToCodes(labels, names, reference)
  % New labels become new categories, unless the categories are fixed.
  codes = zeros(size(labels));
  for k = 1:numel(labels)
    if isempty(labels{k})
      continue
    end
    idx = find(strcmp(names, labels{k}), 1);
    if isempty(idx)
      if reference.isOrdinal
        error('Nelson:categorical:concatenateOrdinalCategories', ...
          'Ordinal arrays must have the same categories, including their order.');
      end
      if reference.isProtected
        error('Nelson:categorical:concatenateProtectedCategories', ...
          'One or more of the arrays is categorical and either ordinal or protected, and the arrays do not all have the same sets of categories.  Use ADDCATS to add new categories.');
      end
      names{end + 1, 1} = labels{k};
      idx = numel(names);
    end
    codes(k) = idx;
  end
end
%=============================================================================
