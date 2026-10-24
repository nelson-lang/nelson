%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = jsonTableRead(root, opts, cache)
  % Builds the table described by a JSONImportOptions object from the
  % decoded JSON document and the rows of its table node (cache, see
  % jsonTableCache) with the columns already extracted by jsonTableDetect.
  [selectors, names, types, selectedNames] = resolveVariables(opts, cache);
  selected = find(ismember(names, selectedNames));
  n = cache.Count;
  columns = cell(1, numel(selected));
  missingRows = false(n, 1);
  keep = true(1, numel(selected));
  for k = 1:numel(selected)
    index = selected(k);
    if index <= numel(cache.Columns)
      values = cache.Columns{index};
      vector = cache.Vectors{index};
    else
      [values, vector] = jsonTableColumn(cache, selectors{index});
    end
    [columns{k}, missing, bad] = jsonTableConvert(values, vector, types{index}, opts.RepeatedNodeRule);
    [missingRows, keepMissing] = applyRule(opts.MissingRule, missing, missingRows, names{index}, 'missing');
    [missingRows, keepValid] = applyRule(opts.ImportErrorRule, bad, missingRows, names{index}, 'conversion');
    keep(k) = keepMissing && keepValid;
  end
  rowNames = {};
  if ~isempty(opts.RowNamesSelector)
    [values, vector] = jsonTableColumn(cache, opts.RowNamesSelector);
    [rowNames, missing] = jsonTableConvert(values, vector, 'char', 'ignore');
    missingRows = missingRows | missing;
  end
  columns = columns(keep);
  selected = selected(keep);
  if any(missingRows)
    for k = 1:numel(columns)
      columns{k} = columns{k}(~missingRows, :);
    end
    if ~isempty(rowNames)
      rowNames = rowNames(~missingRows);
    end
  end
  args = {'VariableNames', names(selected)};
  if ~isempty(rowNames)
    args = [args, {'RowNames', rowNames}];
  end
  T = table(columns{:}, args{:});
  T = setMetadata(T, root, opts.VariableUnitsSelector, 'VariableUnits', names(selected));
  T = setMetadata(T, root, opts.VariableDescriptionsSelector, 'VariableDescriptions', names(selected));
end
%=============================================================================
function [selectors, names, types, selectedNames] = resolveVariables(opts, cache)
  selectors = opts.VariableSelectors;
  names = opts.VariableNames;
  types = opts.VariableTypes;
  selectedNames = opts.SelectedVariableNames;
  if ~isempty(selectors)
    return
  end
  % No selectors: the leaf nodes are read, with the option names and types
  % when their count matches, otherwise Var1, Var2, ... as text.
  selectors = jsonTableLeaves(cache);
  if numel(selectors) ~= numel(names)
    names = cellstr(string('Var') + string(1:numel(selectors)));
    types = repmat({'char'}, 1, numel(selectors));
    selectedNames = names;
  end
end
%=============================================================================
function [omitRows, keep] = applyRule(rule, mask, omitRows, name, kind)
  keep = true;
  if ~any(mask)
    return
  end
  switch rule
    case 'error'
      error('Nelson:spreadsheet:jsonImportRule', 'Unable to import variable ''%s'': %s value found.', name, kind);
    case 'omitrow'
      omitRows = omitRows | mask;
    case 'omitvar'
      keep = false;
  end
end
%=============================================================================
function T = setMetadata(T, root, selector, property, names)
  if isempty(selector)
    return
  end
  [value, found] = jsonPointerGet(root, selector);
  if ~found
    return
  end
  metadata = repmat({''}, 1, numel(names));
  if isstruct(value) && isscalar(value)
    for k = 1:numel(names)
      field = jsonFieldName(value, names{k});
      if ~isempty(field) && ischar(value.(field))
        metadata{k} = value.(field);
      end
    end
  elseif iscellstr(value) && numel(value) == numel(names)
    metadata = value(:)';
  else
    return
  end
  T.Properties.(property) = metadata;
end
%=============================================================================
