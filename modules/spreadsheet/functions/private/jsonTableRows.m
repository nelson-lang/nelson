%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [rows, keys] = jsonTableRows(node, isArrayNode)
  % Normalizes the decoded table node into rows:
  %  - array of objects: struct array (same keys) or cell of structs (n x 1)
  %  - object whose values are all objects: one row per key (keys returned)
  %  - other object: a single row
  %  - array of scalars: one value per row
  keys = {};
  if isstruct(node)
    if isscalar(node) && ~isArrayNode && isKeyedObject(node)
      keys = fieldnames(node);
      rows = toStructRows(struct2cell(node));
      return
    end
    rows = asColumn(node);
    return
  end
  if iscell(node)
    rows = asColumn(node);
    isObject = cellfun('isclass', rows, 'struct') & cellfun('prodofsize', rows) == 1;
    if all(isObject)
      rows = toStructRows(rows);
    end
    return
  end
  if isempty(node) && ~isArrayNode
    error('Nelson:spreadsheet:jsonTableNotFound', 'No tabular data found in the JSON file.');
  end
  rows = asColumn(node);
end
%=============================================================================
function value = asColumn(value)
  if ~iscolumn(value)
    value = value(:);
  end
end
%=============================================================================
function tf = isKeyedObject(node)
  values = struct2cell(node);
  tf = ~isempty(values) && all(cellfun('isclass', values, 'struct')) ...
    && all(cellfun('prodofsize', values) == 1);
end
%=============================================================================
function rows = toStructRows(rows)
  % Concatenates objects sharing the same keys into a struct array.
  try
    rows = vertcat(rows{:});
  catch
  end
end
%=============================================================================
