%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [values, vector] = jsonTableColumn(cache, pointer)
  % Extracts, for each row of the table node (cache, see jsonTableCache),
  % the decoded value selected by a JSON Pointer relative to the row.
  % Absent nodes are returned as [] (n x 1 cell).
  % When every selected value is one number or one logical, 'vector' holds
  % them (n x 1) and 'values' is empty: no per-row cell is built.
  % Text only columns decoded by __jsondecode_columns__ are string arrays.
  rows = cache.Rows;
  keys = cache.Keys;
  n = cache.Count;
  vector = [];
  if strcmp(pointer, 'Keys')
    if isempty(keys)
      values = cellstr(string((0:n - 1)'));
    else
      values = keys(:);
    end
    return
  end
  tokens = jsonPointerTokens(pointer);
  if ~isempty(cache.Columnar)
    [values, vector] = fromColumns(cache.Columnar, tokens{1}, n);
  elseif isempty(tokens)
    if iscell(rows)
      values = rows;
    else
      values = num2cell(rows);
    end
  elseif isstruct(rows)
    [values, vector] = fromStructArray(rows, tokens);
  else
    values = fromCellRows(rows, tokens);
  end
end
%=============================================================================
function [values, vector] = fromColumns(columnar, key, n)
  % one top-level key (see jsonTableColumnar)
  values = {};
  vector = [];
  index = find(strcmp(columnar.Names, jsonFieldName(columnar.Names, key)), 1);
  if isempty(index)
    values = cell(n, 1);
  elseif isnumeric(columnar.Columns{index}) || islogical(columnar.Columns{index})
    vector = columnar.Columns{index};
  else
    values = columnar.Columns{index};
  end
end
%=============================================================================
function [values, vector] = fromStructArray(rows, tokens)
  vector = [];
  name = jsonFieldName(rows, tokens{1});
  if isempty(name)
    values = cell(numel(rows), 1);
    return
  end
  if isscalar(tokens)
    [vector, ok] = jsonFastVector(rows, name);
    if ok
      values = {};
      return
    end
  end
  values = {rows.(name)}';
  if isscalar(tokens)
    return
  end
  isObject = cellfun('isclass', values, 'struct') & cellfun('prodofsize', values) == 1;
  if all(isObject)
    try
      [values, vector] = fromStructArray(vertcat(values{:}), tokens(2:end));
      return
    catch
    end
  end
  values = fromCellRows(values, tokens(2:end));
end
%=============================================================================
function values = fromCellRows(rows, tokens)
  values = cell(numel(rows), 1);
  for k = 1:numel(rows)
    values{k} = jsonPointerGet(rows{k}, tokens);
  end
end
%=============================================================================
