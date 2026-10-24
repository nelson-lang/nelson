%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pointers = jsonTableLeaves(cache)
  % Detects the table variables as JSON Pointers to the leaf nodes of the
  % row objects (cache, see jsonTableCache). Nested objects are flattened,
  % keys keep their first appearance order. Rows that are not objects give
  % a single variable.
  if ~isempty(cache.Columnar)
    pointers = strcat('/', escapeKey(cache.Columnar.Names));
    return
  end
  rows = cache.Rows;
  if isstruct(rows)
    pointers = structLeaves(rows, '');
  elseif iscell(rows) && any(cellfun('isclass', rows, 'struct'))
    pointers = cellLeaves(rows, '');
  else
    pointers = {''};
  end
end
%=============================================================================
function pointers = structLeaves(rows, prefix)
  pointers = {};
  names = fieldnames(rows);
  for k = 1:numel(names)
    pointer = [prefix, '/', escapeKey(names{k})];
    [~, isVector] = jsonFastVector(rows, names{k});
    if isVector
      pointers = [pointers, {pointer}];
    else
      pointers = [pointers, valueLeaves({rows.(names{k})}', pointer)];
    end
  end
end
%=============================================================================
function pointers = cellLeaves(rows, prefix)
  names = {};
  for k = 1:numel(rows)
    if isstruct(rows{k}) && isscalar(rows{k})
      rowNames = fieldnames(rows{k});
      names = [names; rowNames(~ismember(rowNames, names))];
    end
  end
  pointers = {};
  for k = 1:numel(names)
    values = cell(numel(rows), 1);
    for r = 1:numel(rows)
      if isstruct(rows{r}) && isscalar(rows{r}) && isfield(rows{r}, names{k})
        values{r} = rows{r}.(names{k});
      end
    end
    pointers = [pointers, valueLeaves(values, [prefix, '/', escapeKey(names{k})])];
  end
end
%=============================================================================
function pointers = valueLeaves(values, pointer)
  isObject = cellfun('isclass', values, 'struct') & cellfun('prodofsize', values) == 1;
  isNull = cellfun('isclass', values, 'double') & cellfun('isempty', values);
  if ~any(isObject) || ~all(isObject | isNull)
    pointers = {pointer};
    return
  end
  if all(isObject)
    try
      pointers = structLeaves(vertcat(values{:}), pointer);
      return
    catch
    end
  end
  pointers = cellLeaves(values(isObject), pointer);
end
%=============================================================================
function key = escapeKey(key)
  key = strrep(strrep(key, '~', '~0'), '/', '~1');
end
%=============================================================================
