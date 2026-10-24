%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [column, missing, bad] = jsonTableConvert(values, vector, type, repeatedRule)
  % Converts decoded JSON values (n x 1 cell) into a table variable of the
  % requested type. JSON arrays (repeated nodes) become matrix columns
  % ('addcol'), keep their first element ('ignore') or raise an error.
  % missing: absent or null nodes; bad: values that cannot be converted.
  % 'vector' (n x 1 numbers or logicals, values empty) is the fast path.
  if isempty(values) && ~isempty(vector)
    if any(strcmp(type, {'double', 'single', 'int8', 'int16', 'int32', 'int64', ...
        'uint8', 'uint16', 'uint32', 'uint64', 'logical'}))
      [column, missing, bad] = fromVector(vector, type);
      return
    end
    values = num2cell(vector);
  end
  if isstring(values)
    % text only column (no null, no absent node)
    missing = false(size(values));
    bad = missing;
    switch type
      case {'string', 'datetime', 'duration'}
        column = values;
        return
      case 'char'
        column = cellstr(values);
        return
      case 'categorical'
        column = categorical(values);
        return
    end
    values = cellstr(values);
  end
  n = numel(values);
  counts = cellfun('prodofsize', values);
  isDouble = cellfun('isclass', values, 'double');
  isLogical = cellfun('isclass', values, 'logical');
  isText = cellfun('isclass', values, 'char');
  isCellText = cellfun('isclass', values, 'cell');
  missing = isDouble & counts == 0;
  isText = isText & ~missing;
  counts(isText) = 1;
  if strcmp(repeatedRule, 'error') && any(counts > 1)
    error('Nelson:spreadsheet:jsonRepeatedNode', 'Repeated JSON nodes found (RepeatedNodeRule is ''error'').');
  end
  width = max([1; counts(:)]);
  if strcmp(repeatedRule, 'ignore')
    width = 1;
  end
  switch type
    case {'double', 'single', 'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64'}
      [column, bad] = toNumeric(values, isDouble | isLogical, isText, counts, width);
      if ~strcmp(type, 'double')
        column = cast(column, type);
      end
    case 'logical'
      [column, bad] = toNumeric(values, isDouble | isLogical, false(n, 1), counts, width);
      bad = bad | any(~isnan(column) & column ~= 0 & column ~= 1, 2);
      column(isnan(column)) = 0;
      column = logical(column);
    case {'string', 'char', 'categorical', 'datetime', 'duration'}
      [column, bad] = toText(values, isText, isCellText, isDouble | isLogical, width);
      if strcmp(type, 'char')
        column(ismissing(column)) = "";
        column = cellstr(column);
      elseif strcmp(type, 'categorical')
        column = categorical(column);
      end
    otherwise
      column = values;
      bad = false(n, 1);
  end
end
%=============================================================================
function [column, missing, bad] = fromVector(vector, type)
  missing = false(size(vector));
  bad = false(size(vector));
  if strcmp(type, 'logical')
    bad = vector ~= 0 & vector ~= 1;
    column = logical(vector ~= 0);
  else
    column = cast(vector, type);
  end
end
%=============================================================================
function [column, bad] = toNumeric(values, isNumber, isText, counts, width)
  n = numel(values);
  column = NaN(n, width);
  bad = ~isNumber & ~isText & counts > 0;
  scalarRows = isNumber & counts == 1;
  if any(scalarRows)
    column(scalarRows, 1) = double([values{scalarRows}]');
  end
  if any(isText)
    parsed = str2double(values(isText));
    textRows = find(isText);
    column(textRows, 1) = parsed;
    bad(textRows(isnan(parsed) & ~strcmpi(values(isText), 'NaN'))) = true;
  end
  multiple = find(isNumber & counts > 1)';
  for k = multiple
    row = values{k};
    row = double(row(:)');
    m = min(numel(row), width);
    column(k, 1:m) = row(1:m);
  end
end
%=============================================================================
function [column, bad] = toText(values, isText, isCellText, isNumber, width)
  n = numel(values);
  column = repmat(string(missing), n, width);
  bad = false(n, 1);
  if all(isText)
    column(:, 1) = string(values);
    return
  elseif any(isText)
    column(isText, 1) = string(values(isText));
  end
  numbers = find(isNumber & cellfun('prodofsize', values) == 1)';
  for k = numbers
    column(k, 1) = string(values{k});
  end
  cells = find(isCellText)';
  for k = cells
    entries = values{k};
    entries = entries(:)';
    entries = entries(1:min(numel(entries), width));
    if iscellstr(entries)
      column(k, 1:numel(entries)) = string(entries);
    else
      bad(k) = true;
    end
  end
  bad = bad | (~isText & ~isCellText & ~isNumber & cellfun('prodofsize', values) > 0);
end
%=============================================================================
