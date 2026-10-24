%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function column = jsonTableWriteColumn(value, preserveInfAndNaN)
  % Prepares a table variable for the JSON writer: real numeric and logical
  % arrays are kept, text-like values become string arrays (missing, NaT and
  % <undefined> give null), datetime and duration use their display format,
  % other values are encoded row by row as raw JSON text (cell array).
  if (isnumeric(value) && isreal(value) && ~issparse(value)) || islogical(value)
    column = full(value);
  elseif ischar(value)
    column = string(cellstr(value));
  elseif iscellstr(value) || isstring(value) || iscategorical(value)
    column = string(value);
  elseif isdatetime(value)
    column = timeText(value, isnat(value));
  elseif isduration(value)
    column = timeText(value, isnan(value));
  else
    column = encodeRows(value, preserveInfAndNaN);
  end
end
%=============================================================================
function column = timeText(value, isMissing)
  column = repmat(string(missing), size(value));
  if any(~isMissing(:))
    column(~isMissing) = string(value(~isMissing));
  end
end
%=============================================================================
function column = encodeRows(value, preserveInfAndNaN)
  n = size(value, 1);
  column = cell(n, 1);
  for k = 1:n
    if iscell(value) && size(value, 2) == 1
      element = value{k};
    else
      element = value(k, :);
    end
    if isempty(element) && isnumeric(element)
      column{k} = 'null';
    else
      column{k} = jsonencode(element, 'ConvertInfAndNaN', ~preserveInfAndNaN);
    end
  end
end
%=============================================================================
