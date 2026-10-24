%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = tableDisplayCellText(value)
  % One-line text of one row of a table variable. Longer than 15 characters,
  % or N-D, it is replaced by its size and class, except a text or a time.
  originalValue = value;
  if ndims(value) > 2
    value = sizeAsString(value);
    return
  end
  isString = isa(value, 'string');
  if isString && isscalar(value) && ismissing(value)
    value = '<missing>';
    return
  end
  usedCompact = false;
  if ~isString && isscalar(value) && isobject(value) ...
      && isa(value, 'nelson.mixin.CustomCompactDisplayProvider')
    [compactText, usedCompact] = compactObjectText(value);
  end
  if usedCompact
    value = compactText;
  elseif islogical(value) && isrow(value) && ~issparse(value) && ~isempty(value)
    value = logicalText(value);
  else
    value = formattedDisplayText(value);
    if isString
      value = """" + strip(value, 'right', newline) + """";
    end
  end
  value = char(value);
  value = strtrim(value(1:end));
  if length(value) > 15 && ~showsFullText(originalValue)
    value = sizeAsString(originalValue);
  end
end
%=============================================================================
function tf = showsFullText(value)
  % A char row, or a single string, categorical, datetime, duration or
  % calendarDuration, always shows its text.
  tf = (ischar(value) && isrow(value)) || (isscalar(value) && (isstring(value) ...
      || iscategorical(value) || isa(value, 'datetime') || isa(value, 'duration') ...
      || isa(value, 'calendarDuration')));
end
%=============================================================================
function s = logicalText(value)
  % A table shows logical values as words: "true   false".
  words = ["false", "true"];
  s = join(pad(words(double(value) + 1), 5, 'right'), "  ");
end
%=============================================================================
function [s, ok] = compactObjectText(obj)
  % Ask a nelson.mixin.CustomCompactDisplayProvider object for its one-line
  % compact representation, constrained to the table cell budget. Falls back
  % (ok = false) when the method is not overridden or errors.
  s = '';
  ok = false;
  try
    config = nelson.display.DisplayConfiguration();
    rep = obj.compactRepresentationForSingleLine(config, 15);
    if isa(rep, 'nelson.display.CompactDisplayRepresentation')
      s = char(rep.PaddedDisplayOutput);
    else
      s = char(rep);
    end
    ok = true;
  catch
    ok = false;
  end
end
%=============================================================================
function s = sizeAsString(value)
  % Size and class, the size written with the times sign as in a cell.
  sizeStr = sprintf(['%d', char(215)], size(value));
  s = [sizeStr(1:end - 1), ' ', class(value)];
end
%=============================================================================
