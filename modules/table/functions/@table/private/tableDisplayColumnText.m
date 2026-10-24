%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function texts = tableDisplayColumnText(values, rows)
  % Display text (string column) of the rows 'rows' of the table variable
  % 'values'. Each column of a cell or string variable is padded to its own
  % width, as the cells of a cell array are.
  nRows = numel(rows);
  texts = strings(nRows, 1);
  nCols = size(values, 2);
  if nRows == 0
    return
  end
  if (iscell(values) || isstring(values)) && ismatrix(values) && nCols > 0
    for k = 1:nCols
      if iscell(values)
        column = cellColumnText(values(rows, k));
      else
        column = stringColumnText(values(rows, k));
      end
      if k == 1
        texts = column;
      else
        texts = texts + "    " + column;
      end
    end
    return
  end
  if isnumeric(values) && ismatrix(values)
    [cells, ok] = tableDisplayNumberText(values(rows, :));
    if ~ok
      cells = elementsText(values(rows, :));
    end
    texts = numericColumnsText(cells);
    return
  end
  if isa(values, 'datetime') && ismatrix(values)
    % One format for the whole variable, as for a datetime array: a
    % midnight row keeps its time when another row, displayed or not, has
    % one. Only the displayed rows are formatted.
    texts = numericColumnsText(string(cellstr(values(rows, :), values.Format)));
    return
  end
  colons = repmat({':'}, 1, ndims(values) - 1);
  for i = 1:nRows
    texts(i) = tableDisplayCellText(values(rows(i), colons{:}));
  end
  % Real numbers and times are right-aligned in their block, texts (and
  % complex numbers) left-aligned.
  side = 'right';
  if (isnumeric(values) && isreal(values)) || isa(values, 'duration') ...
      || isa(values, 'datetime') || isa(values, 'calendarDuration')
    side = 'left';
  end
  texts = pad(texts, max(strlength(texts)), side);
end
%=============================================================================
function cells = elementsText(values)
  cells = strings(size(values));
  for k = 1:numel(values)
    cells(k) = tableDisplayCellText(values(k));
  end
end
%=============================================================================
function texts = numericColumnsText(cells)
  % All the elements right-aligned to one common width, columns separated by
  % four blanks, then the leading blanks common to every row removed.
  cells = pad(cells, max(strlength(cells(:))), 'left');
  texts = join(cells, "    ", 2);
  leading = min(strlength(texts) - strlength(strip(texts, 'left')));
  texts = extractAfter(texts, leading);
end
%=============================================================================
function texts = stringColumnText(values)
  n = numel(values);
  texts = strings(n, 1);
  for i = 1:n
    texts(i) = tableDisplayCellText(values(i));
  end
  texts = pad(texts, max(strlength(texts)), 'right');
end
%=============================================================================
function texts = cellColumnText(values)
  % {'a'   }, {[  1]}, {0x0 double}: the contents of the cells padded to a
  % common width, numbers right-aligned between their brackets.
  n = numel(values);
  inner = strings(n, 1);
  rightAligned = false(n, 1);
  bracketed = false(n, 1);
  for i = 1:n
    content = values{i};
    text = strtrim(char(formattedDisplayText({content})));
    if length(text) >= 2 && text(1) == '{' && text(end) == '}' && ~any(text == newline)
      text = text(2:end - 1);
    else
      text = sprintf(['%d', char(215)], size(content));
      text = [text(1:end - 1), ' ', class(content)];
    end
    bracketed(i) = length(text) >= 2 && text(1) == '[' && text(end) == ']';
    if bracketed(i)
      text = text(2:end - 1);
      rightAligned(i) = isnumeric(content) || islogical(content);
    end
    inner(i) = text;
  end
  width = max(strlength(inner) + 2 * bracketed);
  for i = 1:n
    if bracketed(i)
      side = 'right';
      if rightAligned(i)
        side = 'left';
      end
      inner(i) = "[" + pad(inner(i), width - 2, side) + "]";
    else
      inner(i) = pad(inner(i), width, 'right');
    end
  end
  texts = "{" + inner + "}";
end
%=============================================================================
