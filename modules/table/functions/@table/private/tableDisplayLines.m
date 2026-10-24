%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lines = tableDisplayLines(strs, showRowLabels)
  % Display lines of a table body. strs(1, :) holds the headers, strs(:, 1)
  % the row labels (row names or row times), shown when showRowLabels is true.
  % Each column is as wide as its header or its widest text. Headers are
  % centered, row labels left-aligned, and the text block of a variable
  % (already justified by tableDisplayColumnText) is centered as a whole.
  % Columns are separated by four blanks, with no trailing separator.
  % Loose format adds an empty line after the underline and at the end of the
  % body; compact format has none.
  if ~showRowLabels
    strs = strs(:, 2:end);
  end
  nCols = size(strs, 2);
  B = strings(size(strs, 1) + 1, nCols);
  for j = 1:nCols
    header = strs(1, j);
    texts = strs(2:end, j);
    blockWidth = max([0; strlength(texts(:))]);
    width = max(strlength(header), blockWidth);
    B(1, j) = centerText(header, width);
    if strlength(header) > 0
      B(2, j) = string(repmat('_', 1, width));
    else
      B(2, j) = string(blanks(width));
    end
    if showRowLabels && j == 1
      B(3:end, j) = pad(texts, width, 'right');
    else
      offset = string(blanks(floor((width - blockWidth) / 2)));
      B(3:end, j) = pad(offset + texts, width, 'right');
    end
  end
  lines = "    " + join(B, "    ", 2);
  currentFormat = format();
  if strcmp(currentFormat.LineSpacing, 'loose')
    lines = [lines(1:2); ""; lines(3:end); ""];
  end
end
%=============================================================================
function text = centerText(text, width)
  left = floor((width - strlength(text)) / 2);
  text = pad(string(blanks(left)) + text, width, 'right');
end
%=============================================================================
