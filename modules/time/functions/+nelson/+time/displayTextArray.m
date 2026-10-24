%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function displayTextArray(values, className, alignLeft, name)
  % Shared display of duration, datetime and calendarDuration arrays.
  % values: string array of the element texts (same size as the object).
  % Without name, behaves as disp; with a name, as display (class header,
  % N-D arrays page by page).
  isDisplay = (nargin > 3);
  if ~isDisplay
    name = '';
  end
  currentFormat = format();
  isLoose = strcmp(currentFormat.LineSpacing, 'loose');
  sz = size(values);
  isND = (length(sz) > 2);
  if isDisplay
    if isLoose
      fprintf('\n');
    end
    if ~isND || isempty(values)
      fprintf('%s = \n', name);
      if isLoose
        fprintf('\n');
      end
    end
    fprintf('%s\n', headerText(sz, className));
    if isLoose && (isempty(values) || ~isND)
      fprintf('\n');
    end
  end
  if isempty(values)
    return
  end
  if ~isND
    fprintf('%s', matrixText(values, alignLeft));
    if isLoose
      fprintf('\n');
    end
    return
  end
  trailing = sz(3:end);
  subs = cell(1, length(trailing));
  for page = 1:prod(trailing)
    [subs{:}] = ind2sub(trailing, page);
    pageIndex = sprintf(',%d', subs{:});
    if isLoose
      fprintf('\n%s(:,:%s) =\n\n', name, pageIndex);
    else
      fprintf('%s(:,:%s) =\n', name, pageIndex);
    end
    fprintf('%s', matrixText(values(:, :, subs{:}), alignLeft));
    if isLoose
      fprintf('\n');
    end
  end
end
%=============================================================================
function text = headerText(sz, className)
  if isequal(sz, [1 1])
    text = ['  ', className];
    return
  end
  dims = strjoin(arrayfun(@int2str, sz, 'UniformOutput', false), char(215));
  if any(sz == 0)
    text = ['  ', dims, ' empty ', className, ' array'];
  else
    text = ['  ', dims, ' ', className, ' array'];
  end
end
%=============================================================================
function text = matrixText(values, alignLeft)
  % One line per row, every element padded to the widest one, 3 blanks apart.
  % Built in one vectorized pass: padding the whole string array at once and
  % joining rows with a single join() avoids the quadratic char-buffer growth
  % of appending each formatted row to an accumulator.
  s = string(values);
  width = max(strlength(s(:)));
  if isempty(width)
    width = 0;
  end
  if alignLeft
    s = pad(s, width, 'right');
  else
    s = pad(s, width, 'left');
  end
  rows = '   ' + join(s, '   ', 2) + newline;
  text = char(join(rows, ''));
end
%=============================================================================
