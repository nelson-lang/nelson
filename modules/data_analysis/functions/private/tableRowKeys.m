%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function keys = tableRowKeys(st, nRows, vars)
  keys = cell(nRows, 1);
  nVars = length(vars);
  % Calling tableValueKey per element is O(nRows) per call in Nelson when its
  % result is accumulated into a cell array (class()/recursive dispatch), so
  % the former loop was O(nRows^2). Hoist everything invariant across rows
  % (the extracted columns, the "#j=" prefixes, the numeric class name) and
  % format the common column kinds inline. The keys produced are byte-for-byte
  % identical to tableValueKey; exotic kinds fall back to it.
  % KIND_OTHER_* cover any remaining type (categorical, datetime, duration,
  % ...). tableValueKey formats those as [class ':' mat2str] or, when mat2str
  % does not apply, [class ':' char(string(value))] -- both of which call
  % class() per element. We decide the formatter once per column (the column
  % is homogeneous) and, for the string path, convert the whole column with a
  % single vectorized string() call instead of n per-element string() calls.
  KIND_OTHER_FALLBACK = 0; KIND_NUMERIC = 1; KIND_CELLSTR = 2; KIND_CHAR = 3;
  KIND_STRING = 4; KIND_OTHER_MAT2STR = 5; KIND_OTHER_STRING = 6;
  columns = cell(1, nVars);
  prefixes = cell(1, nVars);
  classPrefixes = cell(1, nVars);
  otherStrings = cell(1, nVars);
  kinds = zeros(1, nVars);
  for j = 1:nVars
    col = st.data.(st.Properties.VariableNames{vars(j)});
    columns{j} = col;
    prefixes{j} = ['#', int2str(j), '='];
    if isnumeric(col) || islogical(col)
      kinds(j) = KIND_NUMERIC;
      classPrefixes{j} = [class(col), ':'];
    elseif iscellstr(col)
      kinds(j) = KIND_CELLSTR;
    elseif ischar(col)
      kinds(j) = KIND_CHAR;
    elseif isstring(col)
      kinds(j) = KIND_STRING;
    else
      classPrefixes{j} = [class(col), ':'];
      kinds(j) = KIND_OTHER_FALLBACK;
      if nRows >= 1
        canMat2str = true;
        try
          mat2str(tableColumnRows(col, 1));
        catch
          canMat2str = false;
        end
        if canMat2str
          kinds(j) = KIND_OTHER_MAT2STR;
        else
          try
            otherStrings{j} = string(col);
            kinds(j) = KIND_OTHER_STRING;
          catch
            kinds(j) = KIND_OTHER_FALLBACK;
          end
        end
      end
    end
  end
  for r = 1:nRows
    key = '';
    for j = 1:nVars
      switch kinds(j)
        case KIND_NUMERIC
          fragment = [classPrefixes{j}, mat2str(tableColumnRows(columns{j}, r))];
        case KIND_CELLSTR
          fragment = ['cell:char:', columns{j}{r}];
        case KIND_CHAR
          fragment = ['char:', columns{j}(r, :)];
        case KIND_STRING
          fragment = ['string:', char(columns{j}(r))];
        case KIND_OTHER_MAT2STR
          fragment = [classPrefixes{j}, mat2str(tableColumnRows(columns{j}, r))];
        case KIND_OTHER_STRING
          fragment = [classPrefixes{j}, char(otherStrings{j}(r))];
        otherwise
          fragment = tableValueKey(tableColumnRows(columns{j}, r));
      end
      key = [key, prefixes{j}, fragment, ';'];
    end
    keys{r} = key;
  end
end
%=============================================================================
