%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [rows, newRowNames] = tableResolveRows(st, sub, nRows, allowNew)
  % Row numbers selected by ':', a logical mask, numbers or row names in a
  % table of nRows rows. A name that is not a row raises an error; with
  % allowNew (an assignment) it is a new row after the last one instead,
  % and newRowNames lists these names in the order of their rows.
  newRowNames = {};
  if ischar(sub) && strcmp(sub, ':')
    rows = 1:nRows;
  elseif islogical(sub)
    rows = find(sub);
    rows = rows(:)';
  elseif isnumeric(sub)
    rows = sub(:)';
  elseif ischar(sub) || isstring(sub) || iscellstr(sub)
    if ischar(sub)
      names = {sub};
    else
      names = cellstr(sub);
      names = names(:)';
    end
    implicitNames = {};
    if isempty(st.Properties.RowNames)
      implicitNames = tableFirstVariableRowNames(st, nRows);
    end
    rows = zeros(1, length(names));
    for k = 1:length(names)
      idx = find(strcmp(st.Properties.RowNames, names{k}));
      if isempty(idx) && ~isempty(implicitNames)
        idx = find(strcmp(implicitNames, names{k}));
      end
      if isempty(idx) && nargin > 3 && allowNew
        idx = find(strcmp(newRowNames, names{k}));
        if isempty(idx)
          newRowNames{end + 1} = names{k};
          idx = length(newRowNames);
        end
        idx = nRows + idx;
      end
      if isempty(idx)
        error('Nelson:table:notFound', _('Unrecognized table row name ''%s''.'), names{k});
      end
      rows(k) = idx(1);
    end
  else
    error('Nelson:table:invalidSubscript', 'Invalid %s subscript type.', 'row');
  end
end
%=============================================================================
