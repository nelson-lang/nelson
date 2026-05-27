%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Tout = movevars(T, vars, varargin)
  narginchk(3, 4);
  mustBeA(T, 'table', 1);
  st = struct(T);
  vars = localCellstr(vars);
  moveIdx = localFindVariables(st.Properties.VariableNames, vars);
  remaining = st.Properties.VariableNames;
  remaining(moveIdx) = [];

  position = varargin{1};
  reference = '';
  if length(varargin) > 1
    reference = varargin{2};
  end

  switch lower(char(position))
    case 'before'
      insertAt = localFindVariable(remaining, reference);
    case 'after'
      insertAt = localFindVariable(remaining, reference) + 1;
    case 'first'
      insertAt = 1;
    case 'last'
      insertAt = length(remaining) + 1;
    otherwise
      error(_('Position must be Before, After, First, or Last.'));
  end
  st.Properties.VariableNames = [remaining(1:insertAt - 1), vars(:)', remaining(insertAt:length(remaining))];
  st.data = localReorderData(st.data, st.Properties.VariableNames);
  Tout = table.fromStruct(st);
end
%=============================================================================
function names = localCellstr(names)
  if ischar(names)
    names = {names};
  elseif isstring(names)
    names = cellstr(names);
  end
  names = names(:)';
end
%=============================================================================
function idx = localFindVariable(variableNames, name)
  if isnumeric(name) && isscalar(name)
    idx = name;
    if idx < 1 || idx > length(variableNames)
      error(_('Variable index out of range.'));
    end
    return
  end
  names = localCellstr(name);
  if length(names) ~= 1
    error(_('A single variable name is expected.'));
  end
  idx = find(strcmp(variableNames, names{1}));
  if isempty(idx)
    error(_('Unrecognized table variable name.'));
  end
  idx = idx(1);
end
%=============================================================================
function idx = localFindVariables(variableNames, names)
  idx = zeros(1, length(names));
  for k = 1:length(names)
    idx(k) = localFindVariable(variableNames, names{k});
  end
  if length(unique(idx)) ~= length(idx)
    error(_('Duplicate table variable name.'));
  end
end
%=============================================================================
function data = localReorderData(data, variableNames)
  reordered = struct();
  for k = 1:length(variableNames)
    reordered.(variableNames{k}) = data.(variableNames{k});
  end
  data = reordered;
end
%=============================================================================
