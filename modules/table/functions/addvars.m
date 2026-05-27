%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Tout = addvars(T, varargin)
  narginchk(2, Inf);
  mustBeA(T, 'table', 1);

  before = '';
  after = '';
  newNames = {};
  values = {};

  k = 1;
  while k <= length(varargin)
    if (ischar(varargin{k}) || (isstring(varargin{k}) && isscalar(varargin{k}))) && ~isempty(values)
      name = char(varargin{k});
      if any(strcmpi(name, {'Before', 'After', 'NewVariableNames'}))
        if k == length(varargin)
          error(_('Name-value argument must be followed by a value.'));
        end
        switch lower(name)
          case 'before'
            before = varargin{k + 1};
          case 'after'
            after = varargin{k + 1};
          case 'newvariablenames'
            newNames = localCellstr(varargin{k + 1});
        end
        k = k + 2;
        continue
      end
    end
    values{end + 1} = varargin{k};
    k = k + 1;
  end

  if isempty(newNames)
    newNames = cell(1, length(values));
    for k = 1:length(values)
      newNames{k} = ['Var', num2str(width(T) + k)];
    end
  end
  if length(newNames) ~= length(values)
    error(_('NewVariableNames must match the number of added variables.'));
  end

  st = struct(T);
  insertAt = length(st.Properties.VariableNames) + 1;
  if ~isempty(before)
    insertAt = localFindVariable(st.Properties.VariableNames, before);
  elseif ~isempty(after)
    insertAt = localFindVariable(st.Properties.VariableNames, after) + 1;
  end

  for k = 1:length(newNames)
    if any(strcmp(st.Properties.VariableNames, newNames{k}))
      error(_('Duplicate table variable name.'));
    end
    st.data.(newNames{k}) = values{k};
  end
  leftNames = st.Properties.VariableNames(1:insertAt - 1);
  rightNames = st.Properties.VariableNames(insertAt:length(st.Properties.VariableNames));
  st.Properties.VariableNames = [leftNames(:)', newNames(:)', rightNames(:)'];
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
function data = localReorderData(data, variableNames)
  reordered = struct();
  for k = 1:length(variableNames)
    reordered.(variableNames{k}) = data.(variableNames{k});
  end
  data = reordered;
end
%=============================================================================
