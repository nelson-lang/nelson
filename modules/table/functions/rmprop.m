%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Tout = rmprop(T, name)
  if nargin == 2 && isa(T, 'handle')
    if isstring(name)
      name = char(name);
    end
    Tout = __classdef_remove_dynamic_property__(T, name);
    return
  end
  narginchk(2, 2);
  mustBeA(T, 'table', 1);
  names = localCellstr(name);
  st = struct(T);
  if ~isfield(st.Properties, 'CustomProperties') || isempty(st.Properties.CustomProperties)
    error('Nelson:table:propertyNotFound', 'Custom property does not exist.');
  end
  for k = 1:length(names)
    if ~isfield(st.Properties.CustomProperties, names{k})
      error('Nelson:table:propertyNotFound', 'Custom property does not exist.');
    end
    st.Properties.CustomProperties = rmfield(st.Properties.CustomProperties, names{k});
  end
  if isfield(st, 'VariableCustomProperties')
    st.VariableCustomProperties = setdiff(st.VariableCustomProperties, names, 'stable');
  end
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
