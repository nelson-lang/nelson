%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Tout = addprop(T, name, propertyType)
  if nargin == 2 && isa(T, 'handle')
    if isstring(name)
      name = char(name);
    end
    Tout = __classdef_add_dynamic_property__(T, name);
    return
  end
  narginchk(3, 3);
  mustBeA(T, 'table', 1);
  names = localCellstr(name);
  propertyTypes = localCellstr(propertyType);
  if length(propertyTypes) == 1 && length(names) > 1
    propertyTypes = repmat(propertyTypes, 1, length(names));
  end
  if length(propertyTypes) ~= length(names)
    error('Nelson:table:dimagree:propertyTypesMatchPropertyNames', 'Property types must match property names.');
  end
  st = struct(T);
  if ~isfield(st.Properties, 'CustomProperties') || isempty(st.Properties.CustomProperties)
    st.Properties.CustomProperties = struct();
  end
  if ~isfield(st, 'VariableCustomProperties')
    st.VariableCustomProperties = {};
  end
  for k = 1:length(names)
    if isfield(st.Properties.CustomProperties, names{k})
      error('Nelson:table:propertyExists', 'Custom property already exists.');
    end
    if ~any(strcmpi(propertyTypes{k}, {'table', 'variable'}))
      error('Nelson:table:invalidValue:propertyTypeTableVariable', 'Property type must be ''table'' or ''variable''.');
    end
    st.Properties.CustomProperties.(names{k}) = [];
    if strcmpi(propertyTypes{k}, 'variable')
      st.VariableCustomProperties{end + 1} = names{k};
    end
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
