%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = convertvars(T, vars, conversion)
  narginchk(3, 3);
  mustBeA(T, 'table', 1);
  names = T.Properties.VariableNames;
  isFun = isa(conversion, 'function_handle');
  if ~isFun && ~(ischar(conversion) || isstring(conversion))
    error('Nelson:table:invalidType:conversionAFunctionHandleTypeName', 'Conversion must be a function handle or type name.');
  end
  idx = tableResolveVariables(names, vars);
  if isempty(idx)
    return
  end
  if ~isFun
    conversion = char(conversion);
  end
  % Convert every requested variable in a single struct round-trip. Assigning
  % T.(name) once per variable rebuilds the whole backing struct each time
  % (O(numVars) per assignment), making conversion of a wide table O(numVars^2).
  st = struct(T);
  dataNames = fieldnames(st.data);
  vals = struct2cell(st.data);
  [~, nameToData] = ismember(names, dataNames);
  for k = idx(:)'
    pos = nameToData(k);
    if isFun
      vals{pos} = conversion(vals{pos});
    else
      vals{pos} = cast(vals{pos}, conversion);
    end
  end
  st.data = __tableStructFromCell__(vals, dataNames);
  st.Properties.VariableTypes = string([]);
  T = table.fromStruct(st);
end
%=============================================================================
