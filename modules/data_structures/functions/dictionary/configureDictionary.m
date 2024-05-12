%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = configureDictionary(keyType, valueType)
  mustBeTextScalar(keyType, 1);
  mustBeTextScalar(valueType, 2);
  keyType = convertCharsToStrings(keyType);
  if (~checkIsType(keyType))
    msg = sprintf(_("Unknown type '%s'. Dictionary could not be created."), keyType);
    error(msg);
  end
  valueType = convertCharsToStrings(valueType);
  if (~checkIsType(valueType))
    msg = sprintf(_("Unknown type '%s'. Dictionary could not be created."), valueType);
    error(msg);
  end
  map = struct ();
  allKeys = {};
  st = struct();
  st.map = map;
  st.allKeys = allKeys;
  st.keyType = keyType;
  st.valueType = valueType;
  d = class(st, 'dictionary');
end
%=============================================================================
function tf = checkIsType(nameType)
  tf = false;
  numericTypes = [ "int8"; "uint8"; "int16"; "uint16"; "int32"; "uint32"; ...
            "int64"; "uint64"; "single"; "double" ];
  if ismember(nameType, numericTypes)
    tf = true;
    return
  end
  charStringTypes = ["char"; "string"];
  if ismember(nameType, charStringTypes)
    tf = true;
    return
  end
  specializedTypes = ["logical", "function_handle"]; 
  if ismember(nameType, specializedTypes)
    tf = true;
    return
  end
  structuredTypes = ["struct"; "cell"]; 
  if ismember(nameType, structuredTypes)
    tf = true;
    return
  end

  [list_builtin, list_macro] = what();
  all_elements = [list_macro; list_builtin];
  all_elements = string(all_elements);
  all_elements(~startsWith(all_elements, '@')) = [];

  slashIndex = strfind(all_elements, "/");
  
  for k = 1:numel(slashIndex)
    element = convertStringsToChars(all_elements(k));
    idx = slashIndex{k};
    if ~isempty(idx)
      element = element(2:idx(1)-1);
    else
      element = element(2:end);
    end
    element = string(element);
    if strcmp(element, nameType)
      tf = true;
      return
    end
  end 
end
%=============================================================================
