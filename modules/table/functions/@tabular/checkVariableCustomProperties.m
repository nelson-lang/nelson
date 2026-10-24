%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = checkVariableCustomProperties(obj, value, current, names)
  % A per-variable custom property (names) assigned in the Properties struct
  % value holds one element per variable, stored as a row, or is 0x0.
  nVars = length(value.VariableNames);
  for k = 1:length(names)
    name = names{k};
    if ~isfield(value.CustomProperties, name) ...
        || (isfield(current.CustomProperties, name) && isequal(value.CustomProperties.(name), current.CustomProperties.(name)))
      continue
    end
    propertyValue = value.CustomProperties.(name);
    if isequal(size(propertyValue), [0 0])
      continue
    end
    if ~isvector(propertyValue) || numel(propertyValue) ~= nVars
      error('Nelson:table:IncorrectNumberOfVarCustomProps', _('The custom property ''%s'' must be a vector with one element for each variable in the table or must be a 0x0 empty (e.g. [], {}).'), name);
    end
    value.CustomProperties.(name) = reshape(propertyValue, 1, []);
  end
end
%=============================================================================
