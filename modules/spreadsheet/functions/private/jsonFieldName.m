%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = jsonFieldName(value, key)
  % Returns the field of the decoded object 'value' (or of its list of
  % field names) matching the JSON key, or '' when absent. Decoded field
  % names are valid identifiers.
  name = '';
  if iscellstr(value)
    value = cell2struct(cell(numel(value), 1), value(:), 1);
  end
  if ~isstruct(value)
    return
  end
  if isfield(value, key)
    name = key;
    return
  end
  candidate = regexprep(key, '[^a-zA-Z_0-9]', '_');
  if isempty(candidate)
    candidate = 'x';
  elseif candidate(1) == '_'
    candidate = ['x', candidate];
  elseif any(candidate(1) == '0123456789')
    candidate = ['x_', candidate];
  end
  if isfield(value, candidate)
    name = candidate;
  end
end
%=============================================================================
