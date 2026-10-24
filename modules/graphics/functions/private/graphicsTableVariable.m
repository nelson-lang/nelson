%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [value, name] = graphicsTableVariable(tbl, selector)
  names = tbl.Properties.VariableNames;
  if ischar(selector) || isStringScalar(selector)
    name = char(selector);
  elseif isnumeric(selector) && isscalar(selector)
    index = double(selector);
    if index < 1 || index > numel(names) || floor(index) ~= index
      error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
    end
    name = names{index};
  elseif islogical(selector) && isvector(selector) && sum(selector(:)) == 1
    index = find(selector(:), 1);
    name = names{index};
  else
    error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
  end
  if ~any(strcmp(name, names))
    error('Nelson:graphics:unrecognizedTableVariable', 'Unrecognized table variable.');
  end
  value = tbl.(name);
end
%=============================================================================
