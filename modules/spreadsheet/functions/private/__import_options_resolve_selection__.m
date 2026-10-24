%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Resolve a variable selection (names or numeric indices) into a row vector of
% valid variable indices, validating that every selected variable exists.
%=============================================================================
function indices = __import_options_resolve_selection__(opts, selection)
  names = opts.VariableNames;
  if isstring(names)
    names = cellstr(names);
  end
  nvars = numel(names);
  if isnumeric(selection)
    indices = double(selection(:)');
    if any(indices < 1) || any(indices > nvars) || any(indices ~= fix(indices))
      error('Nelson:spreadsheet:setvaropts:invalidSelection', ...
        'Variable index out of range.');
    end
    return
  end
  if ischar(selection)
    selection = {selection};
  elseif isstring(selection)
    selection = cellstr(selection);
  end
  if ~iscellstr(selection)
    error('Nelson:spreadsheet:setvaropts:invalidSelection', ...
      'Variable selection must be variable names or indices.');
  end
  indices = zeros(1, numel(selection));
  for k = 1:numel(selection)
    pos = find(strcmp(names, selection{k}), 1);
    if isempty(pos)
      error('Nelson:spreadsheet:setvaropts:unknownVariable', ...
        'Unrecognized variable name ''%s''.', selection{k});
    end
    indices(k) = pos;
  end
end
%=============================================================================
