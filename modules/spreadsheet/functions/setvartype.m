%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = setvartype(opts, selection, types)
  if nargin < 3
    error('Nelson:spreadsheet:setvartype:notEnoughInputs', 'Not enough input arguments.');
  end
  opts = __import_options_variable_options_init__(opts);
  indices = __import_options_resolve_selection__(opts, selection);
  if isstring(types)
    types = cellstr(types);
  elseif ischar(types)
    types = {types};
  end
  if ~iscellstr(types)
    error('Nelson:spreadsheet:setvartype:invalidType', ...
      'Variable types must be text values.');
  end
  if numel(types) == 1
    types = repmat(types, 1, numel(indices));
  elseif numel(types) ~= numel(indices)
    error('Nelson:spreadsheet:setvartype:sizeMismatch', ...
      'Number of types must match the number of selected variables.');
  end
  variableTypes = opts.VariableTypes;
  if isstring(variableTypes)
    variableTypes = cellstr(variableTypes);
  end
  options = opts.VariableOptions;
  for k = 1:numel(indices)
    variableTypes{indices(k)} = types{k};
    options(indices(k)).Type = types{k};
  end
  opts.VariableTypes = variableTypes;
  opts.VariableOptions = options;
end
%=============================================================================
