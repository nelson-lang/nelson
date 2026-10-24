%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = setvaropts(opts, selection, varargin)
  if nargin < 3
    error('Nelson:spreadsheet:setvaropts:notEnoughInputs', 'Not enough input arguments.');
  end
  if mod(numel(varargin), 2) ~= 0
    error('Nelson:spreadsheet:nameValueExpected', 'Name-value arguments expected.');
  end
  opts = __import_options_variable_options_init__(opts);
  indices = __import_options_resolve_selection__(opts, selection);
  options = opts.VariableOptions;
  for k = 1:2:numel(varargin)
    name = __import_options_variable_option_name__(varargin{k});
    value = varargin{k + 1};
    for idx = indices
      options(idx).(name) = value;
    end
    if strcmp(name, 'Type')
      types = opts.VariableTypes;
      for idx = indices
        types{idx} = __import_options_scalar_text__(value);
      end
      opts.VariableTypes = types;
    end
  end
  opts.VariableOptions = options;
end
%=============================================================================
