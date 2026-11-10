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
  opts = importOptionsVariableOptionsInit(opts);
  indices = importOptionsResolveSelection(opts, selection);
  options = opts.VariableOptions;
  for k = 1:2:numel(varargin)
    name = importOptionsVariableOptionName(varargin{k});
    value = varargin{k + 1};
    for idx = indices
      options(idx).(name) = value;
    end
    if strcmp(name, 'Type')
      types = opts.VariableTypes;
      for idx = indices
        types{idx} = importOptionsScalarText(value);
      end
      opts.VariableTypes = types;
    end
  end
  opts.VariableOptions = options;
end
%=============================================================================
