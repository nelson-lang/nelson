%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = getvaropts(opts, selection)
  if nargin < 2
    error('Nelson:spreadsheet:getvaropts:notEnoughInputs', 'Not enough input arguments.');
  end
  opts = importOptionsVariableOptionsInit(opts);
  indices = importOptionsResolveSelection(opts, selection);
  value = opts.VariableOptions(indices);
end
%=============================================================================
