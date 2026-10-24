%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nVars = tableSubscriptCount(varSub, variableNames)
  % number of variables selected by a variable subscript, new names included
  if ischar(varSub) && strcmp(varSub, ':')
    nVars = length(variableNames);
  elseif islogical(varSub)
    nVars = nnz(varSub);
  elseif isnumeric(varSub)
    nVars = numel(varSub);
  elseif ischar(varSub)
    nVars = 1;
  else
    nVars = numel(varSub);
  end
end
%=============================================================================
