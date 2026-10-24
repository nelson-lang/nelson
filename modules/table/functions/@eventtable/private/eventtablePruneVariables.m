%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function vars = eventtablePruneVariables(vars, variableNames)
  % An event variable removed or renamed is unset.
  fields = fieldnames(vars);
  for k = 1:length(fields)
    name = vars.(fields{k});
    if ~isempty(name) && ~any(strcmp(name, variableNames))
      vars.(fields{k}) = [];
    end
  end
end
%=============================================================================
