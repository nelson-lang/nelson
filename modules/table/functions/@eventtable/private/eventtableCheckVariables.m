%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function vars = eventtableCheckVariables(requested, current, data, variableNames, rowTimes)
  % Event variables after a Properties assignment: a changed name is
  % checked, an unchanged one is unset when its variable is gone.
  vars = eventtableNoVariables();
  fields = fieldnames(vars);
  for k = 1:length(fields)
    vars.(fields{k}) = current.(fields{k});
  end
  vars = eventtablePruneVariables(vars, variableNames);
  names = fieldnames(requested);
  for k = 1:length(names)
    if ~isequal(requested.(names{k}), current.(names{k}))
      vars.(names{k}) = eventtableResolveVariable(names{k}, requested.(names{k}), data, variableNames, rowTimes);
    end
  end
  if ~isempty(vars.EventLengthsVariable) && ~isempty(vars.EventEndsVariable)
    error('Nelson:eventtable:bothLengthsAndEnds', 'Specify either event lengths or event ends but not both.');
  end
end
%=============================================================================
