%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function props = eventtablePropertiesView(raw, vars)
  % Properties as shown: the event variables first, then the timetable
  % properties without Events.
  props = eventtablePruneVariables(vars, raw.VariableNames);
  names = fieldnames(raw);
  for k = 1:length(names)
    if ~strcmp(names{k}, 'Events')
      props.(names{k}) = raw.(names{k});
    end
  end
end
%=============================================================================
