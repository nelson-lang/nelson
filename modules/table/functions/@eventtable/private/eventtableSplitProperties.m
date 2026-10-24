%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [raw, requested] = eventtableSplitProperties(value, raw)
  % Splits an assigned Properties value into the timetable properties raw
  % (the current ones updated in place, so that their order is kept) and
  % the requested event variables (a missing field keeps its value).
  if ~isstruct(value) || ~isscalar(value)
    error('Nelson:table:invalidPropertiesAssignment', _('Properties must be a scalar structure.'));
  end
  requested = struct();
  fields = fieldnames(eventtableNoVariables());
  names = fieldnames(value);
  for k = 1:length(names)
    if any(strcmp(names{k}, fields))
      requested.(names{k}) = value.(names{k});
    else
      raw.(names{k}) = value.(names{k});
    end
  end
end
%=============================================================================
