%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = timetableAssignedProperties(current, value)
  % Properties value assigned to a timetable: a new Events value is checked
  % against the timetable row times. The other fields are checked by the
  % Properties setter.
  if isstruct(value) && isscalar(value) && isfield(value, 'Events') ...
      && ~(strcmp(class(value.Events), class(current.Events)) && isequal(value.Events, current.Events))
    value.Events = timetableCheckedEvents(value.Events, current.RowTimes);
  end
end
%=============================================================================
