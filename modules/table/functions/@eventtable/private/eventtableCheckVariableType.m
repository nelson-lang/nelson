%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function eventtableCheckVariableType(field, value, rowTimes)
  isTime = isa(value, 'datetime') || isa(value, 'duration') || isa(value, 'calendarDuration');
  switch field
    case 'EventLabelsVariable'
      if isTime
        error('Nelson:eventtable:invalidLabelsVariableTime', 'Event labels variable must not be a datetime, duration, or calendarDuration variable.');
      end
    case 'EventLengthsVariable'
      if ~(isa(value, 'duration') || isa(value, 'calendarDuration')) || size(value, 2) ~= 1
        error('Nelson:eventtable:invalidLengthsVariable', 'Event lengths variable must be a duration or calendarDuration column vector.');
      end
    case 'EventEndsVariable'
      if ~(isa(value, 'datetime') || isa(value, 'duration')) || size(value, 2) ~= 1
        error('Nelson:eventtable:invalidEndsVariable', 'Event ends variable must be a datetime or duration column vector.');
      end
      if ~strcmp(class(value), class(rowTimes))
        error('Nelson:eventtable:incompatibleEndsRowTimes', 'Event ends variable must be the same data type as the event table row times.');
      end
  end
end
%=============================================================================
