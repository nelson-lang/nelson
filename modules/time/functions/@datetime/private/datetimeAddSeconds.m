%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [serial, millis, fold] = datetimeAddSeconds(obj, secondsValue, direction)
  % Values of a datetime moved by a number of seconds (direction is +1 to add,
  % -1 to subtract). A zoned datetime moves on the UTC time line, so adding
  % hours(24) across a DST change keeps 24 elapsed hours. millis is empty when
  % the source has no precise time of day; fold marks first occurrences of
  % ambiguous times in the result.
  [serial, millis, fold] = datetimeShiftSeconds(datetime.rawSerial(obj), ...
    datetime.storedMillis(obj), datetime.storedFold(obj), obj.TimeZoneValue, direction .* secondsValue);
end
%=============================================================================
