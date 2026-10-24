%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [serial, millis, zone] = datetimeZoneChange(obj, value)
  % Values of a datetime whose TimeZone becomes value: a zoned datetime keeps
  % its instants (the wall clock moves to the new zone), an unzoned one keeps
  % its wall clock, and removing the time zone keeps the wall clock.
  zone = datetimeTimeZoneName(value);
  serial = datetime.rawSerial(obj);
  millis = datetime.storedMillis(obj);
  if isempty(zone)
    return
  end
  fromZone = obj.TimeZoneValue;
  if isempty(fromZone)
    fromZone = zone;
  end
  [serial, millis] = datetimeConvertZone(serial, millis, fromZone, zone);
end
%=============================================================================
