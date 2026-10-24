%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [serial, millis] = datetimeConvertZone(serial, millis, fromZone, toZone)
  % Wall-clock serial dates read in fromZone, rewritten as the wall clock of
  % the same instants in toZone ('' stands for UTC here). millis, the precise
  % time of day in milliseconds, follows (it may be empty). Converting a zone
  % to itself moves a time skipped by a DST gap after the gap.
  if strcmp(fromZone, toZone) && isFixedZone(fromZone)
    return
  end
  dataPath = [modulepath('time'), '/resources/tzdata'];
  offsetFrom = zeros(size(serial));
  if ~isempty(fromZone)
    offsetFrom = reshape(timezoneoffset(serial, fromZone, dataPath), size(serial));
  end
  offsetTo = zeros(size(serial));
  if ~isempty(toZone)
    utc = serial - offsetFrom ./ 86400;
    offsetTo = reshape(timezoneoffset(utc, toZone, dataPath, 'utc'), size(serial));
  end
  delta = offsetTo - offsetFrom;
  if ~any(delta(:))
    return
  end
  serial = __datetime_serial_add_seconds__(serial, delta, 1);
  if ~isempty(millis)
    millis = mod(millis + delta .* 1000, 86400000);
  end
end
%=============================================================================
function tf = isFixedZone(zone)
  tf = isempty(zone) || strcmp(zone, 'UTC') || strcmp(zone, 'UTCLeapSeconds') ...
    || any(zone(1) == '+-');
end
%=============================================================================
