%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [serial, millis, fold] = datetimeConvertZone(serial, millis, fromZone, toZone, fold)
  % Wall-clock serial dates read in fromZone, rewritten as the wall clock of
  % the same instants in toZone ('' stands for UTC here). millis, the precise
  % time of day in milliseconds, follows (it may be empty). fold marks the
  % first occurrence of an ambiguous wall time, on input in fromZone and on
  % output in toZone ([] when none). Converting a zone to itself moves a time
  % skipped by a DST gap after the gap.
  if nargin < 5
    fold = [];
  end
  if strcmp(fromZone, toZone) && isFixedZone(fromZone)
    fold = [];
    return
  end
  dataPath = [modulepath('time'), '/resources/tzdata'];
  offsetFrom = zeros(size(serial));
  if ~isempty(fromZone)
    offsetFrom = datetimeWallOffset(serial, fromZone, fold);
  end
  [offsetTo, isFirst] = utcOffsets(serial - offsetFrom ./ 86400, toZone, dataPath, nargout > 2);
  delta = offsetTo - offsetFrom;
  if any(delta(:))
    serial = __datetime_serial_add_seconds__(serial, delta, 1);
    if ~isempty(millis)
      millis = mod(millis + delta .* 1000, 86400000);
    end
  end
  fold = [];
  if any(isFirst(:))
    fold = isFirst;
  end
end
%=============================================================================
function [offset, isFirst] = utcOffsets(utc, zone, dataPath, withFold)
  % offsets of UTC instants in zone and, if asked, their first-occurrence flags
  offset = zeros(size(utc));
  isFirst = [];
  if isempty(zone)
    return
  end
  if withFold
    [offset, isFirst] = timezoneoffset(utc, zone, dataPath, 'utc');
    isFirst = reshape(isFirst, size(utc));
  else
    offset = timezoneoffset(utc, zone, dataPath, 'utc');
  end
  offset = reshape(offset, size(utc));
end
%=============================================================================
function tf = isFixedZone(zone)
  tf = isempty(zone) || strcmp(zone, 'UTC') || strcmp(zone, 'UTCLeapSeconds') ...
    || any(zone(1) == '+-');
end
%=============================================================================
