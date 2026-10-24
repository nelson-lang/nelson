%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function b = datetimeAlignZone(b, zone, isConcatenation)
  % The datetime b expressed in the time zone of the datetime it is combined
  % with (same instants). A datetime with a time zone never combines with one
  % without.
  bZone = b.TimeZoneValue;
  if xor(isempty(zone), isempty(bZone))
    if nargin > 2 && isConcatenation
      error('Nelson:datetime:cat:IncompatibleTZ', 'Unable to concatenate a datetime array that has a time zone with one that does not have a time zone.');
    end
    error('Nelson:datetime:IncompatibleTZ', 'Cannot combine or compare a datetime array with a time zone with one without a time zone.');
  end
  if strcmp(zone, bZone)
    return
  end
  [serial, millis] = datetimeConvertZone(datetime.rawSerial(b), datetime.storedMillis(b), bZone, zone);
  b = datetime.fromSerial(serial, b.FormatValue, zone, millis);
end
%=============================================================================
