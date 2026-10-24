%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = datetimeZoneNormalized(obj)
  % A zoned datetime whose wall clock falls in a DST gap moves after the gap.
  zone = obj.TimeZoneValue;
  if isempty(zone)
    return
  end
  [serial, millis] = datetimeConvertZone(datetime.rawSerial(obj), datetime.storedMillis(obj), zone, zone);
  obj = datetime.fromSerial(serial, obj.FormatValue, zone, millis);
end
%=============================================================================
