%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [serial, millis, fold] = datetimeShiftSeconds(serial, millis, fold, zone, secondsValue)
  % Wall-clock serial dates of zone moved by elapsed seconds on the UTC time line.
  if ~isempty(zone)
    [serial, millis] = datetimeConvertZone(serial, millis, zone, '', fold);
  end
  serial = __datetime_serial_add_seconds__(serial, secondsValue, 1);
  if ~isempty(millis)
    millis = mod(millis + secondsValue .* 1000, 86400000);
  end
  fold = [];
  if ~isempty(zone)
    [serial, millis, fold] = datetimeConvertZone(serial, millis, '', zone);
  end
end
%=============================================================================
