%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = datetimeAddCalendar(t, c, signValue)
  % t + signValue * c. Months then days move the wall clock, the time part
  % adds elapsed seconds. A zoned result in a DST gap moves after the gap; an
  % ambiguous one is the later occurrence after a month step and keeps the
  % UTC offset of its source after a day step.
  serial = datetime.rawSerial(t);
  monthsValue = calendarDuration.rawMonths(c) .* signValue;
  daysValue = calendarDuration.rawDays(c) .* signValue;
  secondsValue = calendarDuration.rawSeconds(c) .* signValue;
  targetSize = datetime.commonSize(serial, monthsValue, daysValue, secondsValue);
  monthsValue = datetime.expandToSize(monthsValue, targetSize);
  daysValue = datetime.expandToSize(daysValue, targetSize);
  secondsValue = datetime.expandToSize(secondsValue, targetSize);
  millis = datetime.storedMillis(t);
  if isempty(millis)
    millis = datetime.timeOfDayMillis(0, 0, (serial - floor(serial)) .* 86400);
  end
  fold = datetime.foldOrFalse(t);
  if ~isequal(size(serial), targetSize)
    serial = datetime.expandToSize(serial, targetSize);
    millis = datetime.expandToSize(millis, targetSize);
    fold = datetime.expandToSize(fold, targetSize);
  end
  zone = t.TimeZoneValue;
  moved = monthsValue ~= 0;
  if any(moved(:))
    serial(moved) = addMonths(serial(moved), monthsValue(moved), millis(moved));
    fold(moved) = false;
    [serial, millis, fold] = datetimeConvertZone(serial, millis, zone, zone, fold);
  end
  moved = daysValue ~= 0;
  if any(moved(:))
    sourceOffset = [];
    if ~isempty(zone)
      sourceOffset = datetimeWallOffset(serial, zone, fold);
    end
    serial(moved) = serial(moved) + daysValue(moved);
    [serial, millis, fold] = datetimeResolveWallClock(serial, millis, zone, sourceOffset);
  end
  moved = secondsValue ~= 0;
  if any(moved(:))
    [serial, millis, fold] = datetimeShiftSeconds(serial, millis, fold, zone, secondsValue);
  end
  millis(isnan(serial)) = NaN;
  out = datetime.fromSerial(serial, t.Format, zone, millis, fold);
end
%=============================================================================
function serial = addMonths(serial, monthsValue, millis)
  % same day (clamped to the month end) and time of day, months later
  v = datevec(reshape(serial, [], 1));
  v(:, 2) = v(:, 2) + reshape(monthsValue, [], 1);
  y = v(:, 1) + floor((v(:, 2) - 1) ./ 12);
  m = mod(v(:, 2) - 1, 12) + 1;
  d = min(v(:, 3), eomday(y, m));
  serial = reshape(datenum(y, m, d) + reshape(millis, [], 1) ./ 86400000, size(serial));
end
%=============================================================================
