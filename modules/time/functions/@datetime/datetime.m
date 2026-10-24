%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef datetime
  properties (Dependent)
    Format
    TimeZone
    Year
    Month
    Day
    Hour
    Minute
    Second
  end

  properties (Hidden)
    SerialValue = now()
    % 'default' is a sentinel: the display format is resolved from the content
    % ('dd-MMM-uuuu' if all times of day are zero, 'dd-MMM-uuuu HH:mm:ss' otherwise)
    FormatValue = 'default'
    TimeZoneValue = ''
    % Time of day in milliseconds, in [0, 86400000), kept at full precision.
    % SerialValue is a single serial-day double, so folding the time of day into
    % the large day count loses a few microseconds for dates far from the
    % origin. This companion value keeps the time of day precise (its small
    % magnitude leaves every microsecond representable), while the integer day
    % is recovered from SerialValue, so posixtime/second stay exact. It is
    % either empty (the instant is reconstructed from SerialValue on demand) or
    % an array matching SerialValue in size.
    TimeMillis = []
  end

  methods
    function obj = datetime(varargin)
      obj.SerialValue = now();
      obj.FormatValue = 'default';
      obj.TimeZoneValue = '';
      if nargin == 0
        return
      end
      if nargin == 1 && isa(varargin{1}, 'datetime')
        obj = varargin{1};
        return
      end

      [args, options] = datetimeParseNameValue(varargin);
      obj.FormatValue = options.Format;
      obj.TimeZoneValue = options.TimeZone;
      % time zone the values below are read in, converted to TimeZone at the end
      sourceZone = options.TimeZone;

      if isempty(args)
        obj.SerialValue = datetimeNow(options.TimeZone);
        return
      end

      if numel(args) == 1
        value = args{1};
        if isa(value, 'datetime')
          obj = value;
          obj.FormatValue = options.Format;
          if options.HasTimeZone
            obj.TimeZone = options.TimeZone;
          end
          return
        elseif ischar(value) || isstring(value) || iscellstr(value)
          [serial, millis] = datetime.parseTextArray(value, options);
          obj.SerialValue = serial;
          obj.TimeMillis = millis;
        elseif isnumeric(value)
          [serial, millis] = datetime.fromSingleNumeric(value, options);
          obj.SerialValue = serial;
          obj.TimeMillis = millis;
          sourceZone = datetimeNumericSourceZone(options);
        elseif isa(value, 'missing')
          obj.SerialValue = NaN(size(value));
        else
          error('Nelson:time:invalidDatetimeInputArgument', 'Invalid %s.', 'datetime input argument');
        end
      elseif numel(args) >= 3 && numel(args) <= 7
        values = args;
        while numel(values) < 7
          values{end + 1} = 0;
        end
        [y, mo, d, h, mi, s, ms] = datetime.expandInputs(values{:});
        % Normalize out-of-range months (both greater than 12 and less than 1)
        % into the year, matching expected behavior where month 0 rolls back to
        % December of the previous year, month -1 to November, and so on.
        yearAdj = floor((mo - 1) ./ 12);
        mo = mo - yearAdj .* 12;
        y = y + yearAdj;
        nat = isnan(y) | isnan(mo) | isnan(d) | isnan(h) | isnan(mi) | isnan(s) | isnan(ms);
        secs = s + ms ./ 1000;
        serial = datenum(y, mo, d, h, mi, secs);
        millis = datetime.timeOfDayMillis(h, mi, secs);
        serial(nat) = NaN;
        millis(nat) = NaN;
        obj.SerialValue = serial;
        obj.TimeMillis = millis;
      else
        error('Nelson:time:invalidDatetimeInputArguments', 'Invalid %s.', 'datetime input arguments');
      end
      if ~isempty(obj.TimeZoneValue)
        [obj.SerialValue, obj.TimeMillis] = datetimeConvertZone(obj.SerialValue, ...
          obj.TimeMillis, sourceZone, obj.TimeZoneValue);
      end
    end

    function value = get.Format(obj)
      value = datetime.resolveDisplayFormat(obj.FormatValue, datetime.rawSerial(obj));
    end

    function obj = set.Format(obj, value)
      obj.FormatValue = datetime.validateTextScalar(value, 'Format');
    end

    function value = get.TimeZone(obj)
      value = obj.TimeZoneValue;
    end

    function obj = set.TimeZone(obj, value)
      [serial, millis, zone] = datetimeZoneChange(obj, value);
      obj.SerialValue = serial;
      obj.TimeMillis = millis;
      obj.TimeZoneValue = zone;
    end

    function value = get.Year(obj)
      value = datetime.component(obj, 1);
    end

    function obj = set.Year(obj, value)
      obj = datetime.setComponent(obj, 1, value);
    end

    function value = get.Month(obj)
      value = datetime.component(obj, 2);
    end

    function obj = set.Month(obj, value)
      obj = datetime.setComponent(obj, 2, value);
    end

    function value = get.Day(obj)
      value = datetime.component(obj, 3);
    end

    function obj = set.Day(obj, value)
      obj = datetime.setComponent(obj, 3, value);
    end

    function value = get.Hour(obj)
      value = datetime.component(obj, 4);
    end

    function obj = set.Hour(obj, value)
      obj = datetime.setComponent(obj, 4, value);
    end

    function value = get.Minute(obj)
      value = datetime.component(obj, 5);
    end

    function obj = set.Minute(obj, value)
      obj = datetime.setComponent(obj, 5, value);
    end

    function value = get.Second(obj)
      value = datetime.component(obj, 6);
    end

    function obj = set.Second(obj, value)
      obj = datetime.setComponent(obj, 6, value);
    end

    function out = horzcat(varargin)
      out = datetime.concat(2, varargin{:});
    end

    function out = vertcat(varargin)
      out = datetime.concat(1, varargin{:});
    end

    function out = transpose(obj)
      millis = datetime.storedMillis(obj);
      if ~isempty(millis)
        millis = millis.';
      end
      out = datetime.fromSerial(obj.SerialValue.', obj.FormatValue, obj.TimeZoneValue, millis);
    end

    function out = ctranspose(obj)
      out = transpose(obj);
    end

    function out = reshape(obj, varargin)
      millis = datetime.storedMillis(obj);
      if ~isempty(millis)
        millis = reshape(millis, varargin{:});
      end
      out = datetime.fromSerial(reshape(obj.SerialValue, varargin{:}), ...
        obj.FormatValue, obj.TimeZoneValue, millis);
    end

    function tf = isnan(obj)
      tf = isnan(datenum(obj));
    end

    function tf = isinf(obj)
      tf = isinf(datenum(obj));
    end

    function tf = isfinite(obj)
      tf = isfinite(datenum(obj));
    end

    function [out, index] = sort(obj, varargin)
      [values, index] = sort(datenum(obj), varargin{:});
      millis = datetime.storedMillis(obj);
      if ~isempty(millis)
        millis = millis(index);
      end
      out = datetime.fromSerial(values, obj.FormatValue, obj.TimeZoneValue, millis);
    end

    function [C, ia, ic] = unique(obj, varargin)
      % Set operations delegate to the serial-day representation, mirroring
      % sort/min/max, then rebuild the result as a datetime with the same
      % Format/TimeZone. Each NaT is a NaN serial, which unique keeps distinct.
      serialValues = datenum(obj);
      [values, ia, ic] = unique(serialValues, varargin{:});
      C = datetime.rebuildLike(obj, values, ia);
    end

    function [tf, loc] = ismember(a, b, varargin)
      b = datetimeAlignOperand(a, b);
      leftSerial = datetime.serialOf(a);
      rightSerial = datetime.serialOf(b);
      [tf, loc] = ismember(leftSerial, rightSerial, varargin{:});
    end

    function [C, ia, ib] = intersect(a, b, varargin)
      b = datetimeAlignOperand(a, b);
      leftSerial = datetime.serialOf(a);
      rightSerial = datetime.serialOf(b);
      [values, ia, ib] = intersect(leftSerial, rightSerial, varargin{:});
      if isa(a, 'datetime')
        C = datetime.rebuildLike(a, values, ia);
      else
        C = datetime.rebuildLike(b, values, ib);
      end
    end

    function [C, ia, ib] = union(a, b, varargin)
      % The result mixes elements from both inputs, so it is rebuilt from the
      % serial values (Format/TimeZone taken from the datetime operand).
      b = datetimeAlignOperand(a, b);
      leftSerial = datetime.serialOf(a);
      rightSerial = datetime.serialOf(b);
      [values, ia, ib] = union(leftSerial, rightSerial, varargin{:});
      if isa(a, 'datetime')
        ref = a;
      else
        ref = b;
      end
      C = datetime.fromSerial(values, ref.FormatValue, ref.TimeZoneValue);
    end

    function [C, ia] = setdiff(a, b, varargin)
      b = datetimeAlignOperand(a, b);
      leftSerial = datetime.serialOf(a);
      rightSerial = datetime.serialOf(b);
      [values, ia] = setdiff(leftSerial, rightSerial, varargin{:});
      if isa(a, 'datetime')
        C = datetime.rebuildLike(a, values, ia);
      else
        C = datetime.fromSerial(values, b.FormatValue, b.TimeZoneValue);
      end
    end

    function [out, index] = min(a, b, varargin)
      index = [];
      if nargin >= 2 && isa(b, 'datetime')
        b = datetimeAlignOperand(a, b);
        values = min(datenum(a), datenum(b));
      elseif nargin == 1
        [values, index] = min(datenum(a));
      else
        [values, index] = min(datenum(a), b, varargin{:});
      end
      out = datetime.fromSerial(values, a.FormatValue, a.TimeZoneValue);
    end

    function [out, index] = max(a, b, varargin)
      index = [];
      if nargin >= 2 && isa(b, 'datetime')
        b = datetimeAlignOperand(a, b);
        values = max(datenum(a), datenum(b));
      elseif nargin == 1
        [values, index] = max(datenum(a));
      else
        [values, index] = max(datenum(a), b, varargin{:});
      end
      out = datetime.fromSerial(values, a.FormatValue, a.TimeZoneValue);
    end

    function out = mean(obj, varargin)
      values = mean(datenum(obj), varargin{:});
      out = datetime.fromSerial(values, obj.FormatValue, obj.TimeZoneValue);
    end

    function out = median(obj, varargin)
      values = median(datenum(obj), varargin{:});
      out = datetime.fromSerial(values, obj.FormatValue, obj.TimeZoneValue);
    end

    function tf = isequal(obj, varargin)
      tf = true;
      for k = 1:numel(varargin)
        other = varargin{k};
        % datetimes of two time zones are equal at the same instants
        if ~isa(other, 'datetime') || ~isequal(size(obj), size(other)) ...
            || xor(isempty(obj.TimeZoneValue), isempty(other.TimeZoneValue)) ...
            || ~isequal(datenum(obj), datenum(datetimeAlignZone(other, obj.TimeZoneValue)))
          tf = false;
          return
        end
      end
    end

    function tf = isequalto(obj, other)
      tf = isequal(obj, other);
    end

    function tf = isequaln(obj, varargin)
      tf = true;
      for k = 1:numel(varargin)
        other = varargin{k};
        % datetimes of two time zones are equal at the same instants
        if ~isa(other, 'datetime') || ~isequal(size(obj), size(other)) ...
            || xor(isempty(obj.TimeZoneValue), isempty(other.TimeZoneValue)) ...
            || ~isequaln(datenum(obj), datenum(datetimeAlignZone(other, obj.TimeZoneValue)))
          tf = false;
          return
        end
      end
    end

    function out = plus(a, b)
      if isa(a, 'datetime') && isa(b, 'duration')
        [serial, millis] = datetimeAddSeconds(a, b.SecondsValue, 1);
        out = datetime.fromSerial(serial, a.FormatValue, a.TimeZoneValue, millis);
        return
      end
      if isa(a, 'datetime') && isa(b, 'calendarDuration')
        out = datetimeZoneNormalized(calendarDuration.addToDatetime(a, b, 1));
        return
      end
      if isa(a, 'duration') && isa(b, 'datetime')
        out = b + a;
        return
      end
      if isa(a, 'datetime') && isnumeric(b) && ~isempty(a.TimeZoneValue)
        [serial, millis] = datetimeAddSeconds(a, double(b) .* 86400, 1);
        out = datetime.fromSerial(serial, a.FormatValue, a.TimeZoneValue, millis);
        return
      end
      if isa(a, 'datetime') && isnumeric(b)
        [av, bv] = datetime.expandPair(a.SerialValue, double(b));
        out = datetime.fromSerial(av + bv, a.FormatValue, a.TimeZoneValue);
        return
      end
      if isnumeric(a) && isa(b, 'datetime')
        out = b + a;
        return
      end
      error('Nelson:time:additionDefinedTheseOperands', '%s is not defined for these operands.', 'Addition');
    end

    function out = minus(a, b)
      if isa(a, 'datetime') && isa(b, 'duration')
        [serial, millis] = datetimeAddSeconds(a, b.SecondsValue, -1);
        out = datetime.fromSerial(serial, a.FormatValue, a.TimeZoneValue, millis);
        return
      end
      if isa(a, 'datetime') && isa(b, 'calendarDuration')
        out = datetimeZoneNormalized(calendarDuration.addToDatetime(a, b, -1));
        return
      end
      if isa(a, 'datetime') && isa(b, 'datetime')
        datetime.checkPairSize(a.SerialValue, b.SerialValue);
        % zoned and unzoned datetimes do not subtract; instants of any two
        % time zones do.
        datetimeAlignZone(b, a.TimeZoneValue);
        % Difference from the millisecond-accurate instants so it is exact.
        out = duration.fromSeconds(datetime.secondsBetween(a, b));
        return
      end
      if isa(a, 'datetime') && isnumeric(b) && ~isempty(a.TimeZoneValue)
        [serial, millis] = datetimeAddSeconds(a, double(b) .* 86400, -1);
        out = datetime.fromSerial(serial, a.FormatValue, a.TimeZoneValue, millis);
        return
      end
      if isa(a, 'datetime') && isnumeric(b)
        [av, bv] = datetime.expandPair(a.SerialValue, double(b));
        out = datetime.fromSerial(av - bv, a.FormatValue, a.TimeZoneValue);
        return
      end
      error('Nelson:time:subtractionDefinedTheseOperands', '%s is not defined for these operands.', 'Subtraction');
    end

    function out = eq(a, b)
      [av, bv] = datetime.compareInputs(a, b);
      out = av == bv;
      out(isnan(av) | isnan(bv)) = false;
    end

    function out = ne(a, b)
      out = ~(a == b);
    end

    function out = lt(a, b)
      [av, bv] = datetime.compareInputs(a, b);
      out = av < bv;
    end

    function out = le(a, b)
      [av, bv] = datetime.compareInputs(a, b);
      out = av <= bv;
    end

    function out = gt(a, b)
      [av, bv] = datetime.compareInputs(a, b);
      out = av > bv;
    end

    function out = ge(a, b)
      [av, bv] = datetime.compareInputs(a, b);
      out = av >= bv;
    end

    function out = colon(a, b, c)
      if nargin == 2
        first = a;
        step = days(1);
        last = b;
      else
        first = a;
        step = b;
        last = c;
      end
      if isa(first, 'datetime') && isa(last, 'datetime') && isa(step, 'calendarDuration')
        % Calendar durations (caldays/calmonths/calyears) have variable
        % length. Each element is first + k*step computed from the original
        % start (not cumulatively), so month/year ends clamp like the
        % reference (e.g. Jan-31 + 2*calmonths(1) is Mar-31, not Mar-29).
        out = first([]);
        nextVal = first + step;
        if nextVal ~= first
          ascending = nextVal >= first;
          [stepYears, stepMonths, stepDays, stepTime] = ...
            split(step, {'years', 'months', 'days', 'time'});
          stepSeconds = seconds(stepTime);
          k = 0;
          while true
            kcal = calyears(k * stepYears) + calmonths(k * stepMonths) ...
              + caldays(k * stepDays);
            cur = first + kcal + seconds(k * stepSeconds);
            if (ascending && cur > last) || (~ascending && cur < last)
              break
            end
            out(end + 1) = cur;
            k = k + 1;
          end
        end
        return
      end
      if ~isa(first, 'datetime') || ~isa(step, 'duration') || ~isa(last, 'datetime')
        error('Nelson:time:datetimeColonOperandsExpected', '%s colon operands expected.', 'Datetime');
      end
      % steps counted from the exact elapsed seconds up to last's instant; a
      % zoned range steps on the UTC time line.
      datetimeAlignOperand(first, last); % zoned and unzoned ends do not mix
      stepSeconds = double(step);
      count = floor(datetime.secondsBetween(last, first) ./ stepSeconds + 1e-9);
      if ~isfinite(count)
        count = -1;
      end
      [serial, millis] = datetimeAddSeconds(first, (0:count) .* stepSeconds, 1);
      out = datetime.fromSerial(serial, first.FormatValue, first.TimeZoneValue, millis);
    end

    function value = datenum(obj)
      value = datetime.rawSerial(obj);
    end

    function value = datevec(obj)
      value = datevec(datenum(obj));
    end

    function out = char(obj, varargin)
      out = char(datetimeFormatText(obj, varargin{:}));
    end

    function out = string(obj, varargin)
      out = datetimeFormatText(obj, varargin{:});
      out(isnan(datenum(obj))) = missing;
    end

    function disp(obj)
      nelson.time.displayTextArray(datetimeDisplayText(obj), 'datetime', true);
    end

    function display(obj, varargin)
      if nargin == 1
        name = inputname(1);
      else
        name = varargin{1};
      end
      nelson.time.displayTextArray(datetimeDisplayText(obj), 'datetime', true, name);
    end
  end

  methods (Hidden)
    texts = compactElementText(obj)
  end

  methods (Static, Hidden)
    function obj = empty(varargin)
      obj = datetime();
      obj.SerialValue = double.empty(varargin{:});
    end

    function obj = fromSerial(serial, formatValue, timeZoneValue, timeMillis)
      obj = datetime();
      obj.SerialValue = double(serial);
      if nargin > 1
        obj.FormatValue = formatValue;
      end
      if nargin > 2
        obj.TimeZoneValue = timeZoneValue;
      end
      if nargin > 3
        % Optional precise time of day carried from the source. A mismatched
        % size is ignored later (storedMillis), so a bad value is never unsafe.
        obj.TimeMillis = timeMillis;
      end
    end

    function serial = rawSerial(obj)
      state = builtin('struct', obj);
      serial = state.SerialValue;
    end

    function serial = serialOf(x)
      % Serial-day values of a set-operation operand: a datetime yields its
      % serial value, a numeric operand is taken as already being a serial value.
      if isa(x, 'datetime')
        serial = datenum(x);
      else
        serial = double(x);
      end
    end

    function out = rebuildLike(obj, values, idx)
      % Rebuild a datetime holding the given serial values from a set operation,
      % carrying the source Format/TimeZone. When the source has a precise time
      % of day and the indices line up one-to-one with the values, that precise
      % time is reindexed too; otherwise (e.g. the 'rows' option) it is dropped
      % and the instant is reconstructed from the serial value.
      millis = datetime.storedMillis(obj);
      if ~isempty(millis) && numel(idx) == numel(values)
        millis = reshape(millis(idx), size(values));
      else
        millis = [];
      end
      out = datetime.fromSerial(values, obj.FormatValue, obj.TimeZoneValue, millis);
    end

    function millis = timeOfDayMillis(h, mi, s)
      % Time of day in milliseconds folded into [0, 86400000). Hour/minute/
      % second overflow (or negative values) wrap into the day exactly as the
      % serial-day value does, so this matches the fractional part of
      % SerialValue while staying at a small, fully precise magnitude.
      totalMillis = ((h .* 60 + mi) .* 60 + s) .* 1000;
      millis = mod(totalMillis, 86400000);
    end

    function millis = storedMillis(obj)
      % Return the precise time-of-day milliseconds when they have been recorded
      % and still match SerialValue in size; otherwise return empty so callers
      % fall back to reconstructing the instant from the serial-day value.
      state = builtin('struct', obj);
      millis = [];
      if isfield(state, 'TimeMillis')
        candidate = state.TimeMillis;
        if ~isempty(candidate) && isequal(size(candidate), size(state.SerialValue))
          millis = candidate;
        end
      end
    end

    function p = posixSecondsOf(obj)
      % Seconds since 1970-01-01 for the absolute instant. When the precise time
      % of day is available the whole-day count (recovered from SerialValue) is
      % combined with it so the result is exact; otherwise fall back to the
      % serial-day difference. SerialValue holds the wall-clock time in the
      % datetime's TimeZone, so its UTC offset is removed to reach the absolute
      % instant used for posixtime, differences and comparisons.
      [dayNumber, millis, offset] = datetime.instantParts(obj);
      p = (dayNumber - datenum(1970, 1, 1)) .* 86400 + millis ./ 1000 - offset;
    end

    function s = secondsBetween(a, b)
      % a - b in seconds. Whole days and times of day are subtracted apart so
      % the difference keeps the precision of the times of day instead of the
      % one of seconds counted since 1970.
      [dayA, millisA, offsetA] = datetime.instantParts(a);
      [dayB, millisB, offsetB] = datetime.instantParts(b);
      s = (dayA - dayB) .* 86400 + (millisA - millisB) ./ 1000 - (offsetA - offsetB);
    end

    function [dayNumber, millis, offset] = instantParts(obj)
      % Whole serial day, time of day in milliseconds and UTC offset in seconds
      % of each instant (see posixSecondsOf).
      serial = datetime.rawSerial(obj);
      millis = datetime.storedMillis(obj);
      if isempty(millis)
        dayNumber = floor(serial);
        millis = (serial - dayNumber) .* 86400000;
        millis(isinf(serial)) = 0;
      else
        dayNumber = round(serial - millis ./ 86400000);
      end
      offset = datetime.timeZoneOffsetSeconds(serial, obj.TimeZoneValue);
    end

    function offset = timeZoneOffsetSeconds(serial, timeZoneValue)
      % UTC offset in seconds applied to the wall-clock serial values of a zoned
      % datetime (UTC = wall-clock - offset). Unzoned datetimes have no offset,
      % so the fast path returns zeros without consulting the time zone
      % database and keeps the common case free of any extra cost. Named IANA
      % zones and '+HH:MM'/'-HH:MM' fixed offsets (including their DST rules) are
      % resolved from the bundled time zone data.
      if isempty(timeZoneValue)
        offset = zeros(size(serial));
        return
      end
      timezoneDataPath = [modulepath('time'), '/resources/tzdata'];
      offset = timezoneoffset(serial, timeZoneValue, timezoneDataPath);
      offset = reshape(offset, size(serial));
    end

    function fmt = resolveDisplayFormat(fmt, serial)
      % Resolve the 'default' format sentinel from the content:
      % the time part is displayed only if at least one element of the
      % array has a non zero time of day (aligned with expected behavior).
      if strcmp(fmt, 'default')
        s = serial(isfinite(serial(:)));
        if isempty(s) || all(s == floor(s))
          fmt = 'dd-MMM-uuuu';
        else
          fmt = 'dd-MMM-uuuu HH:mm:ss';
        end
      end
    end

    function out = concat(dim, varargin)
      values = cell(1, nargin - 1);
      millisParts = cell(1, nargin - 1);
      haveMillis = true;
      fmt = 'default';
      % the first datetime operand gives the time zone; text reads in it
      isDatetime = cellfun(@(x) isa(x, 'datetime'), varargin);
      zoneOwner = find(isDatetime, 1);
      tz = '';
      if ~isempty(zoneOwner)
        tz = varargin{zoneOwner}.TimeZoneValue;
      end
      for k = 1:numel(varargin)
        v = datetimeConcatOperand(varargin{k});
        if isDatetime(k) && k ~= zoneOwner
          v = datetimeAlignZone(v, tz, true);
        end
        if k == 1
          fmt = v.FormatValue;
        end
        values{k} = v.SerialValue;
        part = datetime.storedMillis(v);
        missingMillis = isempty(part);
        serialHasData = ~isempty(v.SerialValue);
        if missingMillis && serialHasData
          haveMillis = false;
        end
        millisParts{k} = part;
      end
      if ~haveMillis
        % At least one part lacks a precise time of day; reconstruct the whole
        % result from the serial-day value rather than mixing precisions.
        millis = [];
      else
        millis = cat(dim, millisParts{:});
      end
      out = datetime.fromSerial(cat(dim, values{:}), fmt, tz, millis);
    end

    function tf = isTextScalar(value)
      tf = ischar(value) || (isstring(value) && isscalar(value));
    end

    function value = validateTextScalar(value, name)
      if isstring(value)
        if ~isscalar(value)
          error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
        end
        value = char(value);
      end
      if ~ischar(value)
        error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
      end
    end

    function [serial, millis] = parseTextArray(value, options)
      % millis is the exact time of day of each parsed text, so instants read
      % from text compare and subtract exactly like numeric-component ones.
      if ischar(value)
        rows = cellstr(value);
        outSize = [size(value, 1), 1];
      elseif isstring(value)
        rows = cellstr(value(:));
        outSize = size(value);
      else
        rows = value(:);
        outSize = size(value);
      end
      serial = zeros(numel(rows), 1);
      millis = zeros(numel(rows), 1);
      % Compile the InputFormat once and capture the fallback year once, so
      % every text reuses them instead of rebuilding the regex and reading the
      % clock per element (which made this loop linear with a large constant).
      if isfield(options, 'InputFormat') && ~isempty(options.InputFormat)
        try
          options.CompiledFormat = datetime.compileInputFormat(options.InputFormat);
          baseDv = datevec(now());
          options.BaseYear = baseDv(1);
        catch
          options.CompiledFormat = [];
        end
      end
      if isfield(options, 'CompiledFormat') && ~isempty(options.CompiledFormat)
        % Match every text with a single vectorized regexp (the pattern is
        % compiled once for the whole array), then resolve the fields per text.
        % A per-text regexp call is ~10x slower than one call over the array.
        fields = options.CompiledFormat.fields;
        baseYear = options.BaseYear;
        allTokens = regexp(strtrim(rows), options.CompiledFormat.pattern, 'tokens', 'once');
        for k = 1:numel(rows)
          tok = allTokens{k};
          if isempty(tok)
            % No match: a keyword ('now', 'NaT', ...) or a value for the
            % datenum fallback. Resolve it with the per-text parser.
            [serial(k), millis(k)] = datetime.parseOneText(rows{k}, options);
          else
            [serial(k), millis(k)] = datetime.applyFormatTokens(tok, fields, baseYear);
          end
        end
      else
        for k = 1:numel(rows)
          [serial(k), millis(k)] = datetime.parseOneText(rows{k}, options);
        end
      end
      serial = reshape(serial, outSize);
      millis = reshape(millis, outSize);
    end

    function [serial, millis] = parseOneText(value, options)
      value = char(value);
      millis = [];
      switch lower(strtrim(value))
        case 'now'
          serial = datetimeNow(options.TimeZone);
        case 'today'
          serial = floor(datetimeNow(options.TimeZone));
        case 'tomorrow'
          serial = floor(datetimeNow(options.TimeZone)) + 1;
        case 'yesterday'
          serial = floor(datetimeNow(options.TimeZone)) - 1;
        case 'nat'
          serial = NaN;
        otherwise
          try
            if isempty(options.InputFormat)
              serial = datenum(value);
            else
              try
                if isfield(options, 'CompiledFormat') && ~isempty(options.CompiledFormat)
                  [serial, millis] = datetime.applyInputFormat(value, options.CompiledFormat, options.BaseYear);
                else
                  [serial, millis] = datetime.parseWithInputFormat(value, options.InputFormat);
                end
              catch
                % Fall back to the numeric datenum parser for formats the
                % classdef parser does not handle.
                serial = datenum(value, datetime.toDateNumberFormat(options.InputFormat));
              end
            end
          catch
            serial = NaN;
          end
      end
      if isempty(millis)
        % The text parsers resolve to the millisecond, while the serial-day
        % value carries a rounding error of a few microseconds: rounding the
        % time of day to the millisecond recovers the exact instant.
        millis = mod(round(mod(serial, 1) .* 86400000), 86400000);
      end
    end

    function [serial, millis] = fromSingleNumeric(value, options)
      % millis is the precise millisecond instant when it can be derived
      % without loss (posixtime/epochtime inputs); otherwise it is empty and
      % the instant is later reconstructed from the serial-day value.
      millis = [];
      if isempty(options.ConvertFrom)
        if ismatrix(value) && (size(value, 2) == 3 || size(value, 2) == 6)
          serial = datenum(value);
        else
          serial = double(value);
        end
        return
      end
      switch lower(options.ConvertFrom)
        case 'datenum'
          serial = double(value);
        case 'posixtime'
          % Keep the time of day precise so posixtime round-trips exactly.
          millis = mod(double(value), 86400) .* 1000;
          serial = datenum(1970, 1, 1) + double(value) ./ 86400;
        case 'juliandate'
          serial = double(value) - 1721058.5;
        case {'excel', 'exceltime'}
          % Excel serial dates count from 1899-12-30 (the 1900 date system
          % includes a phantom leap day for 1900-02-29).
          serial = double(value) + datenum(1899, 12, 30);
        case 'epochtime'
          serial = datenum(options.Epoch) + double(value) ./ double(options.TicksPerSecond) ./ 86400;
        otherwise
          error('Nelson:time:unsupportedDatetimeConversion', 'Unsupported %s.', 'datetime conversion');
      end
    end

    function outFmt = toDateNumberFormat(fmt)
      outFmt = char(fmt);
      outFmt = strrep(outFmt, 'uuuu', 'yyyy');
      outFmt = strrep(outFmt, 'MMM', 'mmm');
      outFmt = strrep(outFmt, 'MM', 'mm');
      outFmt = strrep(outFmt, 'HH:mm', 'HH:MM');
      outFmt = strrep(outFmt, ':mm:', ':MM:');
      outFmt = strrep(outFmt, ':mm', ':MM');
      outFmt = strrep(outFmt, 'ss', 'SS');
      outFmt = strrep(outFmt, 'SSS', 'FFF');
    end

    function spec = compileInputFormat(fmt)
      % Build the regex pattern and ordered field list for an LDML InputFormat
      % once. Parsing an array of texts that share a format reuses this spec
      % instead of re-tokenizing and rebuilding (and recompiling) the pattern
      % per element, which made datetime(text, 'InputFormat', ...) take time
      % proportional to the number of texts times a large per-text constant.
      segs = datetime.tokenizeInputFormat(char(fmt));
      pattern = '^';
      fields = {};
      for k = 1:numel(segs)
        seg = segs{k};
        if seg.isField
          % When a numeric token is immediately followed by another numeric
          % token (no literal separator between them), the format is
          % delimiter-less and the token must consume exactly its declared
          % width; otherwise a greedy match would let the first token swallow
          % the digits belonging to the following token (e.g. yyyyMMdd on
          % '20210315'). Delimited formats keep the flexible width they
          % already relied on.
          fixedWidth = false;
          if datetime.isNumericField(seg.letter, seg.count) ...
              && k < numel(segs) && segs{k + 1}.isField ...
              && datetime.isNumericField(segs{k + 1}.letter, segs{k + 1}.count)
            fixedWidth = true;
          end
          [grp, name] = datetime.fieldRegex(seg.letter, seg.count, fixedWidth);
          pattern = [pattern, grp];
          fields{end + 1} = name;
        else
          pattern = [pattern, datetime.regexEscape(seg.text)];
        end
      end
      spec = struct('pattern', [pattern, '$'], 'fields', {fields});
    end

    function [serial, millis] = parseWithInputFormat(value, fmt)
      % Parse a single text value against an LDML InputFormat. This handles
      % quoted literals (e.g. the escaped 'T' in yyyy-MM-dd'T'HH:mm:ss) that
      % the numeric datenum format parser mishandles.
      dv = datevec(now());
      [serial, millis] = datetime.applyInputFormat(value, datetime.compileInputFormat(fmt), dv(1));
    end

    function [serial, millis] = applyInputFormat(value, spec, baseYear)
      % Parse a single text value against a compiled InputFormat spec (see
      % compileInputFormat). baseYear supplies the year when the format omits
      % it, captured once per call so every text in an array shares it.
      tokens = regexp(strtrim(char(value)), spec.pattern, 'tokens', 'once');
      if isempty(tokens)
        error('Nelson:time:failedConvertTextDatetime', 'Failed to convert text to %s.', 'datetime');
      end
      [serial, millis] = datetime.applyFormatTokens(tokens, spec.fields, baseYear);
    end

    function [serial, millis] = applyFormatTokens(tokens, fields, baseYear)
      % Resolve a datetime from format tokens already matched by the compiled
      % pattern (see applyInputFormat). Kept separate so an array of texts can
      % be matched with a single vectorized regexp and only the field
      % resolution runs per text.
      y = baseYear;
      mo = 1;
      d = 1;
      h = 0;
      mi = 0;
      s = 0;
      frac = 0;
      ampm = '';
      for k = 1:numel(fields)
        tok = tokens{k};
        switch fields{k}
          case 'year'
            y = str2double(tok);
            if numel(strtrim(tok)) <= 2
              y = y + 2000;
            end
          case 'month'
            mo = str2double(tok);
          case 'monthname'
            mo = datetime.monthNameToNumber(tok);
          case 'day'
            d = str2double(tok);
          case 'hour'
            h = str2double(tok);
          case 'minute'
            mi = str2double(tok);
          case 'second'
            s = str2double(tok);
          case 'frac'
            frac = str2double(['0.', tok]);
          case 'ampm'
            ampm = lower(tok);
        end
      end
      if strcmp(ampm, 'pm') && h < 12
        h = h + 12;
      elseif strcmp(ampm, 'am') && h == 12
        h = 0;
      end
      if any(isnan([y, mo, d, h, mi, s, frac]))
        error('Nelson:time:failedConvertTextDatetime', 'Failed to convert text to %s.', 'datetime');
      end
      serial = datenum(y, mo, d, h, mi, s + frac);
      millis = datetime.timeOfDayMillis(h, mi, s + frac);
    end

    function segs = tokenizeInputFormat(fmt)
      segs = {};
      letters = 'yuMdHhmsSa';
      i = 1;
      n = numel(fmt);
      litbuf = '';
      while i <= n
        c = fmt(i);
        if c == ''''
          if i < n && fmt(i + 1) == ''''
            litbuf = [litbuf, ''''];
            i = i + 2;
            continue
          end
          i = i + 1;
          while i <= n && fmt(i) ~= ''''
            litbuf = [litbuf, fmt(i)];
            i = i + 1;
          end
          i = i + 1;
          continue
        end
        if any(c == letters)
          if ~isempty(litbuf)
            segs{end + 1} = struct('isField', false, 'letter', '', 'count', 0, 'text', litbuf);
            litbuf = '';
          end
          j = i;
          while j <= n && fmt(j) == c
            j = j + 1;
          end
          segs{end + 1} = struct('isField', true, 'letter', c, 'count', j - i, 'text', '');
          i = j;
        else
          litbuf = [litbuf, c];
          i = i + 1;
        end
      end
      if ~isempty(litbuf)
        segs{end + 1} = struct('isField', false, 'letter', '', 'count', 0, 'text', litbuf);
      end
    end

    function tf = isNumericField(letter, count)
      % A field whose value is written as digits. Month names (MMM/MMMM) and
      % the AM/PM marker are textual and therefore excluded.
      switch letter
        case {'y', 'u', 'd', 'H', 'h', 'm', 's', 'S'}
          tf = true;
        case 'M'
          tf = count < 3;
        otherwise
          tf = false;
      end
    end

    function [grp, name] = fieldRegex(letter, count, fixedWidth)
      if nargin < 3
        fixedWidth = false;
      end
      % A fixed-width numeric field consumes exactly its declared width so
      % adjacent tokens in a delimiter-less format do not run together.
      if fixedWidth
        digits = sprintf('(\\d{%d})', count);
      else
        digits = '';
      end
      switch letter
        case {'y', 'u'}
          if isempty(digits)
            grp = '(\d+)';
          else
            grp = digits;
          end
          name = 'year';
        case 'M'
          if count >= 3
            grp = '([A-Za-z]+)';
            name = 'monthname';
          else
            if isempty(digits)
              grp = '(\d{1,2})';
            else
              grp = digits;
            end
            name = 'month';
          end
        case 'd'
          if isempty(digits)
            grp = '(\d{1,2})';
          else
            grp = digits;
          end
          name = 'day';
        case {'H', 'h'}
          if isempty(digits)
            grp = '(\d{1,2})';
          else
            grp = digits;
          end
          name = 'hour';
        case 'm'
          if isempty(digits)
            grp = '(\d{1,2})';
          else
            grp = digits;
          end
          name = 'minute';
        case 's'
          if isempty(digits)
            grp = '(\d{1,2})';
          else
            grp = digits;
          end
          name = 'second';
        case 'S'
          if isempty(digits)
            grp = '(\d+)';
          else
            grp = digits;
          end
          name = 'frac';
        case 'a'
          grp = '([AaPp][Mm])';
          name = 'ampm';
        otherwise
          grp = '(\S+)';
          name = 'ignore';
      end
    end

    function out = regexEscape(text)
      specials = '\^$.|?*+()[]{}';
      out = '';
      for k = 1:numel(text)
        c = text(k);
        if any(c == specials)
          out = [out, '\', c];
        else
          out = [out, c];
        end
      end
    end

    function m = monthNameToNumber(name)
      name = lower(strtrim(char(name)));
      abbr = {'jan', 'feb', 'mar', 'apr', 'may', 'jun', ...
        'jul', 'aug', 'sep', 'oct', 'nov', 'dec'};
      m = NaN;
      for k = 1:12
        if strncmp(name, abbr{k}, 3)
          m = k;
          return
        end
      end
    end

    function [a, b, c, d, e, f, g] = expandInputs(a, b, c, d, e, f, g)
      targetSize = datetime.commonSize(a, b, c, d, e, f, g);
      a = datetime.expandToSize(a, targetSize);
      b = datetime.expandToSize(b, targetSize);
      c = datetime.expandToSize(c, targetSize);
      d = datetime.expandToSize(d, targetSize);
      e = datetime.expandToSize(e, targetSize);
      f = datetime.expandToSize(f, targetSize);
      g = datetime.expandToSize(g, targetSize);
    end

    function targetSize = commonSize(varargin)
      targetSize = [1 1];
      for k = 1:numel(varargin)
        if ~isscalar(varargin{k})
          if isequal(targetSize, [1 1])
            targetSize = size(varargin{k});
          elseif ~isequal(targetSize, size(varargin{k}))
            error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
          end
        end
      end
    end

    function x = expandToSize(x, targetSize)
      if isscalar(x) && ~isequal(targetSize, [1 1])
        x = repmat(x, targetSize);
      elseif ~isequal(size(x), targetSize)
        error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
      end
    end

    function [a, b] = expandPair(a, b)
      if isscalar(a) && ~isscalar(b)
        a = repmat(a, size(b));
      elseif isscalar(b) && ~isscalar(a)
        b = repmat(b, size(a));
      elseif ~isequal(size(a), size(b))
        error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
      end
    end

    function checkPairSize(a, b)
      if ~isscalar(a) && ~isscalar(b) && ~isequal(size(a), size(b))
        error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
      end
    end

    function [a, b] = compareInputs(a, b)
      % missing compares as NaT: never equal, never ordered.
      withMissing = isa(a, 'missing') || isa(b, 'missing');
      if isa(a, 'missing')
        a = datetime(a);
      end
      if isa(b, 'missing')
        b = datetime(b);
      end
      if ~isa(a, 'datetime') || ~isa(b, 'datetime')
        error('Nelson:time:datetimeOperandsExpected', '%s operands expected.', 'Datetime');
      end
      if ~withMissing && xor(isempty(a.TimeZoneValue), isempty(b.TimeZoneValue))
        datetimeAlignZone(b, a.TimeZoneValue);
      end
      % Compare absolute instants so datetimes in different time zones order by
      % the moment they represent, not by their wall-clock reading. For unzoned
      % datetimes the offset is zero, so this is a monotonic rescale of the
      % serial-day value and the ordering is unchanged.
      [a, b] = datetime.expandPair(datetime.posixSecondsOf(a), datetime.posixSecondsOf(b));
    end

    function value = component(obj, index)
      serial = datenum(obj);
      dv = datevec(serial(:));
      value = reshape(dv(:, index), size(serial));
      if index == 6
        % The seconds column of datevec inherits the microsecond loss of the
        % serial-day value. When the precise time of day is available, recover
        % the seconds (fraction included) from it instead.
        millis = datetime.storedMillis(obj);
        if ~isempty(millis)
          secondOfMinute = mod(millis(:) ./ 1000, 60);
          value = reshape(secondOfMinute, size(serial));
        end
      end
      value(isnan(serial)) = NaN;
    end

    function obj = setComponent(obj, index, value)
      serialCurrent = datenum(obj);
      dv = datevec(serialCurrent(:));
      value = datetime.expandToSize(value, size(serialCurrent));
      dv(:, index) = value(:);
      serial = datenum(dv);
      % Rebuild the precise time of day. When a field other than the seconds is
      % edited, keep the exact seconds recovered from the stored value so its
      % fraction is not degraded by the datevec round-trip.
      secondsColumn = dv(:, 6);
      if index ~= 6
        precise = datetime.storedMillis(obj);
        if ~isempty(precise)
          secondsColumn = mod(precise(:) ./ 1000, 60);
        end
      end
      millis = datetime.timeOfDayMillis(dv(:, 4), dv(:, 5), secondsColumn);
      nanRows = any(isnan(dv), 2);
      serial(nanRows) = NaN;
      millis(nanRows) = NaN;
      obj.SerialValue = reshape(serial, size(serialCurrent));
      obj.TimeMillis = reshape(millis, size(serialCurrent));
      obj = datetimeZoneNormalized(obj);
    end

    function out = formatOne(serial, fmt)
      if isnan(serial)
        out = "NaT";
        return
      end
      try
        out = string(datetime.renderLdmlFormat(serial, char(fmt)));
      catch
        out = string(datestr(serial));
      end
    end

    function out = renderLdmlFormat(serial, fmt)
      % Render a serial date number using an LDML/Unicode date format (the
      % convention of the datetime Format property). Consecutive identical
      % letters form one field; text inside single quotes is literal ('' is an
      % escaped single quote). Fields are emitted left to right into the output
      % so a substituted name (which may itself contain field letters) can never
      % disturb a later field.
      dv = datevec(serial);
      Y = dv(1); Mo = dv(2); D = dv(3); H = dv(4); Mi = dv(5);
      Ssec = floor(dv(6) + 1e-6);
      ms = round((dv(6) - Ssec) * 1000);
      if ms >= 1000
        ms = ms - 1000;
        Ssec = Ssec + 1;
      end
      wd = weekday(serial); % 1 = Sunday .. 7 = Saturday
      out = '';
      n = length(fmt);
      i = 1;
      while i <= n
        c = fmt(i);
        if c == ''''
          % Single quote: an escaped quote ('') or a quoted literal run.
          if i < n && fmt(i + 1) == ''''
            out = [out, ''''];
            i = i + 2;
            continue;
          end
          j = i + 1;
          while j <= n && fmt(j) ~= ''''
            out = [out, fmt(j)];
            j = j + 1;
          end
          i = j + 1;
          continue;
        end
        if isletter(c)
          j = i;
          while j <= n && fmt(j) == c
            j = j + 1;
          end
          out = [out, datetime.renderLdmlToken(c, j - i, Y, Mo, D, H, Mi, Ssec, ms, wd)];
          i = j;
        else
          out = [out, c];
          i = i + 1;
        end
      end
    end

    function s = renderLdmlToken(c, cnt, Y, Mo, D, H, Mi, Ssec, ms, wd)
      monthFull = {'January', 'February', 'March', 'April', 'May', 'June', ...
        'July', 'August', 'September', 'October', 'November', 'December'};
      monthAbbr = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', ...
        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
      dayFull = {'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', ...
        'Friday', 'Saturday'};
      dayAbbr = {'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'};
      switch c
        case {'y', 'u'}
          % Year: 'yy' -> two digits, otherwise the full year (min width cnt).
          if cnt == 2
            s = sprintf('%02d', mod(Y, 100));
          else
            s = sprintf(['%0', int2str(cnt), 'd'], Y);
          end
        case {'M', 'L'}
          % Month: 1 digit / 2 digits / abbreviated name / full name.
          if cnt >= 4
            s = monthFull{Mo};
          elseif cnt == 3
            s = monthAbbr{Mo};
          elseif cnt == 2
            s = sprintf('%02d', Mo);
          else
            s = sprintf('%d', Mo);
          end
        case 'd'
          % Day of month: unpadded for a single 'd', two digits otherwise.
          if cnt >= 2
            s = sprintf('%02d', D);
          else
            s = sprintf('%d', D);
          end
        case {'e', 'E', 'c'}
          % Day of week name: full for 4+ letters, abbreviated otherwise.
          if cnt >= 4
            s = dayFull{wd};
          else
            s = dayAbbr{wd};
          end
        case 'H'
          % Hour, 24-hour clock.
          if cnt >= 2
            s = sprintf('%02d', H);
          else
            s = sprintf('%d', H);
          end
        case 'h'
          % Hour, 12-hour clock.
          h12 = mod(H, 12);
          if h12 == 0
            h12 = 12;
          end
          if cnt >= 2
            s = sprintf('%02d', h12);
          else
            s = sprintf('%d', h12);
          end
        case 'm'
          % Minute.
          if cnt >= 2
            s = sprintf('%02d', Mi);
          else
            s = sprintf('%d', Mi);
          end
        case 's'
          % Second.
          if cnt >= 2
            s = sprintf('%02d', Ssec);
          else
            s = sprintf('%d', Ssec);
          end
        case 'S'
          % Fractional second: cnt digits derived from the milliseconds.
          base = sprintf('%03d', ms);
          if cnt <= 3
            s = base(1:cnt);
          else
            s = [base, repmat('0', 1, cnt - 3)];
          end
        case 'a'
          % Meridiem marker.
          if H < 12
            s = 'AM';
          else
            s = 'PM';
          end
        case {'Q', 'q'}
          % Quarter of the year.
          q = floor((Mo - 1) / 3) + 1;
          if cnt >= 3
            s = ['Q', int2str(q)];
          elseif cnt == 2
            s = sprintf('%02d', q);
          else
            s = sprintf('%d', q);
          end
        case 'D'
          % Day of year, zero-padded to cnt digits.
          doy = floor(datenum(Y, Mo, D)) - floor(datenum(Y, 1, 1)) + 1;
          s = sprintf(['%0', int2str(cnt), 'd'], doy);
        otherwise
          % Unrecognized field letter: emit it verbatim.
          s = repmat(c, 1, cnt);
      end
    end
  end
end
%=============================================================================
