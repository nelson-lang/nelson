%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t2 = dateshift(t, mode, unitOrDay, rule)
  narginchk(3, 4);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  mode = lower(char(mode));
  if nargin < 4
    if strcmp(mode, 'dayofweek')
      rule = 'next';
    else
      rule = 'current';
    end
  end

  serial = datenum(t);
  switch mode
    case {'start', 'end'}
      shifted = shiftUnit(serial, mode, lower(char(unitOrDay)), rule);
    case 'dayofweek'
      shifted = shiftDayOfWeek(serial, unitOrDay, rule);
    otherwise
      error('Nelson:time:invalidDateshiftMode', 'Invalid %s.', 'dateshift mode');
  end
  t2 = datetime(shifted, 'ConvertFrom', 'datenum', 'Format', struct(t).FormatValue, 'TimeZone', t.TimeZone);
end
%=============================================================================
function shifted = shiftUnit(serial, mode, unit, rule)
  base = applyUnitBoundary(serial, mode, unit);
  step = ruleToStep(rule, serial);
  shifted = base;
  if ischar(rule) || (isstring(rule) && isscalar(rule))
    ruleText = lower(char(rule));
    if strcmp(ruleText, 'next')
      step = ones(size(serial));
    elseif strcmp(ruleText, 'previous')
      step = -ones(size(serial));
    elseif strcmp(ruleText, 'nearest')
      previous = shiftUnit(serial, mode, unit, 'previous');
      nextValue = shiftUnit(serial, mode, unit, 'next');
      useNext = abs(nextValue - serial) < abs(serial - previous);
      shifted = previous;
      shifted(useNext) = nextValue(useNext);
      return
    else
      step = zeros(size(serial));
    end
  end
  if any(step(:) ~= 0)
    shifted = addUnit(base, unit, step);
    shifted = applyUnitBoundary(shifted, mode, unit);
  end
  shifted(isnan(serial)) = NaN;
end
%=============================================================================
function out = applyUnitBoundary(serial, mode, unit)
  v = datevec(serial(:));
  switch unit
    case 'year'
      if strcmp(mode, 'start')
        v(:, 2:6) = repmat([1 1 0 0 0], size(v, 1), 1);
      else
        v(:, 2:6) = [repmat([12 31 0 0], size(v, 1), 1), zeros(size(v, 1), 1)];
      end
    case 'quarter'
      qStart = floor((v(:, 2) - 1) ./ 3) .* 3 + 1;
      if strcmp(mode, 'start')
        v(:, 2) = qStart;
        v(:, 3:6) = repmat([1 0 0 0], size(v, 1), 1);
      else
        v(:, 2) = qStart + 2;
        v(:, 3) = eomday(v(:, 1), v(:, 2));
        v(:, 4:6) = 0;
      end
    case 'month'
      if strcmp(mode, 'start')
        v(:, 3:6) = repmat([1 0 0 0], size(v, 1), 1);
      else
        v(:, 3) = eomday(v(:, 1), v(:, 2));
        v(:, 4:6) = 0;
      end
    case 'week'
      dow = weekday(serial(:));
      if strcmp(mode, 'start')
        out = floor(serial(:)) - (dow - 1);
        out = reshape(out, size(serial));
        return
      else
        out = floor(serial(:)) + (7 - dow);
        out = reshape(out, size(serial));
        return
      end
    case 'day'
      if strcmp(mode, 'start')
        out = floor(serial);
      else
        out = floor(serial) + 1;
      end
      return
    case 'hour'
      if strcmp(mode, 'start')
        out = floor(serial .* 24) ./ 24;
      else
        out = floor(serial .* 24) ./ 24 + 1 ./ 24;
      end
      return
    case 'minute'
      if strcmp(mode, 'start')
        out = floor(serial .* 1440) ./ 1440;
      else
        out = floor(serial .* 1440) ./ 1440 + 1 ./ 1440;
      end
      return
    case 'second'
      if strcmp(mode, 'start')
        out = floor(serial .* 86400) ./ 86400;
      else
        out = floor(serial .* 86400) ./ 86400 + 1 ./ 86400;
      end
      return
    otherwise
      error('Nelson:time:invalidDateshiftUnit', 'Invalid %s.', 'dateshift unit');
  end
  out = reshape(datenum(v), size(serial));
end
%=============================================================================
function out = addUnit(serial, unit, n)
  v = datevec(serial(:));
  n = expandRule(n, size(serial));
  n = n(:);
  switch unit
    case 'year'
      v(:, 1) = v(:, 1) + n;
    case 'quarter'
      v(:, 2) = v(:, 2) + 3 .* n;
    case 'month'
      v(:, 2) = v(:, 2) + n;
    case 'week'
      out = serial + reshape(7 .* n, size(serial));
      return
    case 'day'
      out = serial + reshape(n, size(serial));
      return
    case 'hour'
      out = serial + reshape(n ./ 24, size(serial));
      return
    case 'minute'
      out = serial + reshape(n ./ 1440, size(serial));
      return
    case 'second'
      out = serial + reshape(n ./ 86400, size(serial));
      return
  end
  out = reshape(datenum(v), size(serial));
end
%=============================================================================
function shifted = shiftDayOfWeek(serial, dow, rule)
  target = resolveDayOfWeek(dow, serial);
  current = weekday(serial);
  if ischar(rule) || (isstring(rule) && isscalar(rule))
    ruleText = lower(char(rule));
    switch ruleText
      case {'next', 'current'}
        delta = mod(target - current, 7);
      case 'previous'
        delta = -mod(current - target, 7);
      case 'nearest'
        forward = mod(target - current, 7);
        backward = -mod(current - target, 7);
        useForward = abs(forward) <= abs(backward);
        delta = backward;
        delta(useForward) = forward(useForward);
      otherwise
        error('Nelson:time:invalidDateshiftRule', 'Invalid %s.', 'dateshift rule');
    end
  else
    n = expandRule(rule, size(serial));
    delta = mod(target - current, 7) + (n - 1) .* 7;
    delta(n == 0) = mod(target(n == 0) - current(n == 0), 7);
    delta(n < 0) = -mod(current(n < 0) - target(n < 0), 7) + (n(n < 0) + 1) .* 7;
  end
  shifted = floor(serial) + delta + (serial - floor(serial));
  shifted(isnan(serial)) = NaN;
end
%=============================================================================
function target = resolveDayOfWeek(dow, serial)
  if isnumeric(dow)
    target = expandRule(dow, size(serial));
    return
  end
  text = lower(char(dow));
  names = {'sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday'};
  shortNames = {'sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat'};
  idx = find(strcmp(text, names) | strcmp(text, shortNames));
  if ~isempty(idx)
    target = repmat(idx, size(serial));
    return
  end
  if strcmp(text, 'weekend')
    current = weekday(serial);
    target = repmat(7, size(serial));
    target(current == 1) = 1;
    return
  end
  if strcmp(text, 'weekday')
    current = weekday(serial);
    target = current;
    target(current == 1) = 2;
    target(current == 7) = 2;
    return
  end
  error('Nelson:time:invalidDayWeek', 'Invalid %s.', 'day of week');
end
%=============================================================================
function step = ruleToStep(rule, serial)
  if isnumeric(rule)
    step = expandRule(rule, size(serial));
  else
    step = zeros(size(serial));
  end
end
%=============================================================================
function rule = expandRule(rule, targetSize)
  rule = double(rule);
  if isscalar(rule)
    rule = repmat(rule, targetSize);
  elseif ~isequal(size(rule), targetSize)
    error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
  end
end
%=============================================================================
