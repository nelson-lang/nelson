%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = between(t1, t2, components, varargin)
  % Calendar difference t2 - t1 split greedily into the requested components,
  % largest first: each takes the most whole units that do not go past t2.
  narginchk(2, 5);
  if nargin < 3
    units = parseComponents({'years', 'months', 'days', 'time'});
  else
    units = parseComponents(components);
  end
  method = parseArithmeticMethod(varargin, units);
  [first, last, nat] = validPairs(asDatetime(t1), asDatetime(t2));
  forward = last >= first;
  if ~strcmp(method, 'default')
    c = methodDifference(first, last, forward, nat, method);
    return
  end
  % each component starts where the previous one stopped
  anchor = first;
  monthsValue = zeros(size(first));
  if units.years
    k = 12 .* wholeMonthUnits(anchor, last, 12, forward);
    anchor = anchor + calmonths(k);
    monthsValue = monthsValue + k;
  end
  if units.quarters
    % whole months are walked and the extra months walked back, so a month-end day can move
    m = wholeMonthUnits(anchor, last, 1, forward);
    k = 3 .* fix(m ./ 3);
    anchor = anchor + calmonths(m) - calmonths(m - k);
    monthsValue = monthsValue + k;
  end
  if units.months
    k = wholeMonthUnits(anchor, last, 1, forward);
    anchor = anchor + calmonths(k);
    monthsValue = monthsValue + k;
  end
  daysValue = zeros(size(first));
  for u = [7 1; units.weeks units.days]
    if u(2)
      reached = datenum(anchor + caldays(daysValue));
      daysValue = daysValue + u(1) .* fix((datenum(last) - reached) ./ u(1));
    end
  end
  secondsValue = zeros(size(first));
  if units.time
    secondsValue = seconds(last - (anchor + caldays(daysValue)));
  end
  monthsValue(nat) = NaN;
  daysValue(nat) = NaN;
  secondsValue(nat) = NaN;
  c = calendarDuration.fromParts(monthsValue, daysValue, secondsValue, units.format);
end
%=============================================================================
function c = methodDifference(first, last, forward, nat, method)
  % whole months (quarters) kept at month (quarter) ends by the method
  step = 1 + 2 * strcmp(method, 'endofquarter');
  monthsValue = step .* wholeMonthUnits(first, last, step, forward, method);
  zeroParts = zeros(size(monthsValue));
  monthsValue(nat) = NaN;
  zeroParts(nat) = NaN;
  formats = struct('endofmonth', 'mdt', 'endofquarter', 'qmdt');
  c = calendarDuration.fromParts(monthsValue, zeroParts, zeroParts, formats.(method), method);
end
%=============================================================================
function method = parseArithmeticMethod(options, units)
  % 'ArithmeticMethod': 'endofmonth' needs the 'months' component only,
  % 'endofquarter' the 'quarters' component only
  method = 'default';
  if isempty(options)
    return
  end
  if numel(options) ~= 2 || ~(ischar(options{1}) || isstring(options{1})) ...
      || ~strcmpi(char(options{1}), 'ArithmeticMethod')
    error('Nelson:time:invalidBetweenOption', 'Invalid %s.', 'between option');
  end
  method = calendarDuration.validateMethod(options{2});
  needed = struct('default', '', 'endofmonth', 'months', 'endofquarter', 'quarters');
  requested = [units.years, units.quarters, units.months, units.weeks, units.days, units.time];
  if ~isempty(needed.(method)) && (sum(requested) ~= 1 || ~units.(needed.(method)))
    error('Nelson:time:arithmeticMethodComponents', ...
      'With the arithmetic method ''%s'', the components must be ''%s''.', method, needed.(method));
  end
end
%=============================================================================
function k = wholeMonthUnits(t1, t2, unitMonths, forward, varargin)
  % most whole units of unitMonths months after t1 not past t2 (optional
  % arithmetic method of the months added)
  method = [varargin, {'default'}];
  v1 = datevec(t1);
  v2 = datevec(t2);
  remaining = (v2(:, 1) - v1(:, 1)) .* 12 + (v2(:, 2) - v1(:, 2));
  k = reshape(fix(remaining ./ unitMonths), size(t1));
  % the calendar month count may be one too many when the day or time is not reached
  reached = t1 + calendarDuration(0, k .* unitMonths, 0, 'ArithmeticMethod', method{1});
  overshoot = (forward & reached > t2) | (~forward & reached < t2);
  k(overshoot) = k(overshoot) - sign(k(overshoot));
end
%=============================================================================
function units = parseComponents(components)
  % requested components and the display format they give
  list = components;
  if ischar(list) || isstring(list)
    list = cellstr(list);
  end
  if ~iscellstr(list) || isempty(list)
    error('Nelson:time:invalidBetweenComponents', 'Invalid %s.', 'calendar components');
  end
  names = {'years', 'quarters', 'months', 'weeks', 'days', 'time'};
  units = struct('years', false, 'quarters', false, 'months', false, 'weeks', false, 'days', false, 'time', false);
  for k = 1:numel(list)
    name = lower(list{k});
    match = strcmp(names, name) | strcmp(names, [name, 's']);
    if ~any(match)
      error('Nelson:time:invalidBetweenComponents', 'Invalid %s.', 'calendar components');
    end
    units.(names{find(match, 1)}) = true;
  end
  letters = {'y', 'q', 'm', 'w', 'd', 't'};
  shown = [units.years, units.quarters, true, units.weeks, true, true];
  units.format = [letters{shown}];
end
%=============================================================================
function t = asDatetime(t)
  % serial date numbers are accepted for the dates
  if ~isa(t, 'datetime')
    t = datetime(t, 'ConvertFrom', 'datenum');
  end
end
%=============================================================================
function [a, b] = expandPair(a, b)
  if isscalar(a) && ~isscalar(b)
    a = a(ones(size(b)));
  elseif isscalar(b) && ~isscalar(a)
    b = b(ones(size(a)));
  elseif ~isequal(size(a), size(b))
    error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
  end
end
%=============================================================================
function [first, last, nat] = validPairs(first, last)
  % same-size operands; pairs with a NaT are computed on a valid date, then set to NaN
  [first, last] = expandPair(first, last);
  nat = isnat(first) | isnat(last);
  if any(nat(:))
    first(nat) = datetime(2000, 1, 1, 'TimeZone', first.TimeZone);
    last(nat) = datetime(2000, 1, 1, 'TimeZone', last.TimeZone);
  end
end
%=============================================================================
