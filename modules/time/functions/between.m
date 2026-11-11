%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = between(t1, t2, components)
  % Calendar difference t2 - t1 split greedily into the requested components,
  % largest first: each takes the most whole units that do not go past t2.
  narginchk(2, 3);
  if nargin < 3
    units = parseComponents({'years', 'months', 'days', 'time'});
  else
    units = parseComponents(components);
  end
  [first, last] = expandPair(asDatetime(t1), asDatetime(t2));
  nat = isnat(first) | isnat(last);
  if any(nat(:))
    % pairs with a NaT are computed on a valid date, then set to NaN
    first(nat) = datetime(2000, 1, 1, 'TimeZone', first.TimeZone);
    last(nat) = datetime(2000, 1, 1, 'TimeZone', last.TimeZone);
  end
  forward = last >= first;
  monthsValue = zeros(size(first));
  for u = [12 3 1; units.years units.quarters units.months]
    if u(2)
      monthsValue = monthsValue + u(1) .* wholeMonthUnits(first, last, monthsValue, u(1), forward);
    end
  end
  daysValue = zeros(size(first));
  anchor = first + calmonths(monthsValue);
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
function k = wholeMonthUnits(t1, t2, monthsValue, unitMonths, forward)
  % most whole units of unitMonths months after t1 + monthsValue not past t2
  v1 = datevec(t1);
  v2 = datevec(t2);
  remaining = (v2(:, 1) - v1(:, 1)) .* 12 + (v2(:, 2) - v1(:, 2)) - monthsValue(:);
  k = reshape(fix(remaining ./ unitMonths), size(t1));
  % the calendar month count may be one too many when the day or time is not reached
  reached = t1 + calmonths(monthsValue + k .* unitMonths);
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
