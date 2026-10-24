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
  narginchk(2, 3);
  if nargin < 3
    components = 'months';
  end
  serial1 = datenum(t1);
  serial2 = datenum(t2);
  [serial1, serial2] = expandPair(serial1, serial2);
  v1 = datevec(serial1(:));
  v2 = datevec(serial2(:));
  components = lower(char(components));
  switch components
    case {'years', 'year', 'y'}
      monthsValue = wholeMonths(v1, v2);
      monthsValue = fix(monthsValue ./ 12) .* 12;
      daysValue = zeros(size(monthsValue));
    case {'quarters', 'quarter', 'q'}
      monthsValue = wholeMonths(v1, v2);
      monthsValue = fix(monthsValue ./ 3) .* 3;
      daysValue = zeros(size(monthsValue));
    case {'months', 'month', 'm'}
      monthsValue = wholeMonths(v1, v2);
      anchor = addMonths(serial1(:), monthsValue);
      daysValue = floor(serial2(:)) - floor(anchor);
    case {'days', 'day', 'd'}
      monthsValue = zeros(size(serial1(:)));
      daysValue = floor(serial2(:)) - floor(serial1(:));
    otherwise
      monthsValue = wholeMonths(v1, v2);
      anchor = addMonths(serial1(:), monthsValue);
      daysValue = floor(serial2(:)) - floor(anchor);
  end
  secondsValue = (serial2(:) - floor(serial2(:)) - (serial1(:) - floor(serial1(:)))) .* 86400;
  c = calendarDuration.fromParts(reshape(monthsValue, size(serial1)), ...
    reshape(daysValue, size(serial1)), reshape(secondsValue, size(serial1)), 'ymd');
end
%=============================================================================
function m = wholeMonths(v1, v2)
  m = (v2(:, 1) - v1(:, 1)) .* 12 + (v2(:, 2) - v1(:, 2));
  % Clamp the start day to the last day of the target month: when the start day
  % is past the target month's length (e.g. Jan 31 -> Feb 28, or Mar 31 ->
  % Apr 30), reaching that clamped month-end still completes a whole month, so
  % v2.day == eomday must not be treated as "earlier in the month".
  clampedStartDay = min(v1(:, 3), eomday(v2(:, 1), v2(:, 2)));
  adjustPositive = m >= 0 & v2(:, 3) < clampedStartDay;
  adjustNegative = m < 0 & v2(:, 3) > v1(:, 3);
  m(adjustPositive) = m(adjustPositive) - 1;
  m(adjustNegative) = m(adjustNegative) + 1;
end
%=============================================================================
function out = addMonths(serial, monthsValue)
  v = datevec(serial);
  v(:, 2) = v(:, 2) + monthsValue(:);
  y = v(:, 1) + floor((v(:, 2) - 1) ./ 12);
  m = mod(v(:, 2) - 1, 12) + 1;
  d = min(v(:, 3), eomday(y, m));
  out = datenum(y, m, d, v(:, 4), v(:, 5), v(:, 6));
end
%=============================================================================
function [a, b] = expandPair(a, b)
  if isscalar(a) && ~isscalar(b)
    a = repmat(a, size(b));
  elseif isscalar(b) && ~isscalar(a)
    b = repmat(b, size(a));
  elseif ~isequal(size(a), size(b))
    error('Nelson:time:inputSizesMatchScalar', 'Input sizes must match or be scalar.');
  end
end
%=============================================================================
