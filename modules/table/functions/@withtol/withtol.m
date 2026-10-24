%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef withtol
  % Row subscript of a timetable: the rows whose times are within a
  % tolerance of the subscript times.
  properties (Hidden)
    Times = NaT
    Tolerance = NaN
  end

  methods
    function obj = withtol(times, tolerance)
      if nargin == 0
        return
      end
      narginchk(2, 2);
      obj.Times = checkedTimes(times);
      obj.Tolerance = checkedTolerance(tolerance);
      checkIntervals(obj.Times, obj.Tolerance);
    end

    function display(obj, varargin)
      if nargin == 1
        name = inputname(1);
      else
        name = varargin{1};
      end
      if ~isempty(name)
        looseBlankLine();
        fprintf('%s = \n', name);
        looseBlankLine();
      end
      disp(obj);
    end

    function disp(obj)
      fprintf('\ttimetable withtol subscript:\n');
      looseBlankLine();
      fprintf('\t\tSelect timetable rows matching the following times:\n');
      looseBlankLine();
      for k = 1:numel(obj.Times)
        fprintf('\t\t%s\n', char(obj.Times(k)));
      end
      if isa(obj.Tolerance, 'duration')
        fprintf('\t\twith tolerance of +/- %s\n', char(obj.Tolerance));
      else
        fprintf('\t\twith tolerance of +/- %s\n', num2str(obj.Tolerance));
      end
      looseBlankLine();
    end
  end

  methods (Static, Hidden)
    function rows = rows(obj, rowTimes)
      % Row numbers of the row times within the tolerance of each subscript
      % time, subscript time by subscript time. Distances are compared to
      % the nanosecond, a zero tolerance selects the rows at the times.
      if ~strcmp(class(obj.Times), class(rowTimes))
        error('Nelson:withtol:MismatchRowTimesType', _('A timetable with %s row times cannot be subscripted using %s values.'), ...
          class(rowTimes), class(obj.Times));
      end
      tolerance = round(milliseconds(obj.Tolerance) * 1e6);
      matched = cell(1, numel(obj.Times));
      for k = 1:numel(matched)
        found = find(round(abs(milliseconds(rowTimes - obj.Times(k))) * 1e6) <= tolerance);
        matched{k} = found(:)';
      end
      rows = [zeros(1, 0), matched{:}];
    end
  end
end
%=============================================================================
function times = checkedTimes(times)
  if ischar(times) || isstring(times) || iscellstr(times)
    times = datetime(times);
  end
  if ~(isa(times, 'datetime') || isa(times, 'duration'))
    error('Nelson:timetable:InvalidTimes', _('Specify times for subscripting as datetime or duration.'));
  end
  times = times(:);
end
%=============================================================================
function tolerance = checkedTolerance(tolerance)
  if ~isa(tolerance, 'duration') || ~isscalar(tolerance) || tolerance < seconds(0)
    error('Nelson:withtol:InvalidTolerance', _('Tolerance value must be a nonnegative scalar duration.'));
  end
end
%=============================================================================
function checkIntervals(times, tolerance)
  % A tolerance of half the smallest interval between distinct subscript
  % times could select a row twice.
  if numel(times) < 2 || isnan(milliseconds(tolerance))
    return
  end
  gaps = abs(diff(sort(milliseconds(times - times(1)))));
  gaps = gaps(gaps > 0);
  if isempty(gaps) || milliseconds(tolerance) < min(gaps) / 2
    return
  end
  limit = milliseconds(min(gaps) / 2);
  limit.Format = tolerance.Format;
  error('Nelson:withtol:LargeTolerance', _('Tolerance exceeds half the smallest interval between subscript times and might result in selecting duplicate rows. Tolerance must be less than %s.'), ...
    char(string(limit)));
end
%=============================================================================
function looseBlankLine()
  % Separator line of the loose line spacing, absent from the compact one.
  currentFormat = format();
  if strcmp(currentFormat.LineSpacing, 'loose')
    fprintf('\n');
  end
end
%=============================================================================
