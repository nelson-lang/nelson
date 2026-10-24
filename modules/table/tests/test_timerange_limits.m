%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
msg = 'The endpoints cannot be unambiguously interpreted as datetimes or durations. Convert text or numeric inputs to datetime or duration.';
id = 'Nelson:timerange:AmbiguousTimes';
assert_checkerror('timerange(''-inf'', ''inf'')', msg, id);
assert_checkerror('timerange(''inf'', ''-inf'')', msg, id);
assert_checkerror('timerange("-inf", "inf")', msg, id);
assert_checkerror('timerange(''-Inf'', ''+Inf'', ''closed'')', msg, id);
assert_checkerror('timerange(-inf, inf)', msg, id);
assert_checkerror('timerange(single(-inf), single(inf))', msg, id);
assert_checkerror('timerange(-inf, ''inf'')', msg, id);
assert_checkerror('timerange(''-inf'', ''inf'', ''days'')', msg, id);
%=============================================================================
msg = 'Time range endpoints must be datetime or duration scalars, eventfilter, or text representing datetime or duration scalars.';
id = 'Nelson:timerange:InvalidTimes';
assert_checkerror('timerange(1, 2)', msg, id);
assert_checkerror('timerange(1, seconds(2))', msg, id);
assert_checkerror('timerange(datetime(2020, 1, 1), 2)', msg, id);
assert_checkerror('timerange(''-inf'', 2)', msg, id);
assert_checkerror('timerange(NaN, 1)', msg, id);
assert_checkerror('timerange(true, false)', msg, id);
assert_checkerror('timerange(int8(1), int8(2))', msg, id);
assert_checkerror('timerange({1}, {2})', msg, id);
assert_checkerror('timerange(calmonths(1), calmonths(2))', msg, id);
assert_checkerror('timerange(2020, ''years'')', msg, id);
assert_checkerror('timerange(datetime(2020, 1, 1), [''days''; ''days''])', msg, id);
assert_checkerror('timerange(datetime(2020, 1, 1), ["days", "days"])', msg, id);
%=============================================================================
msg = 'Time range endpoints must both be datetimes, both be durations, or both be eventfilters.';
id = 'Nelson:timerange:InputTypesMismatch';
assert_checkerror('timerange(datetime(2020, 1, 1), seconds(2))', msg, id);
assert_checkerror('timerange(seconds(1), ''2020-01-01'')', msg, id);
assert_checkerror('timerange(''2020-01-01'', ''00:00:05'')', msg, id);
assert_checkerror('timerange(datetime(2020, 1, 1), NaN)', msg, id);
assert_checkerror('timerange(datetime(2020, 1, 1), seconds(3), ''months'')', msg, id);
%=============================================================================
assert_checkerror('timerange([seconds(1), seconds(2)], seconds(3))', 'Arguments must be scalars.', 'Nelson:timerange:NonScalarInput');
assert_checkerror('timerange(datetime([2020, 2021], 1, 1), ''days'')', 'Arguments must be scalars.', 'Nelson:timerange:NonScalarInput');
%=============================================================================
msg = 'Specify third argument either as interval type (''openright'', ''openleft'', ''open'', ''closed'') or as unit of time (''years'', ''quarters'', ''months'', ''weeks'', ''days'', ''hours'', ''minutes'', ''seconds'').';
id = 'Nelson:timerange:InvalidIntervalType';
assert_checkerror('timerange(seconds(1), seconds(2), ''bogus'')', msg, id);
assert_checkerror('timerange(seconds(1), seconds(2), 5)', msg, id);
assert_checkerror('timerange(seconds(1), seconds(2), ["open", "open"])', msg, id);
assert_checkerror('timerange(seconds(1), seconds(2), {''open''})', msg, id);
assert_checkerror('timerange(1, 2, ''bogus'')', msg, id);
assert_checkerror('timerange(''-inf'', ''inf'', ''bogus'')', msg, id);
%=============================================================================
msg = 'Durations and event filters are not supported when specifying a unit of time for a time range. Use datetimes when specifying unit of time.';
id = 'Nelson:timerange:UnitOfTimeTypesMismatch';
assert_checkerror('timerange(seconds(1), ''days'')', msg, id);
assert_checkerror('timerange(seconds(1), seconds(3), ''days'')', msg, id);
assert_checkerror('timerange(seconds(1), ''-inf'', ''days'')', msg, id);
assert_checkerror('timerange(''00:00:05'', ''days'')', msg, id);
assert_checkerror('timerange(NaN, ''days'')', msg, id);
%=============================================================================
msg = 'Unable to combine a datetime that has a time zone with an unzoned datetime.';
id = 'Nelson:timerange:TimeZonesMismatch';
u = datetime(2020, 1, 1, 'TimeZone', 'UTC');
n = datetime(2020, 1, 2);
assert_checkerror('timerange(u, n)', msg, id);
assert_checkerror('timerange(n, u)', msg, id);
assert_checkerror('timerange(u, n, ''closed'')', msg, id);
assert_checkerror('timerange(u, n, ''days'')', msg, id);
assert_checkerror('timerange(NaT, u)', msg, id);
assert_checkerror('timerange(u, NaN)', 'Time range endpoints must both be datetimes, both be durations, or both be eventfilters.', 'Nelson:timerange:InputTypesMismatch');
assert_checkerror('timerange(u, n, ''bogus'')', 'Specify third argument either as interval type (''openright'', ''openleft'', ''open'', ''closed'') or as unit of time (''years'', ''quarters'', ''months'', ''weeks'', ''days'', ''hours'', ''minutes'', ''seconds'').', 'Nelson:timerange:InvalidIntervalType');
assert_checkerror('timerange([u, u], n)', 'Arguments must be scalars.', 'Nelson:timerange:NonScalarInput');
% text, infinite limits and two zoned datetimes combine with a zoned datetime
p = datetime(2020, 1, 2, 'TimeZone', 'Europe/Paris');
assert_isequal(class(timerange(u, p)), 'timerange');
assert_isequal(class(timerange(u, p, 'days')), 'timerange');
assert_isequal(class(timerange(u, '2020-01-02')), 'timerange');
assert_isequal(class(timerange('-inf', u)), 'timerange');
assert_isequal(class(timerange(-inf, u)), 'timerange');
assert_isequal(class(timerange(u, 'inf', 'days')), 'timerange');
assert_isequal(class(timerange(datetime(2020, 1, 1, 'TimeZone', ''), n)), 'timerange');
%=============================================================================
% datetime limits select row times by instant
t = datetime(2020, 1, 1, 0:5, 0, 0, 'TimeZone', 'UTC')';
TT = timetable(t, (1:6)', 'VariableNames', {'A'});
R = TT(timerange(datetime(2020, 1, 1, 1, 0, 0, 'TimeZone', 'Europe/Paris'), datetime(2020, 1, 1, 4, 0, 0, 'TimeZone', 'Europe/Paris')), :);
assert_isequal(R.A, [1; 2; 3]);
R = TT(timerange(datetime(2020, 1, 1, 1, 0, 0, 'TimeZone', 'Europe/Paris'), '2020-01-01 04:00:00'), :);
assert_isequal(R.A, [1; 2; 3]);
% text limits alone are read in the time zone of the row times
R = TT(timerange('2020-01-01 01:00:00', '2020-01-01 04:00:00'), :);
assert_isequal(R.A, [2; 3; 4]);
msg = 'Cannot combine or compare a datetime array with a time zone with one without a time zone.';
assert_checkerror('TT(timerange(datetime(2020, 1, 1, 1, 0, 0), datetime(2020, 1, 1, 4, 0, 0)), :)', msg, 'Nelson:datetime:IncompatibleTZ');
T2 = timetable(datetime(2020, 1, 1, 0:5, 0, 0)', (1:6)', 'VariableNames', {'A'});
assert_checkerror('T2(timerange(datetime(2020, 1, 1, 1, 0, 0, ''TimeZone'', ''UTC''), ''inf''), :)', msg, 'Nelson:datetime:IncompatibleTZ');
% a day period across a DST change ends at the next midnight
T3 = timetable(datetime(2020, 3, 29, 0:2:24, 0, 0, 'TimeZone', 'Europe/Paris')', (1:13)', 'VariableNames', {'A'});
R = T3(timerange(datetime(2020, 3, 29, 'TimeZone', 'Europe/Paris'), 'days'), :);
assert_isequal(R.A, (1:12)');
%=============================================================================
id = 'Nelson:datetime:InvalidTextInput';
assert_checkerror('timerange(''abc'', ''inf'')', 'Unable to recognize date/time format of ''abc''. Convert text to datetime or duration.', id);
assert_checkerror('timerange("abc", seconds(1))', 'Unable to recognize date/time format of ''abc''. Convert text to datetime or duration.', id);
assert_checkerror('timerange(datetime(2020, 1, 1), ''closed'')', 'Unable to recognize date/time format of ''closed''. Convert text to datetime or duration.', id);
%=============================================================================
% accepted limits: one infinite endpoint next to a typed one, duration text,
% case-insensitive interval types, NaN endpoints selecting no rows.
TT = timetable(seconds((1:5)'), (1:5)', 'VariableNames', {'A'});
assert_isequal(TT(timerange(-inf, seconds(3)), :).A, [1; 2]);
assert_isequal(TT(timerange('  -INF  ', seconds(3), 'OpenLeft'), :).A, [1; 2; 3]);
assert_isequal(TT(timerange('00:00:02', seconds(4), "closed"), :).A, [2; 3; 4]);
assert_isequal(TT(timerange('-inf', '0:00:03'), :).A, [1; 2]);
assert_isequal(TT(timerange(NaN, seconds(5)), :).A, zeros(0, 1));
assert_isequal(TT(timerange('-inf', NaN), :).A, zeros(0, 1));
assert_isequal(size(TT(timerange(NaN, NaN), :)), [0, 1]);
%=============================================================================
t = datetime(2020, 1, 1:3)';
TT = timetable(t, (1:3)', 'VariableNames', {'A'});
assert_isequal(TT(timerange(-inf, datetime(2020, 1, 2)), :).A, 1);
assert_isequal(TT(timerange(datetime(2020, 1, 2), inf), :).A, [2; 3]);
assert_isequal(TT(timerange(datetime(2020, 1, 1), '2020-01-02'), :).A, 1);
assert_isequal(TT(timerange('-inf', 'days'), :).A, zeros(0, 1));
assert_isequal(TT(timerange('inf', datetime(2020, 1, 2), 'days'), :).A, zeros(0, 1));
assert_isequal(TT(timerange(datetime(2020, 1, 2), 'inf', 'days'), :).A, [2; 3]);
%=============================================================================
header = sprintf('\ttimetable timerange subscript:\n\n\t\tSelect ');
txt = evalc('disp(timerange(NaN, seconds(1)))');
assert_isequal(txt, [header, sprintf('no rows because an endpoint is a NaN/NaT.\n\n')]);
txt = evalc('disp(timerange(NaN, NaN))');
assert_isequal(txt, [header, sprintf('no rows because an endpoint is a NaN/NaT.\n\n')]);
txt = evalc('disp(timerange(seconds(2), seconds(1)))');
assert_isequal(txt, [header, sprintf('no rows because the left endpoint is at or after the right endpoint.\n\n')]);
txt = evalc('disp(timerange(seconds(1), seconds(1)))');
assert_isequal(txt, [header, sprintf('no rows because the left endpoint is at or after the right endpoint.\n\n')]);
txt = evalc('disp(timerange(''-inf'', ''days''))');
assert_isequal(txt, [header, sprintf('no rows because the left endpoint is at or after the right endpoint.\n\n')]);
txt = evalc('disp(timerange(seconds(1), seconds(1), ''closed''))');
assert_isequal(txt, [header, sprintf('timetable rows with times in the closed interval:\n'), ...
  sprintf('\t\t  Starting at, including: 1 sec\n'), ...
  sprintf('\t\t  Ending at, including:   1 sec\n\n')]);
txt = evalc('disp(timerange(-inf, datetime(2020, 1, 1)))');
assert_isequal(txt, [header, sprintf('timetable rows with times in the half-open interval:\n'), ...
  sprintf('\t\t  Starting at, including:   -Inf\n'), ...
  sprintf('\t\t  Ending at, but excluding: 01-Jan-2020 00:00:00\n\n')]);
txt = evalc('disp(timerange(datetime(2020, 1, 1), ''inf'', ''months''))');
assert_isequal(txt, [header, sprintf('timetable rows with times in the half-open interval:\n'), ...
  sprintf('\t\t  Starting at, including:   01-Jan-2020 00:00:00\n'), ...
  sprintf('\t\t  Ending at, but excluding: Inf\n\n')]);
txt = evalc('disp(timerange(''-inf'', ''00:00:05''))');
assert_isequal(txt, [header, sprintf('timetable rows with times in the half-open interval:\n'), ...
  sprintf('\t\t  Starting at, including:   -Inf\n'), ...
  sprintf('\t\t  Ending at, but excluding: 00:00:05\n\n')]);
%=============================================================================
