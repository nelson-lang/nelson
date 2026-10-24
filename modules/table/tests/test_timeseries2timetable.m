%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ts = timeseries([1; 2; 3], [0; 1; 2], 'Name', 'speed');
TT = timeseries2timetable(ts);
asserts.isequal(istimetable(TT), true);
asserts.isequal(TT.Properties.VariableNames, {'speed'});
asserts.isequal(TT.speed, [1; 2; 3]);
asserts.isequal(seconds(TT.Properties.RowTimes), [0; 1; 2]);
R = evalc('TT');
asserts.istrue(contains(R, ['3', char(215), '1 timetable']));
asserts.istrue(contains(R, 'Time'));
asserts.istrue(contains(R, 'speed'));
asserts.istrue(contains(R, '0 sec'));
asserts.istrue(contains(R, '1 sec'));
asserts.istrue(contains(R, '2 sec'));
%=============================================================================
asserts.checkerror('timeseries2timetable(ts, ''VariableNames'', {''value''})', _('Input must be a timeseries object.'), 'Nelson:table:invalidType:inputATimeseriesObject');
asserts.checkerror('timeseries2timetable([])', _('Input must be a timeseries object.'));
%=============================================================================
a = timeseries([1; 2], [1; 2], 'Name', 'a');
b = timeseries([3; 4], [1; 2], 'Name', 'b');
c = timeseries([5; 6], [1; 2]);
TT = timeseries2timetable(a, b, c);
asserts.isequal(TT.Properties.VariableNames, {'a', 'b', 'Data'});
asserts.isequal(TT.b, [3; 4]);
asserts.isequal(seconds(TT.Properties.RowTimes), [1; 2]);
TT = timeseries2timetable(a, a);
asserts.isequal(TT.Properties.VariableNames, {'a', 'a_1'});
d = timeseries([5; 6], [1; 3], 'Name', 'd');
msg = _('All inputs must have the same time vector, TimeInfo Units, and TimeInfo StartDate. Separately convert each timeseries to a timetable, and then use the SYNCHRONIZE function to combine the timetables.');
asserts.checkerror('timeseries2timetable(a, d)', msg, 'Nelson:table:timeseriesTimeMismatch');
%=============================================================================
TT = timeseries2timetable([a a]);
asserts.isequal(TT.Properties.VariableNames, {'a', 'a_1'});
TT = timeseries2timetable([a; c; b]);
asserts.isequal(TT.Properties.VariableNames, {'a', 'Data', 'b'});
asserts.isequal(TT.Variables, [1, 5, 3; 2, 6, 4]);
msg = _('If multiple inputs are given to timeseries2timetable, they all must be scalar timeseries objects.');
asserts.checkerror('timeseries2timetable([a b], c)', msg, 'Nelson:table:timeseriesNonscalarInput');
asserts.checkerror('timeseries2timetable(c, [a b])', msg, 'Nelson:table:timeseriesNonscalarInput');
asserts.checkerror('timeseries2timetable(timeseries.empty())', _('timeseries array input must be non-empty.'), 'Nelson:table:emptyTimeseriesArray');
%=============================================================================
TT = timeseries2timetable(timeseries(int16([1; 2]), [1; 2]));
asserts.isequal(TT.Properties.VariableTypes, "int16");
asserts.istrue(isequal(+TT, TT));
asserts.istrue(isequal(-(-TT), TT));
asserts.istrue(isequal(TT(1:2, :), TT));
%=============================================================================
