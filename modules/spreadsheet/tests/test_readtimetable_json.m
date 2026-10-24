%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% writetimetable writes the row times first, keyed by the first dimension name
TT = timetable(datetime(2024, 1, 1, 10, 0, 0) + hours([0; 1; 2]), [12.5; 13; 11.75], [true; false; true], ...
  'VariableNames', {'Temperature', 'Valid'});
filename = [tempname(), '.json'];
writetimetable(TT, filename, 'PrettyPrint', false);
expected = ['[{"Time":"01-Jan-2024 10:00:00","Temperature":12.5,"Valid":true},', ...
  '{"Time":"01-Jan-2024 11:00:00","Temperature":13,"Valid":false},', ...
  '{"Time":"01-Jan-2024 12:00:00","Temperature":11.75,"Valid":true}]'];
asserts.isequal(fileread(filename), expected);
%=============================================================================
% readtimetable uses the first datetime variable as row times
R = readtimetable(filename);
asserts.istrue(istimetable(R));
asserts.isequal(R.Properties.VariableNames, {'Temperature', 'Valid'});
asserts.isequal(R.Properties.DimensionNames{1}, 'Time');
asserts.isequal(R.Temperature, [12.5; 13; 11.75]);
asserts.isequal(R.Valid, [true; false; true]);
asserts.isequal(round(datenum(R.Properties.RowTimes) * 86400), round(datenum(TT.Properties.RowTimes) * 86400));
%=============================================================================
% duration row times
TT = timetable(duration(0, 0, [1; 2; 3]), [1; 2; 3], 'VariableNames', {'X'});
writetimetable(TT, filename);
R = readtimetable(filename);
asserts.isequal(R.Properties.RowTimes, duration(0, 0, [1; 2; 3]));
asserts.isequal(R.X, [1; 2; 3]);
%=============================================================================
% JSON and timetable name-value arguments together
filename = [tempname(), '.json'];
fid = fopen(filename, 'w', 'n', 'UTF-8');
fprintf(fid, '%s', '{"data": [{"v": 1, "w": 5}, {"v": 2, "w": 6}], /* note */ "n": 2}');
fclose(fid);
R = readtimetable(filename, 'SampleRate', 10, 'TableSelector', '/data', 'VariableSelectors', "/v");
asserts.isequal(R.Properties.VariableNames, {'v'});
asserts.isequal(R.v, [1; 2]);
asserts.isequal(R.Properties.RowTimes, seconds([0; 0.1]));
R = readtimetable(filename, 'RowTimes', seconds([5; 6]));
asserts.isequal(R.Properties.VariableNames, {'v', 'w'});
asserts.isequal(R.Properties.RowTimes, seconds([5; 6]));
asserts.checkerror('readtimetable(filename, ''ParsingMode'', ''strict'', ''RowTimes'', seconds([5; 6]))', _('Comments are not allowed in the JSON file (AllowComments is false).'));
%=============================================================================
opts = detectImportOptions(filename);
R = readtimetable(filename, opts, 'TimeStep', seconds(2));
asserts.isequal(R.Properties.RowTimes, seconds([0; 2]));
%=============================================================================
