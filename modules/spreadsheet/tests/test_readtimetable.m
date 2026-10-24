%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Delimited text timetable import is not stable in the portable runtime yet.');
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtimetable_1.csv'];
TT = readtimetable(csv_filename);
asserts.istrue(istimetable(TT));
asserts.isequal(width(TT), 2);
asserts.isequal(TT.Temperature, [12.5; 13.0; 11.75]);
asserts.isequal(TT.Pressure, [1012; 1014; 1011]);
asserts.isequal(datenum(TT.Properties.RowTimes), datenum(datetime(2024, 1, 1) + days(0:2)'));
%=============================================================================
TT = readtimetable(csv_filename, 'RowTimes', 'Time');
asserts.isequal(TT.Properties.VariableNames, {'Temperature', 'Pressure'});
asserts.isequal(datenum(TT.Time), datenum(datetime(2024, 1, 1) + days(0:2)'));
%=============================================================================
rowTimes = seconds([10; 20; 30]);
TT = readtimetable(csv_filename, 'RowTimes', rowTimes);
asserts.isequal(TT.Properties.RowTimes, rowTimes);
asserts.isequal(datenum(TT{:, 'Time'}), datenum(datetime(2024, 1, 1) + days(0:2)'));
asserts.isequal(TT.Properties.DimensionNames, {'Time_1', 'Variables'});
asserts.isequal(datenum(TT.Time), datenum(datetime(2024, 1, 1) + days(0:2)'));
asserts.isequal(TT.Time_1, rowTimes);
%=============================================================================
TT = readtimetable(csv_filename, 'StartTime', seconds(5), 'TimeStep', seconds(2));
asserts.isequal(TT.Properties.RowTimes, seconds([5; 7; 9]));
%=============================================================================
TT = readtimetable(csv_filename, 'StartTime', seconds(0), 'SampleRate', 0.5);
asserts.isequal(TT.Properties.RowTimes, seconds([0; 2; 4]));
%=============================================================================
TT = timetable(datetime(2024, 1, 1) + days(0:2)', [1; 2; 3], 'VariableNames', {'A'});
filename = [tempname(), '.csv'];
writetimetable(TT, filename);
R = readtimetable(filename);
asserts.isequal(R.A, [1; 2; 3]);
asserts.isequal(datenum(R.Properties.RowTimes), datenum(TT.Properties.RowTimes));
%=============================================================================
filename = [tempname(), '.csv'];
fid = fopen(filename, 'w');
fprintf(fid, 'When;Value;Name\n');
fprintf(fid, '2024-02-01;10;A\n');
fprintf(fid, '2024-02-02;11;B\n');
fclose(fid);
opts = detectImportOptions(filename);
T = readtable(filename, opts);
TT = readtimetable(filename, opts, 'RowTimes', 'When');
asserts.isequal(height(T), 2);
asserts.isequal(width(T), 3);
asserts.isequal(T.Value, TT.Value);
asserts.isequal(T.Name, TT.Name);
asserts.isequal(datenum(TT.Properties.RowTimes), datenum(datetime(2024, 2, 1) + days(0:1)'));
%=============================================================================
filename = [tempname(), '.csv'];
fid = fopen(filename, 'w');
fprintf(fid, 'Elapsed,Value\n');
fprintf(fid, '00:00:05,1\n');
fprintf(fid, '00:00:07.500,2\n');
fprintf(fid, '00:00:10,3\n');
fclose(fid);
TT = readtimetable(filename, 'RowTimes', 'Elapsed');
asserts.isequal(TT.Properties.RowTimes, seconds([5; 7.5; 10]));
%=============================================================================
TT = timetable(seconds([1; 2; 3]), datetime(2024, 3, 1) + days(0:2)', ...
  seconds([10; 20; 30]), 'VariableNames', {'Stamp', 'Elapsed'});
filename = [tempname(), '.csv'];
writetimetable(TT, filename);
R = readtimetable(filename);
asserts.isequal(R.Properties.RowTimes, TT.Properties.RowTimes);
asserts.isequal(datenum(R.Stamp), datenum(TT.Stamp));
asserts.isequal(R.Elapsed, TT.Elapsed);
%=============================================================================
didError = false;
try
  readtimetable([tempname(), '.csv']);
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
didError = false;
try
  readtimetable(csv_filename, 'TimeVariable', 'MissingTime');
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
didError = false;
try
  readtimetable(csv_filename, 'RowTimes', seconds([1; 2]));
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
filename = [tempname(), '.csv'];
fid = fopen(filename, 'w');
fprintf(fid, 'Time,Value\n');
fprintf(fid, 'not-a-time,1\n');
fprintf(fid, 'still-not-a-time,2\n');
fclose(fid);
didError = false;
try
  readtimetable(filename, 'RowTimes', 'Time');
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
