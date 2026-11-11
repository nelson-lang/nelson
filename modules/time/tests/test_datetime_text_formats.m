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
% texts without InputFormat: the layout of the first date text is detected
cases = {'2024-10-27 02:30', [2024 10 27 2 30 0]; ...
  '2024-10-27 02:30:15', [2024 10 27 2 30 15]; ...
  '2024-10-27 02:30:15.250', [2024 10 27 2 30 15.25]; ...
  '2024-10-27', [2024 10 27 0 0 0]; ...
  '2024-10-27T02:30:15', [2024 10 27 2 30 15]; ...
  '27-Oct-2024 02:30', [2024 10 27 2 30 0]; ...
  '10/27/2024 02:30:15', [2024 10 27 2 30 15]; ...
  '2024-10-27 2:30', [2024 10 27 2 30 0]; ...
  '2024-1-5 3:04', [2024 1 5 3 4 0]; ...
  '2024-10-27 02:30 PM', [2024 10 27 14 30 0]; ...
  '27-Oct-2024 02:30 PM', [2024 10 27 14 30 0]; ...
  'Oct 27, 2024', [2024 10 27 0 0 0]; ...
  '27/10/2024 02:30', [2024 10 27 2 30 0]; ...
  '  2024-10-27 02:30  ', [2024 10 27 2 30 0]};
for k = 1:size(cases, 1)
  t = datetime(cases{k, 1});
  asserts.isequal([t.Year, t.Month, t.Day, t.Hour, t.Minute], cases{k, 2}(1:5));
  asserts.isapprox(t.Second, cases{k, 2}(6), 1e-9);
end
%=============================================================================
t = datetime('2024-10-27 02:30');
asserts.isequal(t.Format, 'dd-MMM-uuuu HH:mm:ss');
asserts.isequal(char(t), '27-Oct-2024 02:30:00');
asserts.isapprox(second(datetime('2024-10-27T02:30:15.123456')), 15.123456, 1e-9);
%=============================================================================
% texts after the first one that do not follow its layout are NaT
t = datetime({'2024-10-27 02:30', '27-Oct-2024', '2024-13-01 02:30'});
asserts.isequal(isnat(t), [false true true]);
%=============================================================================
% an unreadable first text is an error
msg = 'Unable to recognize the date and time format of ''%s''. Use the ''InputFormat'' option to give it.';
id = 'Nelson:datetime:UnrecognizedDateString';
texts = {'2024-10-27 24:00', '2024-13-01 02:30', '2024-10-27T02:30', '20241027', 'abc', '2024/10/27', ...
  '27 Oct 2024 02:30', '2024-02-30'};
for k = 1:numel(texts)
  asserts.checkerror(sprintf('datetime(''%s'')', texts{k}), sprintf(msg, texts{k}), id);
end
asserts.checkerror('datetime("abc")', sprintf(msg, 'abc'), id);
asserts.checkerror('datetime(''xyz'', ''TimeZone'', ''UTC'')', sprintf(msg, 'xyz'), id);
msg = 'Unable to recognize the date and time format of the text. Use the ''InputFormat'' option to give it.';
id = 'Nelson:datetime:UnrecognizedDateStrings';
asserts.checkerror('datetime({''abc'', ''2024-10-27''})', msg, id);
asserts.checkerror('datetime({''NaT'', ''abc''})', msg, id);
asserts.checkerror('datetime({''now'', ''abc''})', msg, id);
asserts.checkerror('datetime(["abc"; "2024-10-27"])', msg, id);
asserts.checkerror('datetime([''abc''; ''def''])', msg, id);
%=============================================================================
% with InputFormat or Format, the error names the format
id = 'Nelson:datetime:ParseErr';
asserts.checkerror('datetime(''abc'', ''InputFormat'', ''yyyy-MM-dd'')', ...
  'Unable to read ''abc'' as a datetime with the format ''yyyy-MM-dd''.', id);
asserts.checkerror('datetime(''2024-13-01'', ''InputFormat'', ''yyyy-MM-dd'')', ...
  'Unable to read ''2024-13-01'' as a datetime with the format ''yyyy-MM-dd''.', id);
asserts.checkerror('datetime(''abc'', ''Format'', ''yyyy'')', ...
  'Unable to read ''abc'' as a datetime with the format ''yyyy''.', id);
asserts.checkerror('datetime({''abc'', ''def''}, ''InputFormat'', ''yyyy-MM-dd'')', ...
  'Unable to read the text as datetime values with the format ''yyyy-MM-dd''.', 'Nelson:datetime:ParseErrs');
t = datetime({'abc', '2024-10-27'}, 'InputFormat', 'yyyy-MM-dd');
asserts.isequal(isnat(t), [true false]);
t = datetime({'2024-13-01', '2024-10-27'}, 'InputFormat', 'yyyy-MM-dd');
asserts.isequal(isnat(t), [true false]);
%=============================================================================
% no date text at all: NaT, no error
asserts.istrue(isnat(datetime('')));
asserts.istrue(isnat(datetime('NaT')));
asserts.istrue(isnat(datetime('  ')));
asserts.istrue(isnat(datetime(string(missing))));
asserts.isequal(isnat(datetime({'', ''})), [true true]);
asserts.isequal(isnat(datetime({'NaT', '2024-10-27'})), [true false]);
asserts.isequal(isnat(datetime([""; "2024-10-27"])), [true; false]);
asserts.isequal(isnat(datetime(["2024-10-27"; missing])), [false; true]);
asserts.isequal(size(datetime(char(zeros(0, 5)))), [0 1]);
asserts.isequal(isnat(datetime(['2024-10-27'; 'abcdefghij'])), [false; true]);
asserts.isequal(isnat(datetime({'NaT'}, 'InputFormat', 'yyyy-MM-dd')), true);
%=============================================================================
% the given Format is tried first
t = datetime('10/11/2024', 'Format', 'dd/MM/yyyy');
asserts.isequal([t.Day, t.Month], [10 11]);
t = datetime('2024-10-27', 'Format', 'dd/MM/yyyy');
asserts.isequal([t.Day, t.Month], [27 10]);
%=============================================================================
t = datetime({'2024-10-27 02:30', '2024-10-28 03:45'});
asserts.isequal(t.Hour, [2 3]);
t = datetime(["2024-10-27 02:30"; "NaT"; "2024-10-28 03:45"]);
asserts.isequal(isnat(t), [false; true; false]);
asserts.isequal(t(3).Minute, 45);
%=============================================================================
t = datetime('2024-10-27 02:30', 'TimeZone', 'Europe/Paris');
asserts.isequal(char(t), '27-Oct-2024 02:30:00');
asserts.isequal(hours(tzoffset(t)), 1);
%=============================================================================
% a guessed month-first layout warns when no day is above 12
id = 'Nelson:datetime:AmbiguousDateString';
msg = ['The text was read with the format ''MM/dd/yyyy'', but ''dd/MM/yyyy'' could also apply. ', ...
  'Use the ''InputFormat'' option to choose.'];
ambiguous = {{'10/11/2024'}, {'1/2/2024'}, {'12/12/2024'}, {{'NaT', '10/11/2024'}}, {["10/11/2024"; "10/12/2024"]}, ...
  {'10/11/2024', 'TimeZone', 'UTC'}, {'10/11/2024', 'Format', 'yyyy-MM-dd'}};
for k = 1:numel(ambiguous)
  lastwarn('', '');
  t = datetime(ambiguous{k}{:});
  [warnMessage, warnId] = lastwarn();
  asserts.isequal(warnId, id);
  asserts.isequal(warnMessage, msg);
end
asserts.isequal(datetime('10/11/2024').Month, 10);
lastwarn('', '');
datetime('10/11/2024 02:30');
[warnMessage, warnId] = lastwarn();
asserts.isequal(warnId, id);
asserts.istrue(contains(warnMessage, '''MM/dd/yyyy HH:mm'''));
asserts.istrue(contains(warnMessage, '''dd/MM/yyyy HH:mm'''));
unambiguous = {{'10/27/2024'}, {'27/10/2024'}, {{'10/11/2024', '10/27/2024'}}, {{'10/27/2024', '10/11/2024'}}, ...
  {'10/11/2024', 'Format', 'MM/dd/yyyy'}, {'10/11/2024', 'InputFormat', 'MM/dd/yyyy'}, {'2024-10-11'}, {'11-Oct-2024'}};
for k = 1:numel(unambiguous)
  lastwarn('', '');
  t = datetime(unambiguous{k}{:});
  [~, warnId] = lastwarn();
  asserts.isequal(warnId, '');
end
%=============================================================================
% 'yyyy', 'uuuu', 'uu' and guessed layouts keep the year as written
t = datetime('1/2/3');
asserts.isequal([t.Year, t.Month, t.Day], [3 1 2]);
asserts.isequal(datetime('1/2/24').Year, 24);
asserts.isequal(datetime('24-01-02').Year, 24);
asserts.isequal(datetime('02-Jan-24').Year, 24);
asserts.isequal(datetime('24', 'InputFormat', 'yyyy').Year, 24);
asserts.isequal(datetime('3', 'InputFormat', 'yyyy').Year, 3);
asserts.isequal(datetime('24', 'InputFormat', 'uuuu').Year, 24);
asserts.isequal(datetime('24', 'InputFormat', 'uu').Year, 24);
asserts.isequal(datetime('24', 'InputFormat', 'yyy').Year, 24);
asserts.isequal(datetime('12345', 'InputFormat', 'yyyy').Year, 12345);
%=============================================================================
% 'y' and 'yy' read two digits in the 100 years from the pivot year
pivot = year(datetime('now')) - 50;
expected = @(y2) pivot - mod(pivot, 100) + y2 + 100 * (pivot - mod(pivot, 100) + y2 < pivot);
asserts.isequal(datetime('24', 'InputFormat', 'yy').Year, expected(24));
asserts.isequal(datetime('75', 'InputFormat', 'yy').Year, expected(75));
asserts.isequal(datetime('76', 'InputFormat', 'yy').Year, expected(76));
asserts.isequal(datetime('24', 'InputFormat', 'y').Year, expected(24));
asserts.isequal(datetime('3', 'InputFormat', 'yy').Year, 3);
asserts.isequal(datetime('2024', 'InputFormat', 'yy').Year, 2024);
asserts.isequal(datetime('10/11/24', 'InputFormat', 'MM/dd/yy').Year, expected(24));
asserts.isequal(datetime('240102', 'InputFormat', 'yyMMdd').Year, expected(24));
asserts.isequal(datetime('24', 'Format', 'yy').Year, expected(24));
asserts.isequal(datetime('1/2/3', 'Format', 'MM/dd/yy').Year, 3);
asserts.isequal(datetime('24', 'InputFormat', 'yy', 'PivotYear', 1900).Year, 1924);
asserts.isequal(datetime('99', 'InputFormat', 'yy', 'PivotYear', 2000).Year, 2099);
asserts.isequal(datetime('00', 'InputFormat', 'yy', 'PivotYear', 1950).Year, 2000);
asserts.checkerror('datetime(''24'', ''InputFormat'', ''yy'', ''PivotYear'', 1.5)', ...
  'The pivot year must be an integer scalar.', 'Nelson:datetime:InvalidPivotYear');
%=============================================================================
% abutting numeric fields: each field but the first takes its exact width,
% the first takes the rest; a 'y' year cut to two digits uses the pivot year
abutting = {'2401', 'yyyyMM', [2024 1 1]; '202401', 'yyyyMM', [2024 1 1]; '24011', 'yyyyMM', [240 11 1]; ...
  '240102', 'yyyyMMdd', [2024 1 2]; '2401021', 'yyyyMMdd', [240 10 21]; '20240102', 'yyyyMMdd', [2024 1 2]; ...
  '122024', 'MMyyyy', [2024 12 1]; '12245', 'MMyyyy', [2245 1 1]; '2401', 'uuuuMM', [24 1 1]; ...
  '2401', 'yyyyM', [240 1 1]; '24011', 'yyyyM', [2401 1 1]; '2024-0102', 'yyyy-MMdd', [2024 1 2]; ...
  '240102T0230', 'yyyyMMdd''T''HHmm', [2024 1 2]};
for k = 1:size(abutting, 1)
  t = datetime(abutting{k, 1}, 'InputFormat', abutting{k, 2});
  asserts.isequal([t.Year, t.Month, t.Day], abutting{k, 3});
end
t = datetime('2401021230', 'InputFormat', 'yyyyMMddHHmm');
asserts.isequal([t.Year, t.Month, t.Day, t.Hour, t.Minute], [2024 1 2 12 30]);
t = datetime('102', 'InputFormat', 'ddMM');
asserts.isequal([t.Day, t.Month], [1 2]);
asserts.isequal(datetime('9901', 'InputFormat', 'yyyyMM', 'PivotYear', 1976).Year, 1999);
asserts.isequal(datetime('7601', 'InputFormat', 'yyyyMM', 'PivotYear', 1976).Year, 1976);
asserts.isequal(datetime('7501', 'InputFormat', 'yyyyMM', 'PivotYear', 1976).Year, 2075);
unreadable = {'20241', 'yyyyMM'; '240', 'yyyyMM'; '2024011', 'yyyyMM'; '24010', 'yyyyMMdd'; '202412', 'yyyyMMdd'; ...
  '1224', 'MMyyyy'; '124', 'MMyyyy'; '12', 'ddMM'; '202401', 'yyMM'; '241', 'yyyyM'; '2024-012', 'yyyy-MMdd'};
for k = 1:size(unreadable, 1)
  cmd = sprintf('datetime(''%s'', ''InputFormat'', ''%s'')', unreadable{k, 1}, unreadable{k, 2});
  asserts.checkerror(cmd, sprintf('Unable to read ''%s'' as a datetime with the format ''%s''.', ...
    unreadable{k, 1}, unreadable{k, 2}), 'Nelson:datetime:ParseErr');
end
% spaces of the format match the spaces of the text
asserts.isequal(datetime('240102 ', 'InputFormat', 'yyyyMMdd ').Day, 2);
asserts.isequal(datetime('2024-01-02  12:30', 'InputFormat', 'yyyy-MM-dd HH:mm').Hour, 12);
%=============================================================================
% date fields the format does not give: today's above its largest date field,
% 1 below it, today when it has none
now0 = datetime('now');
t = datetime('02:30', 'InputFormat', 'HH:mm');
asserts.isequal([t.Year, t.Month, t.Day, t.Hour, t.Minute], [now0.Year, now0.Month, now0.Day, 2, 30]);
t = datetime('15', 'InputFormat', 'dd');
asserts.isequal([t.Year, t.Month, t.Day], [now0.Year, now0.Month, 15]);
t = datetime('05', 'InputFormat', 'MM');
asserts.isequal([t.Year, t.Month, t.Day], [now0.Year, 5, 1]);
t = datetime('2024', 'InputFormat', 'yyyy');
asserts.isequal([t.Year, t.Month, t.Day], [2024, 1, 1]);
t = datetime('15 02:30', 'InputFormat', 'dd HH:mm');
asserts.isequal([t.Year, t.Month, t.Day, t.Hour], [now0.Year, now0.Month, 15, 2]);
t = datetime('PM', 'InputFormat', 'a');
asserts.isequal([t.Day, t.Hour], [now0.Day, 12]);
t = datetime({'02:30'; '03:45'}, 'InputFormat', 'HH:mm');
asserts.isequal(t.Day, [now0.Day; now0.Day]);
tokyo = datetime('now', 'TimeZone', 'Asia/Tokyo');
t = datetime('02:30', 'InputFormat', 'HH:mm', 'TimeZone', 'Asia/Tokyo');
asserts.isequal([t.Year, t.Month, t.Day], [tokyo.Year, tokyo.Month, tokyo.Day]);
%=============================================================================
