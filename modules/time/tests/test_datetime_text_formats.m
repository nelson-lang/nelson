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
