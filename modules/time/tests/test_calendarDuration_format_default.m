%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
%=============================================================================
% default formats
assert_isequal(calendarDuration().Format, 'ymdt');
assert_isequal(calendarDuration(1, 2, 3).Format, 'ymdt');
assert_isequal(calendarDuration(1, 2, 3, 4, 5, 6).Format, 'ymdt');
assert_isequal(calendarDuration([1 2 3]).Format, 'ymdt');
assert_isequal(calendarDuration.empty().Format, 'ymdt');
assert_isequal(caldays([1 2]).Format, 'ymdt');
assert_isequal(calmonths(1).Format, 'ymdt');
assert_isequal(calyears(1).Format, 'ymdt');
assert_isequal(calweeks(1).Format, 'ymwdt');
assert_isequal(calquarters(1).Format, 'qmdt');
%=============================================================================
% valid formats, char or string
fmts = {'ymdt', 'yqmdt', 'mwdt', 'qmdt', 'mdt', 'ymwdt', 'yqmwdt'};
for k = 1:numel(fmts)
  c = calendarDuration(1, 2, 3, 'Format', fmts{k});
  assert_isequal(c.Format, fmts{k});
  c = calendarDuration(1, 2, 3);
  c.Format = string(fmts{k});
  assert_isequal(c.Format, fmts{k});
end
%=============================================================================
% missing m, d or t: completed with a warning
lastwarn('');
c = calendarDuration(1, 2, 3, 'Format', 'ymd');
[msg, id] = lastwarn();
assert_isequal(id, 'Nelson:time:calendarDurationFormatIncomplete');
assert_isequal(msg, 'Format ''ymd'' must contain ''m'', ''d'' and ''t'': ''ymdt'' is used instead.');
assert_isequal(c.Format, 'ymdt');
assert_isequal(string(c), "1y 2mo 3d");
completed = {'ym', 'ymdt'; 'mt', 'mdt'; 'dt', 'mdt'; 'md', 'mdt'; 'yt', 'ymdt'; 'w', 'mwdt'; 'ywdt', 'ymwdt'};
for k = 1:size(completed, 1)
  lastwarn('');
  c = calendarDuration(1, 2, 3);
  c.Format = completed{k, 1};
  [~, id] = lastwarn();
  assert_isequal(id, 'Nelson:time:calendarDurationFormatIncomplete');
  assert_isequal(c.Format, completed{k, 2});
end
%=============================================================================
% invalid formats
c = calendarDuration(1, 2, 3);
assert_checkerror('c.Format = ''tdm'';', 'Letters of format ''tdm'' are out of order: use ''mdt''.', ...
  'Nelson:time:calendarDurationFormatOrder');
assert_checkerror('c.Format = ''qwmdt'';', 'Letters of format ''qwmdt'' are out of order: use ''qmwdt''.', ...
  'Nelson:time:calendarDurationFormatOrder');
assert_checkerror('c.Format = ''ymdtt'';', 'Format ''ymdtt'' repeats a letter.', ...
  'Nelson:time:duplicateCalendarDurationFormat');
assert_checkerror('c.Format = ''YMDT'';', 'Format ''YMDT'' is not supported: use only letters from ''yqmwdt''.', ...
  'Nelson:time:unrecognizedCalendarDurationFormat');
assert_checkerror('c.Format = '''';', 'Format '''' is not supported: use only letters from ''yqmwdt''.', ...
  'Nelson:time:unrecognizedCalendarDurationFormat');
assert_checkerror('calendarDuration(1, 2, 3, ''Format'', ''xmdt'');', ...
  'Format ''xmdt'' is not supported: use only letters from ''yqmwdt''.', ...
  'Nelson:time:unrecognizedCalendarDurationFormat');
assert_checkerror('c.Format = 5;', 'Format must be a character vector or string scalar.', ...
  'Nelson:validators:mustBeTextScalar');
assert_checkerror('c.Format = ["ymdt", "mdt"];', 'Format must be a character vector or string scalar.', ...
  'Nelson:validators:mustBeTextScalar');
assert_isequal(c.Format, 'ymdt');
%=============================================================================
% display follows the format
c = calendarDuration(1, 14, 10, 1, 2, 3);
c.Format = 'mdt';
assert_isequal(string(c), "26mo 10d 1h 2m 3s");
c.Format = 'yqmwdt';
assert_isequal(string(c), "2y 2mo 1w 3d 1h 2m 3s");
c = calendarDuration(1, 5, 10, 'Format', 'yqmwdt');
assert_isequal(string(c), "1y 1q 2mo 1w 3d");
assert_isequal(string(calendarDuration(0, 0, 0)), "0d");
R = evalc('c');
assert_istrue(contains(R, '1y 1q 2mo 1w 3d'));
%=============================================================================
% concatenation and addition use every unit of the operands
a = calendarDuration(1, 2, 3, 'Format', 'mdt');
b = calendarDuration(1, 2, 3, 'Format', 'ymwdt');
d = calendarDuration(1, 2, 3);
q = calendarDuration(1, 2, 3, 'Format', 'qmdt');
w = calendarDuration(1, 2, 3, 'Format', 'mwdt');
assert_isequal([a a].Format, 'mdt');
assert_isequal([a b].Format, 'ymwdt');
assert_isequal([b a].Format, 'ymwdt');
assert_isequal([a; d].Format, 'ymdt');
assert_isequal(cat(1, a, b).Format, 'ymwdt');
assert_isequal([q w].Format, 'qmwdt');
assert_isequal([a 1].Format, 'mdt');
assert_isequal([1 q].Format, 'qmdt');
assert_isequal((a + b).Format, 'ymwdt');
assert_isequal((b + a).Format, 'ymwdt');
assert_isequal((a - b).Format, 'ymwdt');
assert_isequal((q + w).Format, 'qmwdt');
%=============================================================================
% unary operations, indexing and assignment keep the format
assert_isequal((-a).Format, 'mdt');
assert_isequal((2 * a).Format, 'mdt');
assert_isequal(a(1).Format, 'mdt');
y = a';
assert_isequal(y.Format, 'mdt');
assert_isequal(reshape(a, 1, 1).Format, 'mdt');
x = a;
x(2) = b;
assert_isequal(x.Format, 'mdt');
%=============================================================================
