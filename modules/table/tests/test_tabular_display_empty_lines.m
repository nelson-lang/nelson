%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The separator lines of a loose display are empty, without any blank; the
% compact display of a timerange or a withtol has no separator line.
%=============================================================================
currentFormat = format();
format('loose');
%=============================================================================
T = table([1; 2]);
R = evalc('T');
REF = sprintf(['\nT =\n\n  2', char(215), '1 table\n\n    Var1\n    ____\n\n', ...
  '     1  \n     2  \n\n']);
assert_isequal(R, REF);
%=============================================================================
T0 = table();
R = evalc('T0');
assert_isequal(R, sprintf(['\nT0 =\n\n  0', char(215), '0 empty table\n\n']));
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2]);
R = evalc('TT');
REF = sprintf(['\nTT =\n\n  2', char(215), '1 timetable\n\n    Time']);
assert_isequal(R(1:length(REF)), REF);
%=============================================================================
E = eventtable(seconds([2; 4]));
R = evalc('E');
REF = sprintf(['\nE =\n\n  2', char(215), '1 eventtable\n\n', ...
  '  Event Labels Variable: EventLabels\n', ...
  '  Event Lengths Variable: <instantaneous>\n\n', ...
  '    Time     EventLabels\n']);
assert_isequal(R(1:length(REF)), REF);
%=============================================================================
tr = timerange(seconds(1), seconds(3));
R = evalc('tr');
REF = sprintf(['\ntr = \n\n\ttimetable timerange subscript:\n\n', ...
  '\t\tSelect timetable rows with times in the half-open interval:\n', ...
  '\t\t  Starting at, including:   1 sec\n', ...
  '\t\t  Ending at, but excluding: 3 sec\n\n']);
assert_isequal(R, REF);
%=============================================================================
tre = timerange(seconds(3), seconds(1));
R = evalc('tre');
REF = sprintf(['\ntre = \n\n\ttimetable timerange subscript:\n\n', ...
  '\t\tSelect no rows because the left endpoint is at or after the right endpoint.\n\n']);
assert_isequal(R, REF);
%=============================================================================
w = withtol(seconds([1; 2]), seconds(0.1));
R = evalc('w');
REF = sprintf(['\nw = \n\n\ttimetable withtol subscript:\n\n', ...
  '\t\tSelect timetable rows matching the following times:\n\n', ...
  '\t\t1 sec\n\t\t2 sec\n\t\twith tolerance of +/- 0.1 sec\n\n']);
assert_isequal(R, REF);
%=============================================================================
format('compact');
R = evalc('T');
REF = sprintf(['T =\n  2', char(215), '1 table\n    Var1\n    ____\n']);
assert_isequal(R(1:length(REF)), REF);
%=============================================================================
R = evalc('E');
REF = sprintf(['E =\n  2', char(215), '1 eventtable\n', ...
  '  Event Labels Variable: EventLabels\n', ...
  '  Event Lengths Variable: <instantaneous>\n', ...
  '    Time     EventLabels\n']);
assert_isequal(R(1:length(REF)), REF);
%=============================================================================
R = evalc('tr');
REF = sprintf(['tr = \n\ttimetable timerange subscript:\n', ...
  '\t\tSelect timetable rows with times in the half-open interval:\n', ...
  '\t\t  Starting at, including:   1 sec\n', ...
  '\t\t  Ending at, but excluding: 3 sec\n']);
assert_isequal(R, REF);
%=============================================================================
R = evalc('disp(w)');
REF = sprintf(['\ttimetable withtol subscript:\n', ...
  '\t\tSelect timetable rows matching the following times:\n', ...
  '\t\t1 sec\n\t\t2 sec\n\t\twith tolerance of +/- 0.1 sec\n']);
assert_isequal(R, REF);
%=============================================================================
format(currentFormat);
%=============================================================================
