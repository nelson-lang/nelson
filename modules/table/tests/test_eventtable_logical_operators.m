%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% logical and unary operators on an eventtable call the inherited timetable
% methods and return an eventtable
E1 = eventtable(seconds([1; 2]), 'EventLabels', [5; 0]);
%=============================================================================
R = E1 & E1;
assert_isequal(class(R), 'eventtable');
assert_isequal(R.EventLabels, [true; false]);
assert_isequal(R.Properties.RowTimes, seconds([1; 2]));
assert_isequal(class(and(E1, E1)), 'eventtable');
%=============================================================================
R = E1 | E1;
assert_isequal(class(R), 'eventtable');
assert_isequal(R.EventLabels, [true; false]);
assert_isequal(class(or(E1, E1)), 'eventtable');
%=============================================================================
R = ~E1;
assert_isequal(class(R), 'eventtable');
assert_isequal(R.EventLabels, [false; true]);
assert_isequal(class(not(E1)), 'eventtable');
%=============================================================================
R = -E1;
assert_isequal(class(R), 'eventtable');
assert_isequal(R.EventLabels, [-5; 0]);
assert_isequal(class(abs(E1)), 'eventtable');
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 0]);
assert_isequal(class(~TT), 'timetable');
assert_isequal(class(TT & TT), 'timetable');
%=============================================================================
