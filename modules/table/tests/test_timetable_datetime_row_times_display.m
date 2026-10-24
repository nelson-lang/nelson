%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% datetime row times longer than 15 characters display their text, not
% their size and class
%=============================================================================
TD = timetable(datetime(2020, 1, 1) + hours(0:3)', (1:4)');
R = evalc('disp(TD(2, :))');
assert_istrue(contains(R, '01-Jan-2020 01:00:00'));
assert_isfalse(contains(R, 'datetime'));
R = evalc('disp(TD)');
assert_istrue(contains(R, '01-Jan-2020 03:00:00'));
assert_istrue(contains(R, '01-Jan-2020 00:00:00'));
assert_isfalse(contains(R, 'datetime'));
R = evalc('TD(2:3, :)');
assert_istrue(contains(R, '01-Jan-2020 01:00:00'));
assert_istrue(contains(R, '01-Jan-2020 02:00:00'));
%=============================================================================
% a datetime or duration variable of a table shows its values as well
T = table(datetime(2020, 1, 1) + hours(1), duration(1234567890, 0, 0));
R = evalc('disp(T)');
assert_istrue(contains(R, '01-Jan-2020 01:00:00'));
assert_istrue(contains(R, '1234567890:00:00'));
assert_isfalse(contains(R, '1x1'));
%=============================================================================
% a datetime variable displays every row in the format of its rows
T = table(datetime(2020, 1, 1) + hours([0; 1]), [1; 2]);
R = evalc('disp(T)');
assert_istrue(contains(R, '01-Jan-2020 00:00:00'));
assert_istrue(contains(R, '01-Jan-2020 01:00:00'));
T = table(datetime(2020, 1, 1) + days([0; 1]), [1; 2]);
R = evalc('disp(T)');
assert_istrue(contains(R, '01-Jan-2020'));
assert_isfalse(contains(R, '00:00:00'));
%=============================================================================
