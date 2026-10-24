%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% char/string accept an explicit display format for datetime and duration.
%=============================================================================
assert_isequal(char(datetime(2020, 3, 5), 'yyyy-MM-dd'), '2020-03-05');
assert_isequal(char(datetime(2020, 3, 5, 13, 7, 9), 'HH:mm:ss'), '13:07:09');
assert_isequal(char(datetime(2020, 1, 1), 'eeee'), 'Wednesday');
assert_isequal(char(datetime(2020, 1, 1), 'MMMM'), 'January');
assert_isequal(string(datetime(2020, 3, 5), 'dd/MM/yyyy'), "05/03/2020");
%=============================================================================
% Day-of-year token D (zero-padded to the number of D letters).
%=============================================================================
assert_isequal(char(datetime(2020, 3, 5), 'D'), '65');
assert_isequal(char(datetime(2020, 3, 5), 'DD'), '65');
assert_isequal(char(datetime(2020, 3, 5), 'DDD'), '065');
assert_isequal(char(datetime(2021, 1, 1), 'DDD'), '001');
assert_isequal(char(datetime(2020, 12, 31), 'DDD'), '366');
assert_isequal(char(datetime(2021, 12, 31), 'DDD'), '365');
%=============================================================================
% Duration char/string with a format.
%=============================================================================
assert_isequal(char(hours(1.5), 'hh:mm'), '01:30');
assert_isequal(char(hours(1.5), 'hh:mm:ss'), '01:30:00');
assert_isequal(string(hours(1.5), 'hh:mm'), "01:30");
assert_isequal(char(days(2.5), 'dd:hh:mm:ss'), '02:12:00:00');
%=============================================================================
% One-argument forms are unchanged.
%=============================================================================
assert_isequal(char(datetime(2020, 3, 5)), '05-Mar-2020');
assert_isequal(char(duration(1, 2, 3)), '01:02:03');
assert_isequal(char(NaT), 'NaT');
%=============================================================================
% dateshift keeps the adaptive default format: a midnight result shows no time.
%=============================================================================
assert_isequal(char(dateshift(datetime(2020, 3, 15, 10, 30, 0), 'end', 'day')), '16-Mar-2020');
assert_isequal(char(dateshift(datetime(2020, 3, 15, 10, 30, 0), 'start', 'day')), '15-Mar-2020');
assert_isequal(char(dateshift(datetime(2020, 3, 15, 10, 30, 0), 'start', 'month')), '01-Mar-2020');
assert_isequal(char(dateshift(datetime(2020, 3, 15, 10, 30, 0), 'start', 'hour')), '15-Mar-2020 10:00:00');
%=============================================================================
