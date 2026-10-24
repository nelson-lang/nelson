%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the format of a datetime variable is the format of the whole variable, not
% of the rows shown by the compact display of a long table
T = table(datetime(2020, 1, 1) + days(0:29)', (1:30)');
T.Var1(15) = T.Var1(15) + hours(1);
R = evalc('disp(T)');
assert_istrue(contains(R, '01-Jan-2020 00:00:00'));
assert_istrue(contains(R, '30-Jan-2020 00:00:00'));
T = table(datetime(2020, 1, 1) + days(0:29)', (1:30)');
R = evalc('disp(T)');
assert_istrue(contains(R, '01-Jan-2020'));
assert_isfalse(contains(R, '00:00:00'));
%=============================================================================
% a multi-column datetime variable keeps its size and shows every element in
% one format
X = [datetime(2020, 1, 1), datetime(2020, 1, 2) + hours(3); datetime(2020, 1, 3), datetime(2020, 1, 4)];
T = table(X);
assert_isequal(size(T), [2 1]);
assert_isequal(size(T.X), [2 2]);
R = evalc('disp(T)');
assert_istrue(contains(R, '01-Jan-2020 00:00:00    02-Jan-2020 03:00:00'));
assert_istrue(contains(R, '03-Jan-2020 00:00:00    04-Jan-2020 00:00:00'));
T = table([datetime(2020, 1, 1), datetime(2020, 1, 2)]);
assert_isequal(size(T), [1 1]);
R = evalc('disp(T)');
assert_istrue(contains(R, '01-Jan-2020    02-Jan-2020'));
assert_isfalse(contains(R, 'datetime'));
%=============================================================================
% same for a multi-column duration variable: one row per table row
T = table([days(1), days(2)]);
assert_isequal(size(T), [1 1]);
%=============================================================================
