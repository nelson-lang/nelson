%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% A datetime read from text is the exact instant it names
%=============================================================================
A = datetime('2020-01-01 02:00:00');
B = datetime(2020, 1, 1, 2, 0, 0);
assert_isequal(milliseconds(A - B), 0);
assert_isequal(milliseconds(datetime(2020, 1, 1) + hours(2) - A), 0);
assert_istrue(A == B);
assert_isfalse(A ~= B);
assert_isequal(second(A), 0);
%=============================================================================
C = datetime(2020, 1, 1) + hours(0:3)';
assert_isequal(C == A, [false; false; true; false]);
assert_isequal(C >= A, [false; false; true; true]);
assert_isequal(C < A, [true; true; false; false]);
%=============================================================================
T = datetime({'2020-01-01 02:00:00', 'NaT'; '2021-06-15 23:59:59', '2020-01-01'});
R = datetime([2020, 2020; 2021, 2020], [1, 1; 6, 1], [1, 1; 15, 1], [2, 0; 23, 0], [0, 0; 59, 0], [0, 0; 59, 0]);
D = milliseconds(T - R);
assert_isequal(D([1 2 4]), [0, 0, 0]);
assert_istrue(isnan(D(3)));
assert_isequal(T == R, [true, false; true, true]);
%=============================================================================
S = datetime(["2020-01-01 02:00:00"; "2020-01-01 03:00:00"]);
assert_isequal(S == C(3:4), [true; true]);
%=============================================================================
F = datetime('2020-01-01T02:00:00.123', 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS');
assert_isequal(round(milliseconds(F - B), 9), 123);
assert_istrue(F == B + milliseconds(123));
%=============================================================================
Z = datetime('2020-01-01 02:00:00', 'TimeZone', 'UTC');
assert_istrue(Z == datetime(2020, 1, 1, 2, 0, 0, 'TimeZone', 'UTC'));
%=============================================================================
assert_istrue(isnat(datetime('NaT')));
%=============================================================================
