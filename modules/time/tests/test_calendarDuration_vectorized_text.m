%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% calendarDuration array text is rendered in one vectorized pass; only the
% non-zero components appear, an all-zero value shows as '0d', and a present
% time component is shown as hours/minutes/seconds.
%=============================================================================
assert_isequal(string(calendarDuration(1, 2, 3)), "1y 2mo 3d");
assert_isequal(string(calendarDuration(0, 13, 0, 0, 0, 3661)), "1y 1mo 1h 1m 1s");
assert_isequal(string(calmonths(0)), "0d");
assert_isequal(string(calendarDuration(1, 1, 1, 0, 0, 3661.25)), "1y 1mo 1d 1h 1m 1.25s");
%=============================================================================
% A mixed array: each element shows only its non-zero parts.
c = [calmonths(1), caldays(7), calyears(2) + caldays(3)];
assert_isequal(string(c), ["1mo", "7d", "2y 3d"]);
%=============================================================================
% Negative components keep their sign.
assert_isequal(string(calmonths([-1 -13])'), ["-1mo"; "-1y -1mo"]);
%=============================================================================
% Shapes preserved.
assert_isequal(size(string(calmonths(reshape(1:6, 2, 3)))), [2 3]);
assert_isequal(size(string(calmonths(zeros(0, 3)))), [0 3]);
%=============================================================================
