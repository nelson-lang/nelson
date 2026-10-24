%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
c = calendarDuration(1, 2, 3);
t = datetime(2024, 1, 31) + c;
asserts.isequal([year(t), month(t), day(t)], [2025, 4, 3]);
c = calendarDuration(0, [1 2], [3 4]);
t = datetime(2024, 1, 1) + c;
asserts.isequal(day(t), [4 5]);
c = calendarDuration(0, 0, 0, 1, 2, 3);
asserts.isequal(seconds(time(c)), 3723);
%=============================================================================
% Display shows only non-zero components (all-zero shows '0d').
asserts.isequal(char(calyears(1)), '1y');
asserts.isequal(char(calmonths(2)), '2mo');
asserts.isequal(char(caldays(5)), '5d');
asserts.isequal(char(calyears(1) + calmonths(2) + caldays(3)), '1y 2mo 3d');
asserts.isequal(char(calmonths(1) + caldays(15)), '1mo 15d');
asserts.isequal(char(calyears(2) + caldays(10)), '2y 10d');
asserts.isequal(char(caldays(0)), '0d');
asserts.isequal(char(calmonths(0)), '0d');
%=============================================================================
