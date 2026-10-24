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
t = datetime(2024, 1, 31) + calmonths(1);
asserts.isequal([year(t), month(t), day(t)], [2024 2 29]);
t = datetime(2024, 1, 1) + calmonths([1 2]);
asserts.isequal(month(t), [2 3]);
asserts.istrue(iscalendarduration(calmonths(2)));
%=============================================================================
% calmonths extracts the months component from a calendarDuration
assert_isequal(calmonths(calyears(2)), 24);
assert_isequal(calmonths(calmonths(5)), 5);
assert_isequal(calmonths(calyears(1) + caldays(3)), 12);
assert_isequal(class(calmonths(calyears(2))), 'double');
%=============================================================================
% scalar multiplication of a calendarDuration
assert_isequal(calmonths(calmonths(2) * 3), 6);
assert_isequal(calmonths(3 * calmonths(2)), 6);
%=============================================================================
