%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
t = datetime(2024, 1, 1) + caldays(3);
asserts.isequal(day(t), 4);
t = datetime(2024, 1, 1) + caldays([1 2]);
asserts.isequal(day(t), [2 3]);
asserts.istrue(iscalendarduration(caldays(2)));
%=============================================================================
% caldays extracts the days component from a calendarDuration
assert_isequal(caldays(caldays(10)), 10);
assert_isequal(caldays(calweeks(3)), 21);
%=============================================================================
