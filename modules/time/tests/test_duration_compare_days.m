%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a number compared with a duration is a count of days
asserts.isequal(seconds([1 -1]) > 0, [true false]);
asserts.istrue(0 < seconds(1));
asserts.isfalse(seconds(1) == 0);
asserts.istrue(seconds(0) >= 0);
asserts.isfalse(seconds(1) > 1);
asserts.isfalse(hours(1) > 0.5);
asserts.istrue(hours(13) > 0.5);
asserts.istrue(days(2) == 2);
asserts.istrue(hours(24) <= 1);
asserts.checkerror('seconds(1) > ''a''', 'Duration operands expected.', 'Nelson:time:durationOperandsExpected');
%=============================================================================
