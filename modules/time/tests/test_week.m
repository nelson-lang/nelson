%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(week(datetime(2024, 1, 1)), 1);
asserts.isequal(week(datetime(2024, 1, 8)), 2);
asserts.isequal(week([datetime(2024, 1, 1), datetime(2024, 1, 15)]), [1 3]);
%=============================================================================
% weeks are Sunday-starting calendar weeks; 2020 starts on a Wednesday so
% January 7th already falls in week 2.
asserts.isequal(week(datetime(2020, 1, 1)), 1);
asserts.isequal(week(datetime(2020, 1, 7)), 2);
asserts.isequal(week(datetime(2020, 6, 15)), 25);
asserts.isequal(week(datetime(2020, 12, 31)), 53);
%=============================================================================
% 2017 starts on a Sunday
asserts.isequal(week(datetime(2017, 1, 1)), 1);
asserts.isequal(week(datetime(2017, 1, 8)), 2);
%=============================================================================
