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
t = datetime(2024, 5, 17, 13, 14, 15);
asserts.isequal(day(dateshift(t, 'start', 'month')), 1);
asserts.isequal(day(dateshift(t, 'end', 'month')), 31);
asserts.isequal(hour(dateshift(t, 'start', 'day')), 0);
asserts.isequal(weekday(dateshift(datetime(2024, 5, 17), 'dayofweek', 'Monday', 'next')), 2);
asserts.isequal(month(dateshift(t, 'start', 'quarter')), 4);
%=============================================================================
t = datetime(2020 + mod(0:999, 5), 1 + mod(0:999, 12), 1 + mod(0:999, 28));
r = dateshift(t, 'start', 'month');
asserts.isequal(size(r), size(t));
asserts.isequal(day(r), ones(size(t)));
asserts.isequal(hour(r), zeros(size(t)));
%=============================================================================
