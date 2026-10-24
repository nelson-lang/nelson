%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build a schedule from a month-end date using calendar durations.
scheduleStart = datetime(2024, 1, 31);
monthlySchedule = scheduleStart + calmonths(0:5);
scheduleDates = yyyymmdd(monthlySchedule);
weekendFlags = isweekend(monthlySchedule);
nextMonthStarts = dateshift(monthlySchedule, 'start', 'month', 'next');

disp('Monthly schedule:');
disp(scheduleDates);
disp('Falls on a weekend:');
disp(weekendFlags);
