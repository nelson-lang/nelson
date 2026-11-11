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
asserts.isequal(nargin('timezoneslist'), 1);
asserts.isequal(nargout('timezoneslist'), 2);
asserts.isequal(nargin('timezoneoffset'), 4);
asserts.isequal(nargout('timezoneoffset'), 2);
asserts.isequal(nargin('timezonedst'), 4);
asserts.isequal(nargout('timezonedst'), 2);
asserts.isequal(nargin('timezonename'), 2);
asserts.isequal(nargout('timezonename'), 1);
%=============================================================================
% wall-clock time skipped by the spring DST gap moves forward
t = datetime(2024, 3, 31, 2, 30, 0, 'TimeZone', 'Europe/Paris');
asserts.isequal(char(t), '31-Mar-2024 03:30:00');
asserts.isequal(isdst(t), true);
asserts.isequal(seconds(tzoffset(t)), 7200);
u = t;
u.TimeZone = 'UTC';
asserts.isequal(char(u), '31-Mar-2024 01:30:00');
%=============================================================================
t = datetime(2024, 3, 10, 2, 30, 0, 'TimeZone', 'America/New_York');
asserts.isequal(char(t), '10-Mar-2024 03:30:00');
asserts.isequal(isdst(t), true);
asserts.isequal(seconds(tzoffset(t)), -14400);
%=============================================================================
% ambiguous autumn time takes the standard time occurrence
t = datetime(2024, 10, 27, 2, 30, 0, 'TimeZone', 'Europe/Paris');
asserts.isequal(char(t), '27-Oct-2024 02:30:00');
asserts.isequal(isdst(t), false);
asserts.isequal(seconds(tzoffset(t)), 3600);
%=============================================================================
% raw builtin: a gap time uses the offset before the gap to reach UTC
gap = datenum(2024, 3, 31, 2, 30, 0);
asserts.isequal(timezoneoffset(gap, 'Europe/Paris'), 3600);
asserts.isequal(timezoneoffset(gap - 1/24, 'Europe/Paris', '', 'utc'), 7200);
%=============================================================================
