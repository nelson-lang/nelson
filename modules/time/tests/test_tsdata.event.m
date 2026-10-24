%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ev = tsdata.event('start', 10, 'Units', 'seconds');
asserts.isequal(ev.Name, 'start');
asserts.isequal(ev.Time, 10);
asserts.isequal(get(ev, 'Units'), 'seconds');
asserts.isequal(getTimeStr(ev), '10 seconds');
state = get(ev);
asserts.isequal(state.Name, 'start');
ev = set(ev, 'StartDate', '01-Jan-2024', 'UserData', 42);
asserts.isequal(ev.UserData, 42);
asserts.isequal(getTimeStr(ev), datestr(datenum('01-Jan-2024') + 10 / (24 * 60 * 60)));
same = tsdata.event('start', 10, 'Units', 'seconds', 'StartDate', '01-Jan-2024');
asserts.isequal(ev == same, true);
[found, index] = findEvent([tsdata.event('other', 1), ev], 'start');
asserts.isequal(index, 2);
asserts.isequal(found.Name, 'start');
%=============================================================================
