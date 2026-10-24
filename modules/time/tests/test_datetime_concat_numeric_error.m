%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% a number next to a datetime is not a date
msg = 'All inputs must be datetimes or date/time character vectors or date/time strings.';
d = datetime(2020, 1, 1);
assert_checkerror('[d, 5]', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('[5, d]', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('[d; 5]', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('[d, true]', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('[d, int8(3)]', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('[d, single(3)]', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('horzcat(d, 5)', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('vertcat(d, 5)', msg, 'Nelson:time:concatenateDatetimeOperands');
assert_checkerror('cat(3, d, 5)', msg, 'Nelson:time:concatenateDatetimeOperands');
%=============================================================================
% [] contributes nothing
R = [d, []];
assert_isequal(class(R), 'datetime');
assert_isequal(size(R), [1 1]);
R = [[]; d];
assert_isequal(size(R), [1 1]);
%=============================================================================
% text is still parsed as a date
R = [d, '2020-01-03'];
assert_isequal(day(R), [1 3]);
R = [d, "2020-01-04"];
assert_isequal(day(R), [1 4]);
R = [d, {'2020-01-05'}];
assert_isequal(day(R), [1 5]);
%=============================================================================
