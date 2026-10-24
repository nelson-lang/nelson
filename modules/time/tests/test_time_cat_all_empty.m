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
% cat along dim > 2 of empty time arrays stacks them
R = cat(3, datetime.empty, datetime.empty);
assert_isequal(class(R), 'datetime');
assert_isequal(size(R), [0 0 2]);
R = cat(4, datetime.empty, datetime.empty);
assert_isequal(class(R), 'datetime');
assert_isequal(size(R), [0 0 1 2]);
R = cat(3, duration.empty, duration.empty);
assert_isequal(class(R), 'duration');
assert_isequal(size(R), [0 0 2]);
R = cat(3, calendarDuration.empty, calendarDuration.empty);
assert_isequal(class(R), 'calendarDuration');
assert_isequal(size(R), [0 0 2]);
%=============================================================================
% an empty next to a non-empty value is skipped
R = cat(3, datetime.empty, datetime(2020, 1, 1));
assert_isequal(size(R), [1 1]);
R = cat(3, duration.empty, seconds(1));
assert_isequal(size(R), [1 1]);
assert_isequal(R, seconds(1));
R = cat(3, calendarDuration.empty, calmonths(1));
assert_isequal(size(R), [1 1]);
%=============================================================================
