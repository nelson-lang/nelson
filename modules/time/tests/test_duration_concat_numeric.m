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
% numbers concatenated with a duration are fixed-length 24-hour days
d = [seconds(1); 3];
assert_isequal(class(d), 'duration');
assert_isequal(size(d), [2 1]);
assert_isequal(seconds(d), [1; 3 * 86400]);
assert_isequal(d(2), days(3));
%=============================================================================
d = [seconds(1), 3];
assert_isequal(class(d), 'duration');
assert_isequal(size(d), [1 2]);
assert_isequal(seconds(d), [1, 3 * 86400]);
%=============================================================================
d = [3; seconds(1)];
assert_isequal(class(d), 'duration');
assert_isequal(seconds(d), [3 * 86400; 1]);
%=============================================================================
d = [hours([1 2]); [0.5 2]];
assert_isequal(size(d), [2 2]);
assert_isequal(hours(d), [1 2; 12 48]);
%=============================================================================
d = [minutes(1), true];
assert_isequal(seconds(d), [60, 86400]);
%=============================================================================
% the first duration still sets the format
d = [1, hours(2)];
assert_isequal(d.Format, hours(2).Format);
%=============================================================================
% other types still error
assert_checkerror('[seconds(1), datetime(2020, 1, 1)]', _('Invalid duration input argument.'));
assert_checkerror('[datetime(2020, 1, 1), seconds(1)]', _('Invalid datetime input argument.'));
assert_checkerror('[seconds(1), {1}]', _('Invalid duration input argument.'));
assert_checkerror('[minutes(1), int8(2)]', _('Invalid duration input argument.'));
assert_checkerror('[seconds(1); single(2)]', _('Invalid duration input argument.'));
