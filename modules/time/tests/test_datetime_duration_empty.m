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
% datetime.empty and duration.empty
d = datetime.empty;
assert_isequal(class(d), 'datetime');
assert_isequal(size(d), [0 0]);
assert_istrue(isempty(d));
assert_isequal(size(datetime.empty(0, 3)), [0 3]);
assert_isequal(size(datetime.empty(3, 0)), [3 0]);
assert_isequal(size(datetime.empty([0 2])), [0 2]);
assert_isequal(size(datetime.empty(0, 2, 3)), [0 2 3]);
assert_isequal(size(isnat(datetime.empty(0, 2))), [0 2]);
d = duration.empty;
assert_isequal(class(d), 'duration');
assert_isequal(size(d), [0 0]);
assert_isequal(size(duration.empty(0, 3)), [0 3]);
assert_isequal(size(isnan(duration.empty(2, 0))), [2 0]);
%=============================================================================
msg = _('At least one dimension must be zero to create an empty array.');
asserts.checkerror('datetime.empty(1, 1)', msg);
asserts.checkerror('duration.empty(2)', msg);
%=============================================================================
% growing an array fills the gaps with NaT (datetime) or zero (duration)
d = datetime.empty;
d(2) = datetime(2020, 1, 1);
assert_isequal(isnat(d), [true false]);
d = datetime.empty(0, 1);
d(end + 1) = datetime(2020, 1, 1);
assert_isequal(size(d), [1 1]);
d = datetime(2020, 1, 1);
d(3) = datetime(2020, 1, 3);
assert_isequal(isnat(d), [false true false]);
d = duration.empty;
d(2) = seconds(3);
assert_isequal(seconds(d), [0 3]);
%=============================================================================
d = [datetime.empty, datetime(2020, 1, 1)];
assert_isequal(size(d), [1 1]);
assert_isequal(cellstr(d), {'01-Jan-2020'});
%=============================================================================
