%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = calendarDuration.empty;
assert_isequal(class(R), 'calendarDuration');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
%=============================================================================
R = calendarDuration.empty(0, 2);
assert_isequal(class(R), 'calendarDuration');
assert_isequal(size(R), [0 2]);
%=============================================================================
R = calendarDuration.empty(3, 0);
assert_isequal(size(R), [3 0]);
%=============================================================================
R = calendarDuration.empty([0 4]);
assert_isequal(size(R), [0 4]);
%=============================================================================
R = calendarDuration.empty(0, 0, 2);
assert_isequal(size(R), [0 0 2]);
%=============================================================================
R = [calendarDuration.empty(0, 1); calendarDuration(1, 2, 3)];
assert_isequal(size(R), [1 1]);
%=============================================================================
assert_checkerror('calendarDuration.empty(2, 3)', _('At least one dimension must be zero to create an empty array.'));
%=============================================================================
