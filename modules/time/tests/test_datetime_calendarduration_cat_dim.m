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
% cat along the third dimension keeps the requested dimension
t = cat(3, datetime(2020, 1, 1), datetime(2021, 2, 3));
assert_isequal(class(t), 'datetime');
assert_isequal(size(t), [1 1 2]);
assert_isequal(datenum(t), cat(3, datenum(2020, 1, 1), datenum(2021, 2, 3)));
%=============================================================================
t = cat(1, datetime(2020, 1, 1), datetime(2021, 2, 3));
assert_isequal(size(t), [2 1]);
t = cat(2, datetime(2020, 1, 1), datetime(2021, 2, 3));
assert_isequal(size(t), [1 2]);
%=============================================================================
t = cat(3, [datetime(2020, 1, 1), datetime(2020, 1, 2)], [datetime(2021, 1, 1), datetime(2021, 1, 2)]);
assert_isequal(size(t), [1 2 2]);
assert_isequal(datenum(t(1, 2, 2)), datenum(2021, 1, 2));
%=============================================================================
c = cat(3, calmonths(1), caldays(12));
assert_isequal(class(c), 'calendarDuration');
assert_isequal(size(c), [1 1 2]);
assert_isequal(split(c, 'months'), cat(3, 1, 0));
assert_isequal(split(c, 'days'), cat(3, 0, 12));
%=============================================================================
c = cat(1, calmonths(1), caldays(12));
assert_isequal(size(c), [2 1]);
%=============================================================================
% arithmetic with an N-D duration keeps its shape
t = datetime(2020, 1, 1) + days(cat(3, 0, 1));
assert_isequal(class(t), 'datetime');
assert_isequal(size(t), [1 1 2]);
assert_isequal(datenum(t), cat(3, datenum(2020, 1, 1), datenum(2020, 1, 2)));
%=============================================================================
t = datetime(2020, 1, 2) - days(cat(3, 0, 1));
assert_isequal(size(t), [1 1 2]);
assert_isequal(datenum(t), cat(3, datenum(2020, 1, 2), datenum(2020, 1, 1)));
%=============================================================================
