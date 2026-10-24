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
% cellstr returns the display text, 'NaN' for a NaN duration
C = cellstr(seconds([NaN 1]));
assert_isequal(C, {'NaN sec', '1 sec'});
assert_istrue(iscellstr(C));
assert_isequal(cellstr(hours([1 2; NaN 4])), {'1 hr', '2 hr'; 'NaN hr', '4 hr'});
assert_isequal(cellstr(duration(1, 2, 3)), {'01:02:03'});
assert_isequal(cellstr(minutes(-1)), {'-1 min'});
assert_isequal(size(cellstr(seconds(zeros(0, 2)))), [0 2]);
%=============================================================================
% the Format property and an explicit format
d = seconds(90);
d.Format = 'mm:ss';
assert_isequal(cellstr(d), {'01:30'});
assert_isequal(cellstr(seconds(90), 'mm:ss'), {'01:30'});
assert_isequal(cellstr(seconds(90), 'm'), {'1.5 min'});
assert_isequal(cellstr(hours(1.5), "hh:mm"), {'01:30'});
assert_isequal(cellstr(seconds(1), 's'), {'1 sec'});
%=============================================================================
