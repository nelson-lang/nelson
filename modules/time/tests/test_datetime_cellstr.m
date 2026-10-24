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
% cellstr returns the display text, 'NaT' for not-a-time
C = cellstr(datetime(2020, 1, [1 NaN]));
assert_isequal(C, {'01-Jan-2020', 'NaT'});
assert_istrue(iscellstr(C));
assert_isequal(cellstr(NaT), {'NaT'});
C = cellstr(datetime(2020, 1, [1 2; 3 NaN]));
assert_isequal(C, {'01-Jan-2020', '02-Jan-2020'; '03-Jan-2020', 'NaT'});
assert_isequal(size(cellstr(NaT(0, 3))), [0 3]);
%=============================================================================
% the Format property and an explicit format
d = datetime(2020, 1, 1);
d.Format = 'yyyy';
assert_isequal(cellstr(d), {'2020'});
assert_isequal(cellstr(datetime(2020, 1, [1 NaN]), 'yyyy-MM-dd'), {'2020-01-01', 'NaT'});
assert_isequal(cellstr(datetime(2020, 1, 1), "yyyy"), {'2020'});
assert_isequal(cellstr(datetime(2020, 1, 1), 'yyyy', 'en_US'), {'2020'});
%=============================================================================
msg = 'Format must be a character vector or string scalar.';
asserts.checkerror('cellstr(datetime(2020, 1, 1), 1)', msg);
msg = 'Locale must be a character vector or string scalar.';
asserts.checkerror('cellstr(datetime(2020, 1, 1), ''yyyy'', 1)', msg);
%=============================================================================
