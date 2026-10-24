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
% missing compares as NaN (duration) or NaT (datetime): never equal, never ordered
assert_isequal(seconds(1) == missing, false);
assert_isequal(missing == seconds(1), false);
assert_isequal(seconds([1 NaN]) == missing, [false false]);
assert_isequal(seconds(1) ~= missing, true);
assert_isequal(seconds(1) < missing, false);
assert_isequal(seconds(1) <= missing, false);
assert_isequal(seconds(1) > missing, false);
assert_isequal(seconds(1) >= missing, false);
%=============================================================================
assert_isequal(datetime(2020, 1, 1) == missing, false);
assert_isequal(datetime(2020, 1, [1 2]) == missing, [false false]);
assert_isequal(datetime(2020, 1, 1) ~= missing, true);
assert_isequal(missing < datetime(2020, 1, 1), false);
assert_isequal(datetime(2020, 1, 1) >= missing, false);
%=============================================================================
