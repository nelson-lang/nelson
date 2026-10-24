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
% a string before a datetime or a duration is converted by that class
d = ["2020-01-01" datetime(2020, 1, 2)];
assert_isequal(class(d), 'datetime');
assert_isequal(size(d), [1 2]);
assert_isequal(day(d), [1 2]);
d = [datetime(2020, 1, 2) "2020-01-03"];
assert_isequal(day(d), [2 3]);
d = ["1:00:00" seconds(1)];
assert_isequal(class(d), 'duration');
assert_isequal(seconds(d), [3600 1]);
%=============================================================================
% without an object, strings still win
assert_isequal(class(["a" {1}]), 'string');
assert_isequal(class([{1} "a"]), 'string');
assert_isequal(class(["a" 1]), 'string');
%=============================================================================
