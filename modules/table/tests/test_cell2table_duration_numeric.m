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
% a column mixing a duration and a number becomes a duration column (days)
T = cell2table({"a", seconds(1); "b", 3});
assert_isequal(class(T.Var2), 'duration');
assert_isequal(size(T.Var2), [2 1]);
assert_isequal(seconds(T.Var2), [1; 3 * 86400]);
%=============================================================================
