%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 'sorted' and 'stable' are mutually exclusive
%=============================================================================
msg = 'You can specify either ''sorted'' or ''stable'' but not both.';
id = 'Nelson:data_analysis:uniqueSortedStableCombination';
assert_checkerror('unique([1 2 1], ''stable'', ''sorted'')', msg, id);
assert_checkerror('unique([1 2 1], ''sorted'', ''stable'')', msg, id);
assert_checkerror('unique([1 2 1], "sorted", "stable")', msg, id);
assert_checkerror('unique([1 2; 1 2], ''rows'', ''stable'', ''sorted'')', msg, id);
assert_checkerror('unique({''b'', ''a''}, ''sorted'', ''stable'')', msg, id);
assert_checkerror('[C, ia, ic] = unique([3 1 3], ''stable'', ''last'', ''sorted'')', msg, id);
assert_checkerror('unique(table([2; 1; 2]), ''stable'', ''sorted'')', msg, id);
%=============================================================================
% repeating the same flag stays valid
assert_isequal(unique([2 1 2], 'stable', 'stable'), [2 1]);
assert_isequal(unique([2 1 2], 'sorted', 'sorted'), [1 2]);
%=============================================================================
