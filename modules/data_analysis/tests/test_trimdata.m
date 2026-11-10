%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(trimdata(1:6, 4), 1:4);
assert_isequal(trimdata(1:6, 4, 'Side', 'leading'), 3:6);
assert_isequal(trimdata(1:6, 3, 'Side', 'both'), 2:4);
assert_isequal(trimdata(1:7, 4, 'Side', 'both'), 2:5);
M = magic(4);
assert_isequal(trimdata(M, 2), M(1:2, :));
assert_isequal(trimdata(M, 3, 'Dimension', 2), M(:, 1:3));
assert_isequal(trimdata(M, [2 3]), M(1:2, 1:3));
assert_isequal(trimdata(1:3, 5), 1:3);
assert_isequal(trimdata((1:5).', 2), [1; 2]);
assert_isequal(trimdata({1, 2, 3}, 2), {1, 2});
assert_isequal(trimdata(["a" "b" "c"], 2), ["a" "b"]);
assert_isequal(trimdata('abcdef', 3), 'abc');
assert_checkerror('trimdata(1:3, -1)', _('Length must be a vector of nonnegative integers.'));
assert_checkerror('trimdata(1:3, 2, ''Side'', ''middle'')', _('Side must be ''leading'', ''trailing'' or ''both''.'));
assert_checkerror('trimdata(1:3, 2, ''Pattern'', ''edge'')', _('Options must be name-value pairs.'));
%=============================================================================
