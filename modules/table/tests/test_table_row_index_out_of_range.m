%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a row subscript beyond the height raises the table row index error
%=============================================================================
msg = _('Row index exceeds table dimensions.');
id = 'Nelson:table:rowIndexOutOfRange';
T = table([1; 2]);
assert_checkerror('R = T(5, :);', msg, id);
assert_checkerror('R = T(3:4, :);', msg, id);
assert_checkerror('R = T(5, 1);', msg, id);
assert_checkerror('R = T(logical([1 0 1]), :);', msg, id);
assert_checkerror('R = T{5, 1};', msg, id);
assert_checkerror('R = T{logical([0 0 1]), 1};', msg, id);
R = T(logical([1 0 0]), :);
assert_isequal(R.Var1, 1);
R = T{logical([0 1 0]), 1};
assert_isequal(R, 2);
%=============================================================================
T = table([1; 2], [3; 4]);
assert_checkerror('R = T(5, 1:2);', msg, id);
assert_checkerror('R = T{5, 2};', msg, id);
%=============================================================================
T = table([1; 2], 'RowNames', {'a'; 'b'});
assert_checkerror('R = T(5, :);', msg, id);
assert_checkerror('R = T(logical([1 0 1]), :);', msg, id);
assert_checkerror('R = T{5, 1};', msg, id);
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
assert_checkerror('R = TT(5, :);', msg, id);
assert_checkerror('R = TT{5, 1};', msg, id);
assert_checkerror('R = TT(logical([1 0 1]), :);', msg, id);
%=============================================================================
% invalid subscripts keep the indexing error
T = table([1; 2]);
assert_checkerror('R = T(0, :);', _('Zero or negative index encountered.'));
assert_checkerror('R = T(1.5, :);', _('index must either be real positive integers or logicals.'));
assert_checkerror('R = T{0, 1};', _('Zero or negative index encountered.'));
%=============================================================================
