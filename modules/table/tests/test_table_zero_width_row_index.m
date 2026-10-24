%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a row index beyond the height is rejected even when no variable is selected
%=============================================================================
msg = _('Row index exceeds table dimensions.');
id = 'Nelson:table:rowIndexOutOfRange';
T = table([1; 2]);
T(:, 1) = [];
assert_checkerror('R = T(5, :);', msg, id);
assert_checkerror('R = T(5, []);', msg, id);
assert_checkerror('R = T([true false true], []);', msg, id);
R = T(2, []);
assert_isequal(size(R), [1 0]);
R = T([2 1 2], :);
assert_isequal(size(R), [3 0]);
%=============================================================================
T = table([1; 2]);
assert_checkerror('R = T(5, []);', msg, id);
assert_checkerror('R = T([true false true], []);', msg, id);
R = T(2, []);
assert_isequal(size(R), [1 0]);
%=============================================================================
T = table([1; 2], 'RowNames', {'a', 'b'});
assert_checkerror('R = T(3, []);', msg, id);
R = T(2, []);
assert_isequal(R.Properties.RowNames, {'b'});
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
assert_checkerror('R = TT(5, []);', msg, id);
TT(:, 1) = [];
assert_checkerror('R = TT(5, :);', msg, id);
%=============================================================================
