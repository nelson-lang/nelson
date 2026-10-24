%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% join matches every left row to a single right row, in left order, and looks
% up the right match regardless of the right row order.
L = table([1; 2], [10; 20], 'VariableNames', {'Key', 'LeftValue'});
R = table([2; 1], [200; 100], 'VariableNames', {'Key', 'RightValue'});
J = join(L, R, 'Keys', 'Key');
asserts.isequal(J.Key, [1; 2]);
asserts.isequal(J.LeftValue, [10; 20]);
asserts.isequal(J.RightValue, [100; 200]);
%=============================================================================
% left keys may repeat; each maps to its single right match.
L = table([2; 2; 3], {'a'; 'b'; 'c'}, 'VariableNames', {'id', 'L'});
R = table([2; 3; 4], [20; 30; 40], 'VariableNames', {'id', 'val'});
J = join(L, R);
asserts.isequal(J.id, [2; 2; 3]);
asserts.isequal(J.val, [20; 20; 30]);
%=============================================================================
% the right key must cover every left key value.
L = table([1; 2], [10; 20], 'VariableNames', {'Key', 'LeftValue'});
R = table([2; 3], [200; 300], 'VariableNames', {'Key', 'RightValue'});
msg = _('Right-table key values must cover every left-table key value.');
assert_checkerror('join(L, R, ''Keys'', ''Key'')', msg);
%=============================================================================
% the right key must have unique values.
L = table([1; 2], [10; 20], 'VariableNames', {'Key', 'LeftValue'});
R = table([1; 2; 2], [100; 200; 201], 'VariableNames', {'Key', 'RightValue'});
msg = _('Right-table key values must be unique.');
assert_checkerror('join(L, R, ''Keys'', ''Key'')', msg);
%=============================================================================
