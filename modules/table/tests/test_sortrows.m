%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([3; 1; 2]), [30; 10; 20], 'VariableNames', {'A'});
[S, I] = sortrows(TT);
asserts.isequal(S.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(S.A, [10; 20; 30]);
asserts.isequal(I, [2; 3; 1]);
%=============================================================================
% multi-column and string-column sort
T = table([3; 1; 1], [1; 2; 1], 'VariableNames', {'a', 'b'});
T2 = sortrows(T, {'a', 'b'});
assert_isequal([T2.a T2.b], [1 1; 1 2; 3 1]);
T3 = sortrows(T);
assert_isequal([T3.a T3.b], [1 1; 1 2; 3 1]);
Ts = table(["c"; "a"; "b"], [1; 2; 3], 'VariableNames', {'s', 'n'});
assert_isequal(sortrows(Ts, 's').n', [2 3 1]);
%=============================================================================
