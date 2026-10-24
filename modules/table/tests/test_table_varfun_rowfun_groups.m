%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table({'a'; 'a'; 'b'}, [1; 2; 4], [10; 20; 30], 'VariableNames', {'G', 'X', 'Y'});
V = varfun(@mean, T, 'InputVariables', vartype('double'));
asserts.isequal(V.mean_X, 7 / 3);
asserts.isequal(V.mean_Y, 20);
R = rowfun(@(x, y) x + y, T, 'InputVariables', {'X', 'Y'}, 'OutputVariableNames', 'Sum');
asserts.isequal(R.Sum, [11; 22; 34]);
C = groupcounts(T, 'G');
asserts.isequal(C.G, {'a'; 'b'});
asserts.isequal(C.GroupCount, [2; 1]);
G = groupsummary(T, 'G', 'sum', {'X', 'Y'});
asserts.isequal(G.sum_X, [3; 4]);
asserts.isequal(G.sum_Y, [30; 30]);
%=============================================================================
