%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
L = table([1; 2; 3], [10; 20; 30], [100; 200; 300], 'VariableNames', {'K', 'LX', 'KeepL'});
R = table([2; 3; 4], [200; 300; 400], [5; 6; 7], 'VariableNames', {'K', 'RX', 'KeepR'});
%=============================================================================
T = innerjoin(L, R, 'Keys', 'K', 'LeftVariables', {'LX'}, 'RightVariables', {'RX'});
asserts.isequal(T.Properties.VariableNames, {'LX', 'RX'});
asserts.isequal(T.LX, [20; 30]);
asserts.isequal(T.RX, [200; 300]);
%=============================================================================
T = innerjoin(L, R, 'Keys', 'K', 'LeftVariables', {'K', 'LX'}, 'RightVariables', {'K', 'RX'});
asserts.isequal(T.Properties.VariableNames, {'K_L', 'LX', 'K_R', 'RX'});
asserts.isequal(T.K_L, [2; 3]);
asserts.isequal(T.K_R, [2; 3]);
%=============================================================================
T = outerjoin(L, R, 'Keys', 'K', 'Type', 'left', 'MergeKeys', true);
asserts.isequal(T.Properties.VariableNames, {'K', 'LX', 'KeepL', 'RX', 'KeepR'});
asserts.isequal(T.K, [1; 2; 3]);
%=============================================================================
T = outerjoin(L, R, 'Keys', 'K', 'Type', 'left', 'LeftVariables', {'LX'}, 'RightVariables', {'RX'});
asserts.isequal(T.Properties.VariableNames, {'LX', 'RX'});
asserts.isequal(T.LX, [10; 20; 30]);
asserts.istrue(isnan(T.RX(1)));
asserts.isequal(T.RX(2:3), [200; 300]);
%=============================================================================
