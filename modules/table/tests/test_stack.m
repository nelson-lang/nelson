%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table({'a'; 'b'}, [1; 2], [3; 4], 'VariableNames', {'ID', 'X', 'Y'});
S = stack(T, {'X', 'Y'}, 'NewDataVariableName', 'Value', 'IndexVariableName', 'Measure');
asserts.isequal(height(S), 4);
asserts.isequal(S.Value, [1; 3; 2; 4]);
%=============================================================================
T2 = table({'a'; 'b'}, [1; 2], [3; 4], 'VariableNames', {'ID', 'X', 'Y'});
S2 = stack(T2, {'X', 'Y'});
asserts.isequal(S2.Properties.VariableNames, {'ID', 'X_Y_Indicator', 'X_Y'});
asserts.isequal(S2.X_Y, [1; 3; 2; 4]);
%=============================================================================
