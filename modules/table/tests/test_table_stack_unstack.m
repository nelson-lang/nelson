%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table({'A'; 'B'}, [1; 2], [3; 4], 'VariableNames', {'ID', 'X', 'Y'});
S = stack(T, {'X', 'Y'}, 'NewDataVariableName', 'Value', 'IndexVariableName', 'Measure');
asserts.isequal(height(S), 4);
asserts.isequal(S.Measure, {'X'; 'Y'; 'X'; 'Y'});
asserts.isequal(S.Value, [1; 3; 2; 4]);
U = unstack(S, 'Value', 'Measure', 'GroupingVariables', 'ID');
asserts.isequal(U.ID, {'A'; 'B'});
asserts.isequal(U.X, [1; 2]);
asserts.isequal(U.Y, [3; 4]);
%=============================================================================
