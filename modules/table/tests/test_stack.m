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
assert_isequal(height(S), 4);
assert_isequal(S.Value, [1; 3; 2; 4]);
%=============================================================================
