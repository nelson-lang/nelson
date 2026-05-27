%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [5; 6], 'VariableNames', {'A', 'C'});
T = addvars(T, [3; 4], 'NewVariableNames', {'B'}, 'Before', 'C');
assert_isequal(T.Properties.VariableNames, {'A', 'B', 'C'});
assert_isequal(T.B, [3; 4]);
T = addvars(T, [7; 8], 'NewVariableNames', {'First'}, 'Before', 1);
assert_isequal(T.Properties.VariableNames, {'First', 'A', 'B', 'C'});
assert_isequal(T.First, [7; 8]);
%=============================================================================
