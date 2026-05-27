%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
T.C = [5; 6];
assert_isequal(T.Properties.VariableNames, {'A', 'B', 'C'});
assert_isequal(T.C, [5; 6]);
T{1, 'A'} = 10;
assert_isequal(T.A, [10; 2]);
T(:, 'B') = [];
assert_isequal(T.Properties.VariableNames, {'A', 'C'});
%=============================================================================
