%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], [5; 6], 'VariableNames', {'A', 'B', 'C'});
T = movevars(T, 'C', 'Before', 1);
assert_isequal(T.Properties.VariableNames, {'C', 'A', 'B'});
T = movevars(T, 'A', 'After', 'B');
assert_isequal(T.Properties.VariableNames, {'C', 'B', 'A'});
%=============================================================================
