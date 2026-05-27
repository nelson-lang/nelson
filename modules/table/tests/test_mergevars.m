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
R = mergevars(T, {'A', 'B'}, 'NewVariableName', 'AB');
assert_isequal(R.Properties.VariableNames, {'AB'});
assert_isequal(R.AB, [1 3; 2 4]);
%=============================================================================
