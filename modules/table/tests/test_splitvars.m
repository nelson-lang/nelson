%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1 3; 2 4], 'VariableNames', {'AB'});
R = splitvars(T, 'AB', 'NewVariableNames', {'A', 'B'});
assert_isequal(R.Properties.VariableNames, {'A', 'B'});
assert_isequal(R.A, [1; 2]);
assert_isequal(R.B, [3; 4]);
%=============================================================================
