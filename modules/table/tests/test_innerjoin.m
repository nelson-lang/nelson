%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
L = table([1; 2; 3], [10; 20; 30], 'VariableNames', {'Key', 'LeftValue'});
R = table([2; 3; 4], [200; 300; 400], 'VariableNames', {'Key', 'RightValue'});
J = innerjoin(L, R, 'Keys', 'Key');
assert_isequal(J.Key, [2; 3]);
assert_isequal(J.LeftValue, [20; 30]);
assert_isequal(J.RightValue, [200; 300]);
%=============================================================================
