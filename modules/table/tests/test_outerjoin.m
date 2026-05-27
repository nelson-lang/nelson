%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
L = table([1; 2], [10; 20], 'VariableNames', {'Key', 'LeftValue'});
R = table([2; 3], [200; 300], 'VariableNames', {'Key', 'RightValue'});
J = outerjoin(L, R, 'Keys', 'Key');
assert_isequal(J.Key_L, [1; 2; NaN]);
assert_isequal(J.Key_R, [NaN; 2; 3]);
%=============================================================================
J = outerjoin(L, R, 'Keys', 'Key', 'MergeKeys', true);
assert_isequal(J.Key, [1; 2; 3]);
%=============================================================================
