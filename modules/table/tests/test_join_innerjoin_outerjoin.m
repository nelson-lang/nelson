%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
L = table([1; 2; 3], {'A'; 'B'; 'C'}, 'VariableNames', {'Key', 'LeftValue'});
R = table([2; 3; 4], [20; 30; 40], 'VariableNames', {'Key', 'RightValue'});
%=============================================================================
J = innerjoin(L, R, 'Keys', 'Key');
assert_isequal(J.Key, [2; 3]);
assert_isequal(J.LeftValue, {'B'; 'C'});
assert_isequal(J.RightValue, [20; 30]);
%=============================================================================
J = join(L, R, 'Keys', 'Key');
assert_isequal(J.Key, [2; 3]);
%=============================================================================
O = outerjoin(L, R, 'Keys', 'Key');
assert_isequal(O.Key_L, [1; 2; 3; NaN]);
assert_isequal(O.Key_R, [NaN; 2; 3; 4]);
assert_istrue(isnan(O.RightValue(1)));
assert_isequal(O.RightValue(2:4), [20; 30; 40]);
%=============================================================================
O = outerjoin(L, R, 'Keys', 'Key', 'MergeKeys', true);
assert_isequal(O.Key, [1; 2; 3; 4]);
%=============================================================================
