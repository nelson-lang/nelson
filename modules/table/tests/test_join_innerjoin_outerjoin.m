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
asserts.isequal(J.Key, [2; 3]);
asserts.isequal(J.LeftValue, {'B'; 'C'});
asserts.isequal(J.RightValue, [20; 30]);
%=============================================================================
% join requires the right key to cover every left key; Key = 1 is uncovered.
msg = _('Right-table key values must cover every left-table key value.');
assert_checkerror('join(L, R, ''Keys'', ''Key'')', msg);
%=============================================================================
O = outerjoin(L, R, 'Keys', 'Key');
asserts.isequal(O.Key_L, [1; 2; 3; NaN]);
asserts.isequal(O.Key_R, [NaN; 2; 3; 4]);
asserts.istrue(isnan(O.RightValue(1)));
asserts.isequal(O.RightValue(2:4), [20; 30; 40]);
%=============================================================================
O = outerjoin(L, R, 'Keys', 'Key', 'MergeKeys', true);
asserts.isequal(O.Key, [1; 2; 3; 4]);
%=============================================================================
