%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
L = table([1; 2], [10; 20], 'VariableNames', {'Key', 'LeftValue'});
R = table([2; 3], [200; 300], 'VariableNames', {'Key', 'RightValue'});
J = outerjoin(L, R, 'Keys', 'Key');
asserts.isequal(J.Key_L, [1; 2; NaN]);
asserts.isequal(J.Key_R, [NaN; 2; 3]);
%=============================================================================
J = outerjoin(L, R, 'Keys', 'Key', 'MergeKeys', true);
asserts.isequal(J.Key, [1; 2; 3]);
%=============================================================================
% Missing text in a cellstr column is filled with '', not [].
M = table([101; 103], [18.2; 19.8], 'VariableNames', {'Station', 'Temperature'});
A = table([101; 104], {'North'; 'South'}, 'VariableNames', {'Station', 'Area'});
C = outerjoin(M, A, 'Keys', 'Station', 'MergeKeys', true);
assert_isequal(C.Area, {'North'; ''; 'South'});
assert_istrue(iscellstr(C.Area));
%=============================================================================
