%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('grpstats'), -4);
asserts.isequal(nargout('grpstats'), -1);
%=============================================================================
X = [1 10; 2 20; 3 30; 4 40];
g = [1 1 2 2]';
M = grpstats(X, g);
asserts.isequal(M, [1.5 15; 3.5 35]);
%=============================================================================
[M, S, N, G] = grpstats(X, g, {'mean', 'std', 'numel', 'gname'});
asserts.isequal(M, [1.5 15; 3.5 35]);
asserts.isapprox(S, [0.707106781186548 7.071067811865475; ...
                    0.707106781186548 7.071067811865475], 1e-12);
asserts.isequal(N, [2 2; 2 2]);
asserts.isequal(G, {'1'; '2'});
%=============================================================================
R = grpstats(X, g, {'min', 'max', 'range', 'median', 'mode'});
asserts.isequal(R, [1 10; 3 30]);
%=============================================================================
R = grpstats(X, g, @(v) sum(v, 1));
asserts.isequal(R, [3 30; 7 70]);
%=============================================================================
[M, G] = grpstats((1:4)', {["a"; "a"; "b"; "b"], [1; 1; 1; 2]}, {'mean', 'gname'});
asserts.isequal(M, [1.5; 3; 4]);
asserts.isequal(G, {'a', '1'; 'b', '1'; 'b', '2'});
%=============================================================================
CI = grpstats(X, g, 'meanci', 'Alpha', 0.10);
REF = [0.677573186524264 6.775731865242642 2.322426813475736 23.224268134757359; ...
       2.677573186524264 26.775731865242641 4.322426813475736 43.224268134757359];
asserts.isapprox(CI, REF, 1e-12);
%=============================================================================
T = table({'a'; 'a'; 'b'}, [1; 3; 5], [10; 20; 30], 'VariableNames', {'G', 'X', 'Y'});
S = grpstats(T, 'G');
asserts.isequal(S.GroupCount, [2; 1]);
asserts.isequal(S.mean_X, [2; 5]);
asserts.isequal(S.mean_Y, [15; 30]);
%=============================================================================
S = grpstats(T, 'G', {'mean', 'numel'}, 'DataVars', 'X');
asserts.isequal(S.mean_X, [2; 5]);
asserts.isequal(S.numel_X, [2; 1]);
%=============================================================================
asserts.checkerror('grpstats()', _('Not enough input arguments.'));
asserts.checkerror('grpstats([1;2], [1;2;3])', sprintf(_('%s must have the same number of rows.'), 'X and group'));
asserts.checkerror('grpstats([1;2], [1;1], ''bad'')', sprintf(_('Unsupported %s.'), 'summary statistic'));
asserts.checkerror('grpstats([1;2], [1;1], ''mean'', ''Alpha'', 2)', _('Alpha must be a scalar between 0 and 1.'));
%=============================================================================
