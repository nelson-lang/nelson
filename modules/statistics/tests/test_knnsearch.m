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
%=============================================================================
asserts.isequal(nargin('knnsearch'), -1);
asserts.isequal(nargout('knnsearch'), 2);
%=============================================================================
X = [0 0; 1 0; 0 2; 4 4];
Y = [0 1; 3 4];
idx = knnsearch(X, Y);
asserts.isequal(idx, [1; 4]);
%=============================================================================
[idx, D] = knnsearch(X, Y, 'K', 2);
asserts.isequal(idx, [1 3; 4 3]);
asserts.isapprox(D, [1 1; 1 sqrt(13)], 1e-12);
%=============================================================================
[idx, D] = knnsearch(X, Y, 'K', 2, 'Distance', 'cityblock');
asserts.isequal(idx, [1 3; 4 3]);
asserts.isequal(D, [1 1; 1 5]);
%=============================================================================
[idx, D] = knnsearch(X, Y, 'K', 2, 'Distance', 'minkowski', 'P', 1);
asserts.isequal(idx, [1 3; 4 3]);
asserts.isequal(D, [1 1; 1 5]);
%=============================================================================
[idx, D] = knnsearch(X, Y, 'Distance', 'seuclidean', 'Scale', [1 2]);
asserts.isequal(idx, [1; 4]);
asserts.isapprox(D, [0.5; 1], 1e-12);
%=============================================================================
X = [0; 2; 4];
Y = [1; 3];
[idx, D] = knnsearch(X, Y, 'K', 1, 'IncludeTies', true);
asserts.isequal(size(idx), [2 1]);
asserts.isequal(idx{1}, [1 2]);
asserts.isequal(idx{2}, [2 3]);
asserts.isequal(D{1}, [1 1]);
asserts.isequal(D{2}, [1 1]);
%=============================================================================
idx = knnsearch([0; 1; 2], [0.2; 1.8], 'NSMethod', 'exhaustive', 'SortIndices', false);
asserts.isequal(idx, [1; 3]);
%=============================================================================
asserts.checkerror('knnsearch()', _('Not enough input arguments.'));
asserts.checkerror('knnsearch([1; 2], [1], ''K'')', _('Name-value arguments must be paired.'));
asserts.checkerror('knnsearch({1}, [1])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('knnsearch([1 2], [1])', sprintf(_('X and Y must have %s.'), 'the same number of columns'));
asserts.checkerror('knnsearch([1; 2], [1], ''K'', 3)', _('K must not exceed the number of rows in X.'));
asserts.checkerror('knnsearch([1; 2], [1], ''K'', 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('knnsearch([1; 2], [1], ''Distance'', ''bad'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('knnsearch([1; 2], [1], ''Distance'', ''minkowski'', ''P'', 0)', sprintf(_('%s must be a positive scalar.'), 'P'));
asserts.checkerror('knnsearch([1; 2], [1], ''NSMethod'', ''bad'')', sprintf(_('Invalid %s value.'), 'NSMethod'));
asserts.checkerror('knnsearch([1; 2], [1], ''IncludeTies'', 2)', sprintf(_('%s must be true or false.'), 'IncludeTies'));
%=============================================================================
