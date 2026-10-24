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
asserts.isequal(nargin('rangesearch'), -1);
asserts.isequal(nargout('rangesearch'), 2);
%=============================================================================
X = [0 0; 1 0; 0 2; 4 4];
Y = [0 1; 3 4; 10 10];
[idx, D] = rangesearch(X, Y, 1.1);
asserts.isequal(size(idx), [3 1]);
asserts.isequal(idx{1}, [1 3]);
asserts.isequal(idx{2}, 4);
asserts.isequal(idx{3}, zeros(1, 0));
asserts.isequal(D{1}, [1 1]);
asserts.isequal(D{2}, 1);
asserts.isequal(D{3}, zeros(1, 0));
%=============================================================================
[idx, D] = rangesearch(X, Y(1:2, :), 5, 'Distance', 'cityblock');
asserts.isequal(idx{1}, [1 3 2]);
asserts.isequal(idx{2}, [4 3]);
asserts.isequal(D{1}, [1 1 2]);
asserts.isequal(D{2}, [1 5]);
%=============================================================================
[idx, D] = rangesearch(X, Y(1, :), 1.1, 'Distance', 'minkowski', 'P', 1);
asserts.isequal(idx{1}, [1 3]);
asserts.isequal(D{1}, [1 1]);
%=============================================================================
[idx, D] = rangesearch(X, Y(1, :), 0.5, 'Distance', 'seuclidean', 'Scale', [1 2]);
asserts.isequal(idx{1}, [1 3]);
asserts.isequal(D{1}, [0.5 0.5]);
%=============================================================================
[idx, D] = rangesearch([0; 1; 2], [1], 1, 'SortIndices', false, 'NSMethod', 'exhaustive');
asserts.isequal(idx{1}, [1 2 3]);
asserts.isequal(D{1}, [1 0 1]);
%=============================================================================
asserts.checkerror('rangesearch()', _('Not enough input arguments.'));
asserts.checkerror('rangesearch([1; 2], [1], 1, ''Distance'')', _('Name-value arguments must be paired.'));
asserts.checkerror('rangesearch({1}, [1], 1)', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('rangesearch([1 2], [1], 1)', sprintf(_('X and Y must have %s.'), 'the same number of columns'));
asserts.checkerror('rangesearch([1; 2], [1], -1)', sprintf(_('%s must be a nonnegative scalar.'), 'R'));
asserts.checkerror('rangesearch([1; 2], [1], Inf)', sprintf(_('%s must be a nonnegative scalar.'), 'R'));
asserts.checkerror('rangesearch([1; 2], [1], 1, ''Distance'', ''bad'')', sprintf(_('Invalid %s value.'), 'Distance'));
asserts.checkerror('rangesearch([1; 2], [1], 1, ''Distance'', ''minkowski'', ''P'', 0)', sprintf(_('%s must be a positive scalar.'), 'P'));
asserts.checkerror('rangesearch([1; 2], [1], 1, ''NSMethod'', ''bad'')', sprintf(_('Invalid %s value.'), 'NSMethod'));
asserts.checkerror('rangesearch([1; 2], [1], 1, ''SortIndices'', 2)', sprintf(_('%s must be true or false.'), 'SortIndices'));
%=============================================================================
