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
asserts.isequal(nargin('kmedoids'), -1);
asserts.isequal(nargout('kmedoids'), 6);
%=============================================================================
X = [0; 1; 10; 11];
[idx, C, sumd, D, midx, info] = kmedoids(X, 2, 'Start', [1; 3]);
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isequal(C, [0; 10]);
asserts.isequal(sumd, [1; 1]);
asserts.isequal(size(D), [4 2]);
asserts.isequal(midx, [1; 3]);
asserts.istrue(isfield(info, 'Iterations'));
asserts.istrue(isfield(info, 'BestSum'));
%=============================================================================
X2 = [0 0; 0 1; 5 5; 5 6];
[idx, C] = kmedoids(X2, 2, 'Start', [0 0; 5 5], 'Distance', 'cityblock');
asserts.isequal(idx, [1; 1; 2; 2]);
asserts.isequal(C, [0 0; 5 5]);
%=============================================================================
S = zeros(2, 2, 2);
S(:, :, 1) = [0 0; 5 5];
S(:, :, 2) = [5 5; 0 0];
[idx, C] = kmedoids(X2, 2, 'Start', S, 'Replicates', 2);
asserts.isequal(size(idx), [4 1]);
asserts.isequal(size(C), [2 2]);
%=============================================================================
rng(11);
idx1 = kmedoids(X, 2, 'Replicates', 2);
rng(11);
idx2 = kmedoids(X, 2, 'Replicates', 2);
asserts.isequal(idx1, idx2);
%=============================================================================
opts = statset('kmedoids');
asserts.isequal(opts.Display, 'off');
[idx, C] = kmedoids(X, 2, 'Start', [1; 3], 'Options', opts, 'MaxIter', 2);
asserts.isequal(size(idx), [4 1]);
asserts.isequal(size(C), [2 1]);
%=============================================================================
asserts.checkerror('kmedoids()', _('Not enough input arguments.'));
asserts.checkerror('kmedoids([], 1)', sprintf(_('%s must be a nonempty numeric matrix.'), 'X'));
asserts.checkerror('kmedoids([1; 2], 3)', _('K must not exceed the number of rows in X.'));
asserts.checkerror('kmedoids([1; 2], 0)', sprintf(_('%s must be a positive integer scalar.'), 'K'));
asserts.checkerror('kmedoids([1; 2], 1, ''Distance'')', _('Name-value arguments must be paired.'));
asserts.checkerror('kmedoids([1; 2], 1, ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('kmedoids([1; 2], 1, ''Display'', ''bad'')', _('Display must be off, final, or iter.'));
asserts.checkerror('kmedoids([1; 2], 1, ''Options'', 1)', _('Options must be a structure.'));
asserts.checkerror('kmedoids([1; 2], 1, ''Start'', ''bad'')', sprintf(_('Invalid %s value.'), 'Start'));
asserts.checkerror('kmedoids([1; 2], 1, ''Start'', 3)', sprintf(_('Invalid %s value.'), 'Start'));
asserts.checkerror('kmedoids([1; 2], 1, ''Start'', 1.5)', sprintf(_('Invalid %s value.'), 'Start'));
asserts.checkerror('kmedoids([1; 2], 1, ''Replicates'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'Replicates'));
asserts.checkerror('kmedoids([1; 2], 1, ''MaxIter'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MaxIter'));
%=============================================================================
