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
asserts.isequal(nargin('corr'), -2);
asserts.isequal(nargout('corr'), 2);
%=============================================================================
X = [1 2 3; 2 4 1; 3 NaN 2; 4 8 4; 5 10 5];
R = corr(X);
REF = [1 NaN 0.7; NaN NaN NaN; 0.7 NaN 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = corr(X, 'Rows', 'pairwise');
REF = [1 1 0.7; 1 1 0.748331477354788; 0.7 0.748331477354788 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = corr(X, 'Rows', 'complete');
REF = [1 1 0.748331477354788; 1 1 0.748331477354788; ...
       0.748331477354788 0.748331477354788 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = corr(X, 'Type', 'Spearman', 'Rows', 'pairwise');
REF = [1 0.989949493661166 0.7; 0.989949493661166 1 0.680336051416609; ...
       0.7 0.680336051416609 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = corr(X, 'Type', 'Kendall', 'Rows', 'pairwise');
REF = [1 1 0.6; 1 1 0.666666666666667; 0.6 0.666666666666667 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
Y = [5 1; 4 2; 3 3; 2 4; 1 5];
[R, P] = corr(X(:, [1 3]), Y, 'Tail', 'right');
asserts.isapprox(R, [-1 1; -0.7 0.7], 1e-12);
asserts.isapprox(P, [1 0; 0.905939797812906 0.094060202187094], 1e-12);
[R, P] = corr(X(:, [1 3]), Y, 'Tail', 'left');
asserts.isapprox(R, [-1 1; -0.7 0.7], 1e-12);
asserts.isapprox(P, [0 1; 0.094060202187094 0.905939797812906], 1e-12);
%=============================================================================
[R, P] = corr(X(:, [1 3]), Y, 'Weights', [1 2 3 4 5]);
asserts.isapprox(R, [-1 1; -0.857492925712544 0.857492925712544], 1e-12);
asserts.istrue(all(isnan(P(:))));
%=============================================================================
asserts.isequal(corr([1 2 3]), NaN(3, 3));
asserts.istrue(isempty(corr([])));
asserts.isequal(corr(zeros(0, 3)), NaN(3, 3));
asserts.isequal(class(corr(single([1; 2; 3]), single([3; 2; 1]))), 'double');
%=============================================================================
asserts.checkerror('corr()', _('Not enough input arguments.'));
asserts.checkerror('corr({1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('corr([1; 2], {1})', sprintf(_('%s must be a real numeric matrix.'), 'Y'));
asserts.checkerror('corr([1; 2], [1 2])', sprintf(_('%s must have the same number of rows.'), 'X and Y'));
asserts.checkerror('corr([1; 2], ''Type'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TYPE'));
asserts.checkerror('corr([1; 2], ''Rows'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'ROWS'));
asserts.checkerror('corr([1; 2], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
asserts.checkerror('corr([1; 2], ''Weights'', [1 -1])', sprintf(_('%s must contain nonnegative finite values.'), 'Weights'));
%=============================================================================
