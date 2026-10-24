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
asserts.isequal(nargin('vartest2'), -1);
asserts.isequal(nargout('vartest2'), 4);
%=============================================================================
x = [4.5 4.8 5.1 5.4 5.7 6.0];
y = [3.9 4.1 4.2 4.4 4.5];
[h, p, ci, stats] = vartest2(x, y);
asserts.isequal(h, false);
asserts.isapprox(p, 0.122655020131751, 1e-12);
asserts.isapprox(ci, [0.590136474144896; 40.8277896780601], 1e-12);
asserts.isapprox(stats.fstat, 5.52631578947388, 1e-12);
asserts.isequal(stats.df1, 5);
asserts.isequal(stats.df2, 4);
%=============================================================================
[h, p, ci, stats] = vartest2(x, y, 'Tail', 'right', 'Alpha', 0.1);
asserts.isequal(h, true);
asserts.isapprox(p, 0.061327510065875, 1e-12);
asserts.isapprox(ci(1), 1.36432734563439, 1e-12);
asserts.istrue(isinf(ci(2)) && ci(2) > 0);
asserts.isapprox(stats.fstat, 5.52631578947388, 1e-12);
asserts.isequal(stats.df1, 5);
asserts.isequal(stats.df2, 4);
%=============================================================================
X = [1 2 3; 2 NaN 4; 3 5 5; 4 6 6];
Y = [1 1 2; 1 2 3; 2 3 4; 2 4 5; 3 5 6];
[h, p, ci, stats] = vartest2(X, Y, 'Dim', 1);
asserts.isequal(h, [false false false]);
asserts.isapprox(p, [0.420826885551409 0.573979591836735 0.769800358919501], 1e-12);
asserts.isapprox(ci, [0.238591543525191 0.162767896007484 0.066805632187053; ...
                     35.9547117429665 68.0305906074280 10.0673192880306], 1e-12);
asserts.isapprox(stats.fstat, [2.38095238095238 1.73333333333333 0.666666666666667], 1e-12);
asserts.isequal(stats.df1, [3 2 3]);
asserts.isequal(stats.df2, [4 4 4]);
%=============================================================================
A = reshape(1:24, [2 3 4]);
B = A + reshape(mod(1:24, 5), [2 3 4]);
[h, p, ci, stats] = vartest2(A, B, 'Dim', 3, 'Tail', 'left');
asserts.isequal(size(h), [2 3]);
asserts.isequal(size(p), [2 3]);
asserts.isequal(size(ci), [2 6]);
asserts.isequal(size(stats.fstat), [2 3]);
asserts.isequal(h, false(2, 3));
asserts.isapprox(p, [0.403015545091910 0.602233656236609 0.403015545091910; ...
                    0.539909350248268 0.539909350248268 0.403015545091910], 1e-12);
asserts.isequal(ci(1, :), zeros(1, 6));
asserts.isapprox(ci(2, :), [6.81548190843292 10.5183815279752 12.8445620582005 ...
                           10.5183815279752 6.81548190843292 6.81548190843292], 1e-12);
asserts.isapprox(stats.fstat, [0.734693877551020 1.38461538461538 0.734693877551020; ...
                              1.13385826771654 1.13385826771654 0.734693877551020], 1e-12);
asserts.isequal(stats.df1, 3 * ones(2, 3));
asserts.isequal(stats.df2, 3 * ones(2, 3));
%=============================================================================
[h, p, ci, stats] = vartest2([1 1 1], [1 1 1]);
asserts.isequal(h, false);
asserts.isequal(p, 1);
asserts.istrue(all(isnan(ci)));
asserts.istrue(isnan(stats.fstat));
asserts.isequal(stats.df1, 2);
asserts.isequal(stats.df2, 2);
%=============================================================================
[h, p, ci, stats] = vartest2([1 NaN], [1 2 3]);
asserts.isequal(h, false);
asserts.istrue(isnan(p));
asserts.istrue(all(isnan(ci)));
asserts.istrue(isnan(stats.fstat));
asserts.isequal(stats.df1, 0);
asserts.isequal(stats.df2, 2);
%=============================================================================
asserts.checkerror('vartest2([1 2; 3 4], [1 2 3], ''Dim'', 1)', sprintf(_('X and Y must have %s.'), 'compatible sizes'));
asserts.checkerror('vartest2([1 2 3], [1 2 3], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('vartest2([1 2 3], [1 2 3], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('vartest2([1 2 3], [1 2 3], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('vartest2([1 2 3], [1 2 3], ''Tail'', ''bad'')', _('TAIL must be ''both'', ''right'', or ''left''.'));
asserts.checkerror('vartest2([1 2 3], [1 2 3], ''Dim'', 0)', sprintf(_('Dimension argument must be %s.'), 'a positive integer scalar within indexing range'));
%=============================================================================
