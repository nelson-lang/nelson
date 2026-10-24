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
asserts.isequal(nargin('ttest'), -1);
asserts.isequal(nargout('ttest'), 4);
%=============================================================================
x = [2 4 5 6 9];
[h, p, ci, stats] = ttest(x, 4);
asserts.isequal(h, false);
asserts.isapprox(p, 0.358456344622964, 1e-12);
asserts.isapprox(ci, [-2.01396757073199; 4.41396757073199], 1e-12);
asserts.isapprox(stats.tstat, 1.03664211069763, 1e-12);
asserts.isequal(stats.df, 4);
asserts.isapprox(stats.sd, 2.58843582110896, 1e-12);
%=============================================================================
X = [1 4 7; 2 5 8; 3 6 9];
[h, p, ci, stats] = ttest(X, 0, 'Dim', 1);
asserts.isequal(h, [false true true]);
asserts.isapprox(p, [0.0741799002274486 0.0130724575603465 0.00516799325238604], 1e-12);
asserts.isapprox(ci, [-0.484137711750327 2.51586228824967 5.51586228824967; ...
                      4.48413771175033 7.48413771175033 10.4841377117503], 1e-12);
asserts.isapprox(stats.tstat, [3.46410161513775 8.66025403784439 13.856406460551], 1e-12);
%=============================================================================
[h, p] = ttest([4 6 7 9 10], [3 5 7 7 8], 'Tail', 'right');
asserts.isequal(h, true);
asserts.isapprox(p, 0.0163389616684015, 1e-12);
%=============================================================================
[h, p, ci, stats] = ttest(X, 4, 'Dim', 2);
asserts.isequal(h, [false; false; false]);
asserts.isapprox(p, [1; 0.622035526990773; 0.367544467966324], 1e-12);
asserts.isapprox(ci, [-7.45241313525098 -6.45241313525098 -5.45241313525098; ...
                      7.45241313525098 8.45241313525098 9.45241313525098], 1e-12);
asserts.isapprox(stats.tstat, [0; 0.577350269189626; 1.15470053837925], 1e-12);
asserts.isequal(stats.df, [2; 2; 2]);
asserts.isequal(stats.sd, [3; 3; 3]);
%=============================================================================
A = reshape(1:24, [2 3 4]);
[h, p, ci, stats] = ttest(A, 10, 'Dim', 3);
asserts.isequal(size(h), [2 3]);
asserts.isequal(size(p), [2 3]);
asserts.isequal(size(ci), [2 6]);
asserts.isequal(size(stats.tstat), [2 3]);
asserts.isequal(size(stats.df), [2 3]);
asserts.isequal(size(stats.sd), [2 3]);
asserts.isequal(h, false(2, 3));
asserts.isapprox(p, [1 0.641229143482416 0.377662487809741; ...
                     0.812953483523856 0.495025346059711 0.287189741069735], 1e-12);
asserts.isapprox(ci, [-12.3255615405631 -11.3255615405631 -10.3255615405631 -9.32556154056311 -8.32556154056311 -7.32556154056311; ...
                       12.3255615405631 13.3255615405631 14.3255615405631 15.3255615405631 16.3255615405631 17.3255615405631], 1e-12);
asserts.isapprox(stats.tstat, [0 0.516397779494322 1.03279555898864; ...
                              0.258198889747161 0.774596669241483 1.29099444873581], 1e-12);
asserts.isequal(stats.df, 3 * ones(2, 3));
asserts.isapprox(stats.sd, 7.74596669241483 * ones(2, 3), 1e-12);
%=============================================================================
B = A;
B(1, 1, 2) = NaN;
[h, p, ci, stats] = ttest(B, A - 1, 'Dim', 3, 'Tail', 'right');
asserts.isequal(size(h), [2 3]);
asserts.isequal(size(ci), [2 6]);
asserts.isequal(h, true(2, 3));
asserts.isequal(p, zeros(2, 3));
asserts.isequal(ci(1, :), ones(1, 6));
asserts.istrue(all(isinf(ci(2, :))));
asserts.istrue(all(isinf(stats.tstat(:))));
asserts.isequal(stats.df, [2 3 3; 3 3 3]);
asserts.isequal(stats.sd, zeros(2, 3));
%=============================================================================
[h, p, ci, stats] = ttest([1 2 3], 4, 'Tail', 'left');
asserts.isequal(h, true);
asserts.isapprox(p, 0.037089950113724, 1e-12);
asserts.istrue(isinf(ci(1)) && ci(1) < 0);
asserts.isapprox(ci(2), -0.314145539152953, 1e-12);
asserts.isapprox(stats.tstat, -3.46410161513775, 1e-12);
%=============================================================================
[h, p] = ttest([1 NaN 2 3], 0);
asserts.isequal(h, false);
asserts.isapprox(p, 0.0741799002274486, 1e-12);
%=============================================================================
[h, p, ci, stats] = ttest([1 1 1], 0);
asserts.isequal(h, true);
asserts.isequal(p, 0);
asserts.isequal(ci, [1; 1]);
asserts.istrue(isinf(stats.tstat));
%=============================================================================
[h, p] = ttest([1 1 1], 1);
asserts.isequal(h, false);
asserts.istrue(isnan(p));
%=============================================================================
asserts.checkerror('ttest([1 2 3], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('ttest([1 2 3], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('ttest([1 2 3], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('ttest([1 2 3], ''Tail'', ''bad'')', _('TAIL must be ''both'', ''right'', or ''left''.'));
asserts.checkerror('ttest([1 2], [1 2 3])', sprintf(_('X and Y must have %s.'), 'the same size'));
asserts.checkerror('ttest([1 2 3], ''Dim'', 0)', sprintf(_('Dimension argument must be %s.'), 'a positive integer scalar within indexing range'));
%=============================================================================
