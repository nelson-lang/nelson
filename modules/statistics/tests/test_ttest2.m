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
asserts.isequal(nargin('ttest2'), -1);
asserts.isequal(nargout('ttest2'), 4);
%=============================================================================
[h, p, ci, stats] = ttest2([1 2 3 4], [2 3 4 5]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.315333596201230, 1e-12);
asserts.isapprox(ci, [-3.23371469516470; 1.23371469516470], 1e-12);
asserts.isapprox(stats.tstat, -1.09544511501033, 1e-12);
asserts.isequal(stats.df, 6);
asserts.isapprox(stats.sd, 1.29099444873581, 1e-12);
%=============================================================================
[h, p, ci, stats] = ttest2([10 11 13 15 18], [7 8 8 9], 'Vartype', 'unequal', 'Tail', 'right', 'Alpha', 0.1);
asserts.isequal(h, true);
asserts.isapprox(p, 0.008693040249151, 1e-12);
asserts.isapprox(ci(1), 3.17119983133200, 1e-12);
asserts.istrue(isinf(ci(2)) && ci(2) > 0);
asserts.isapprox(stats.tstat, 3.61881311803104, 1e-12);
asserts.isapprox(stats.df, 4.63299681944190, 1e-12);
asserts.isapprox(stats.sd, [3.20936130717624; 0.816496580927726], 1e-12);
%=============================================================================
X = [1 4 7; 2 5 8; 3 NaN 9; 4 7 10];
Y = [1 2 3; 2 3 4; 2 4 5; 3 5 6; 4 6 7];
[h, p, ci, stats] = ttest2(X, Y, 'Dim', 1);
asserts.isequal(h, [false false true]);
asserts.isapprox(p, [0.905187744664214 0.287208140444222 0.009167452294336], 1e-12);
asserts.isapprox(ci, [-1.81478255524534 -1.46054682910659 1.17798508077452; ...
                      2.01478255524534 4.12721349577325 5.82201491922548], 1e-12);
asserts.isapprox(stats.tstat, [0.123493095605825 1.16774841624228 3.56422554052121], 1e-12);
asserts.isequal(stats.df, [7 6 7]);
asserts.isapprox(stats.sd, [1.20712172424444 1.56347191994114 1.46385010942280], 1e-12);
%=============================================================================
A = reshape(1:48, [2 3 4 2]);
B = A + reshape(mod(1:48, 5), [2 3 4 2]);
[h, p, ci, stats] = ttest2(A, B, 'Dim', 3, 'Vartype', 'unequal', 'Tail', 'left');
asserts.isequal(size(h), [2 3 1 2]);
asserts.isequal(size(p), [2 3 1 2]);
asserts.isequal(size(ci), [2 12]);
asserts.isequal(size(stats.tstat), [2 3 1 2]);
asserts.isequal(size(stats.df), [2 3 1 2]);
asserts.isequal(size(stats.sd), [2 12]);
asserts.isequal(h, false(2, 3, 1, 2));
asserts.isapprox(p(:, :, 1, 1), [0.344691674683048 0.353949144881500 0.404802279532035; ...
                                0.343380638432433 0.376555049898787 0.344691674683048], 1e-12);
asserts.istrue(all(isinf(ci(1, :)) & ci(1, :) < 0));
asserts.isapprox(ci(2, 1:4), [9.11286029136174 8.08161639787162 7.92271095618497 8.58161639787162], 1e-12);
asserts.isapprox(stats.tstat(:, :, 1, 2), [-0.252050415125042 -0.423480570224814 -0.329373776841522; ...
                                          -0.420084025208403 -0.393495501470372 -0.252050415125042], 1e-12);
asserts.isapprox(stats.df(1, :, 1, 1), [5.86286177982148 5.84787018255578 5.86286177982148], 1e-12);
asserts.isapprox(stats.sd(:, 1:3), [7.74596669241483 7.74596669241483 7.74596669241483; ...
                                   9.03696114115064 7.27438428093173 6.58280588604383], 1e-12);
%=============================================================================
[h, p, ci, stats] = ttest2(single([1 2 3 NaN]), single([1 2 2 2]));
asserts.isequal(h, false);
asserts.isapprox(p, 0.677380178349306, 1e-12);
asserts.isapprox(ci, [-1.20603285931207; 1.70603285931207], 1e-12);
asserts.isapprox(stats.tstat, 0.441367414752375, 1e-12);
asserts.isequal(stats.df, 5);
asserts.isapprox(stats.sd, 0.741619848709566, 1e-12);
%=============================================================================
[h, p, ci, stats] = ttest2([1 1 1], [1 1 1]);
asserts.isequal(h, false);
asserts.istrue(isnan(p));
asserts.istrue(all(isnan(ci)));
asserts.istrue(isnan(stats.tstat));
asserts.isequal(stats.df, 4);
asserts.isequal(stats.sd, 0);
%=============================================================================
[h, p, ci, stats] = ttest2([], []);
asserts.isequal(size(h), [1 0]);
asserts.isequal(size(p), [1 0]);
asserts.isequal(size(ci), [2 0]);
asserts.isequal(size(stats.tstat), [1 0]);
asserts.isequal(size(stats.df), [1 0]);
asserts.isequal(size(stats.sd), [1 0]);
%=============================================================================
asserts.checkerror('ttest2([1 2 3], [1 2], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('ttest2([1 2 3], [1 2], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('ttest2([1 2 3], [1 2], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('ttest2([1 2 3], [1 2], ''Tail'', ''bad'')', _('TAIL must be ''both'', ''right'', or ''left''.'));
asserts.checkerror('ttest2([1 2 3], [1 2], ''Vartype'', ''bad'')', _('VARTYPE must be ''equal'' or ''unequal''.'));
asserts.checkerror('ttest2([1 2; 3 4], [1 2 3; 4 5 6], ''Dim'', 1)', sprintf(_('X and Y must have %s.'), 'compatible sizes'));
%=============================================================================
