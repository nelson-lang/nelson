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
asserts.isequal(nargin('vartest'), -1);
asserts.isequal(nargout('vartest'), 4);
%=============================================================================
v = [4.5 4.8 5.1 5.4 5.7 6.0];
[h, p, ci, stats] = vartest(v, 0.4);
asserts.isequal(h, false);
asserts.isapprox(p, 0.883101737846991, 1e-12);
asserts.isapprox(ci, [0.122735223476507; 1.89482434369920], 1e-12);
asserts.isapprox(stats.chisqstat, 3.93750000000004, 1e-12);
asserts.isequal(stats.df, 5);
%=============================================================================
[h, p, ci, stats] = vartest(v, 0.2, 'Tail', 'right', 'Alpha', 0.1);
asserts.isequal(h, false);
asserts.isapprox(p, 0.163260506206144, 1e-12);
asserts.isapprox(ci(1), 0.170521777914119, 1e-12);
asserts.istrue(isinf(ci(2)) && ci(2) > 0);
asserts.isapprox(stats.chisqstat, 7.87500000000009, 1e-12);
asserts.isequal(stats.df, 5);
%=============================================================================
V = [1 2 3; 2 NaN 4; 3 5 5; 4 6 6];
[h, p, ci, stats] = vartest(V, 1.5, 'Dim', 1);
asserts.isequal(h, [false false false]);
asserts.isapprox(p, [0.686060292276489 0.111275996555686 0.686060292276489], 1e-12);
asserts.isapprox(ci, [0.534850677349353 1.17470179962120 0.534850677349353; ...
                     23.1701079801375 171.157524222565 23.1701079801375], 1e-12);
asserts.isapprox(stats.chisqstat, [3.33333333333333 5.77777777777778 3.33333333333333], 1e-12);
asserts.isequal(stats.df, [3 2 3]);
%=============================================================================
A = reshape(1:24, [2 3 4]);
[h, p, ci, stats] = vartest(A, 20, 'Dim', 3, 'Tail', 'left');
asserts.isequal(size(h), [2 3]);
asserts.isequal(size(p), [2 3]);
asserts.isequal(size(ci), [2 6]);
asserts.isequal(size(stats.chisqstat), [2 3]);
asserts.isequal(size(stats.df), [2 3]);
asserts.isequal(h, false(2, 3));
asserts.isapprox(p, 0.970709113465106 * ones(2, 3), 1e-12);
asserts.isequal(ci(1, :), zeros(1, 6));
asserts.isapprox(ci(2, :), 511.586993865513 * ones(1, 6), 1e-12);
asserts.isequal(stats.chisqstat, 9 * ones(2, 3));
asserts.isequal(stats.df, 3 * ones(2, 3));
%=============================================================================
[h, p, ci, stats] = vartest([1 1 1], 0);
asserts.isequal(h, false);
asserts.isequal(p, 1);
asserts.isequal(ci, [0; 0]);
asserts.istrue(isnan(stats.chisqstat));
asserts.isequal(stats.df, 2);
%=============================================================================
[h, p, ci, stats] = vartest([1 NaN], 1);
asserts.isequal(h, false);
asserts.istrue(isnan(p));
asserts.istrue(all(isnan(ci)));
asserts.istrue(isnan(stats.chisqstat));
asserts.istrue(isnan(stats.df));
%=============================================================================
asserts.checkerror('vartest([1 2 3])', _('Not enough input arguments.'));
asserts.checkerror('vartest([1 2 3], -1)', sprintf(_('%s must be nonnegative.'), 'V'));
asserts.checkerror('vartest([1 2 3], 1, ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('vartest([1 2 3], 1, ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('vartest([1 2 3], 1, ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('vartest([1 2 3], 1, ''Tail'', ''bad'')', _('TAIL must be ''both'', ''right'', or ''left''.'));
asserts.checkerror('vartest([1 2 3], 1, ''Dim'', 0)', sprintf(_('Dimension argument must be %s.'), 'a positive integer scalar within indexing range'));
%=============================================================================
