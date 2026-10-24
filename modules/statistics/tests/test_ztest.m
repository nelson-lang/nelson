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
asserts.isequal(nargin('ztest'), -1);
asserts.isequal(nargout('ztest'), 4);
%=============================================================================
x = [72 75 77 70 74 76];
[h, p, ci, zval] = ztest(x, 75, 10);
asserts.isequal(h, false);
asserts.isapprox(p, 0.806495940507340, 1e-12);
asserts.isapprox(ci, [65.9984805394078; 82.0015194605922], 1e-12);
asserts.isapprox(zval, -0.244948974278318, 1e-12);
%=============================================================================
[h, p, ci, zval] = ztest(x, 72, 10, 'Tail', 'right', 'Alpha', 0.1);
asserts.isequal(h, false);
asserts.isapprox(p, 0.312103057383203, 1e-12);
asserts.isapprox(ci(1), 68.7680876422513, 1e-12);
asserts.istrue(isinf(ci(2)) && ci(2) > 0);
asserts.isapprox(zval, 0.489897948556636, 1e-12);
%=============================================================================
X = [1 4 7; 2 NaN 8; 3 6 9];
[h, p, ci, zval] = ztest(X, 0, 2, 'Dim', 1);
asserts.isequal(h, [false true true]);
asserts.isapprox(p, [0.083264516663550 0.000406952017445 0.000000000004262], 1e-12);
asserts.isapprox(ci, [-0.263171468152346 2.22819235130064 5.73682853184766; ...
                      4.26317146815235 7.77180764869936 10.2631714681523], 1e-12);
asserts.isapprox(zval, [1.73205080756888 3.53553390593274 6.92820323027551], 1e-12);
%=============================================================================
A = reshape(1:24, [2 3 4]);
[h, p, ci, zval] = ztest(A, 10, 5, 'Dim', 3, 'Tail', 'left');
asserts.isequal(size(h), [2 3]);
asserts.isequal(size(p), [2 3]);
asserts.isequal(size(ci), [2 6]);
asserts.isequal(size(zval), [2 3]);
asserts.isequal(h, false(2, 3));
asserts.isapprox(p, [0.5 0.788144601416603 0.945200708300442; ...
                    0.655421741610324 0.884930329778292 0.977249868051821], 1e-12);
asserts.istrue(all(isinf(ci(1, :)) & ci(1, :) < 0));
asserts.isapprox(ci(2, :), [14.1121340673787 15.1121340673787 16.1121340673787 ...
                           17.1121340673787 18.1121340673787 19.1121340673787], 1e-12);
asserts.isapprox(zval, [0 0.8 1.6; 0.4 1.2 2.0], 1e-12);
%=============================================================================
[h, p, ci, zval] = ztest([NaN NaN], 0, 1);
asserts.isequal(h, false);
asserts.istrue(isnan(p));
asserts.istrue(all(isnan(ci)));
asserts.istrue(isnan(zval));
%=============================================================================
asserts.checkerror('ztest([1 2 3], 0)', _('Not enough input arguments.'));
asserts.checkerror('ztest([1 2 3], 0, -1)', sprintf(_('%s must be positive.'), 'SIGMA'));
asserts.checkerror('ztest([1 2 3], 0, 1, ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('ztest([1 2 3], 0, 1, ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('ztest([1 2 3], 0, 1, ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('ztest([1 2 3], 0, 1, ''Tail'', ''bad'')', _('TAIL must be ''both'', ''right'', or ''left''.'));
asserts.checkerror('ztest([1 2 3], 0, 1, ''Dim'', 0)', sprintf(_('Dimension argument must be %s.'), 'a positive integer scalar within indexing range'));
%=============================================================================
