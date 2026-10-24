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
asserts.isequal(nargin('trimmean'), -3);
asserts.isequal(nargout('trimmean'), 1);
%=============================================================================
x = [-100 1 2 3 4 5 6 7 8 100];
asserts.isequal(trimmean(x, 20), 4.5);
asserts.isequal(trimmean(x, 20, 'floor'), 4.5);
asserts.isequal(trimmean(x, 15, 'round'), 4.5);
asserts.isequal(trimmean(x, 15, 'floor'), 3.6);
asserts.isapprox(trimmean(x, 15, 'weighted'), 4.235294117647059, 1e-12);
%=============================================================================
X = reshape(1:40, [5 4 2]);
X([3 37]) = -100;
asserts.isapprox(trimmean(X, 10, 'all'), 19.472222222222221, 1e-12);
P = trimmean(X, 10, [1 2]);
asserts.isequal(size(P), [1 1 2]);
asserts.isapprox(P(:), [10.388888888888889; 29.611111111111111], 1e-12);
R = trimmean(X, 10, [2 3]);
asserts.isapprox(R(:), [18.5; 2.375; 7.625; 21.5; 22.5], 1e-12);
%=============================================================================
A = [1 2 100; 4 NaN 6; 7 8 9];
asserts.isapprox(trimmean(A, 20), [4 5 38.333333333333336], 1e-12);
asserts.isapprox(trimmean(A, 20, 2), [34.333333333333336; 5; 8], 1e-12);
asserts.isapprox(trimmean(A, 20, 'floor', 2), [34.333333333333336; 5; 8], 1e-12);
asserts.isapprox(trimmean(A, 20, 2, 'floor'), [34.333333333333336; 5; 8], 1e-12);
%=============================================================================
asserts.isequal(trimmean([1 2 3], 20, 3), [1 2 3]);
asserts.isequal(trimmean([1 2 3], 20, [3 4]), [1 2 3]);
asserts.istrue(isnan(trimmean([1 2], 100)));
asserts.istrue(isnan(trimmean([NaN NaN], 20)));
S = trimmean(single([1 2 100 4]), 50);
asserts.isequal(class(S), 'single');
asserts.isequal(S, single(3));
%=============================================================================
asserts.checkerror('trimmean()', _('Not enough input arguments.'));
asserts.checkerror('trimmean([1 2])', _('Not enough input arguments.'));
asserts.checkerror('trimmean({1}, 20)', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('trimmean([1 2], -1)', _('PERCENT must be a scalar between 0 and 100.'));
asserts.checkerror('trimmean([1 2], 101)', _('PERCENT must be a scalar between 0 and 100.'));
asserts.checkerror('trimmean([1 2], 20, 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('trimmean([1 2], 20, ''bad'')', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('trimmean([1 2], 20, ''round'', ''floor'')', _('Invalid trimming flag.'));
%=============================================================================
