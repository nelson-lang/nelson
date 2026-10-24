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
asserts.isequal(nargin('nanvar'), -2);
asserts.isequal(nargout('nanvar'), 1);
%=============================================================================
X = magic(3);
X([1 6:9]) = NaN;
asserts.isapprox(nanvar(X), [0.5 8 NaN], 1e-12);
asserts.isapprox(nanvar(X, 1), [0.25 4 NaN], 1e-12);
asserts.isapprox(nanvar(X, 0, 2), [0; 2; 0], 1e-12);
%=============================================================================
A = reshape(1:24, [3 4 2]);
A([8:10 18]) = NaN;
asserts.isapprox(nanvar(A, 0, 'all'), 56.828947368421055, 1e-12);
V12 = nanvar(A, 0, [1 2]);
asserts.isequal(size(V12), [1 1 2]);
asserts.isapprox(V12(:), [14.5; 14.272727272727252], 1e-12);
V13 = nanvar(A, 0, [1 3]);
asserts.isequal(size(V13), [1 4]);
asserts.isapprox(V13(:), [44; 40.299999999999997; 42.916666666666664; 40.300000000000011], 1e-12);
%=============================================================================
W = [1 NaN 3; 4 5 NaN];
asserts.isapprox(nanvar(W, [1 2], 1), [2 0 0], 1e-12);
asserts.isapprox(nanvar(W, [1 2 3], 2), [0.75; 0.222222222222224], 1e-12);
asserts.isapprox(nanvar([1 NaN 3], [1 NaN 2]), 0.888888888888889, 1e-12);
%=============================================================================
Z = nanvar([1 NaN 3], 0, 3);
asserts.isequal(Z, [0 NaN 0]);
asserts.istrue(isnan(nanvar([NaN NaN])));
asserts.istrue(isnan(nanvar([])));
S = single([1 NaN 4]);
Y = nanvar(S);
asserts.isequal(class(Y), 'single');
asserts.isapprox(double(Y), 4.5, 1e-6);
%=============================================================================
asserts.checkerror('nanvar()', _('Not enough input arguments.'));
asserts.checkerror('nanvar({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('nanvar([1 2], 2)', _('Weights must be 0, 1, or a nonnegative vector.'));
asserts.checkerror('nanvar([1 2], -1)', _('Weights must be 0, 1, or a nonnegative vector.'));
asserts.checkerror('nanvar([1 2], [1 2], ''all'')', _('Vector weights require a scalar operating dimension.'));
asserts.checkerror('nanvar([1 2], [1 2 3])', _('Weights length must match the operating dimension.'));
%=============================================================================
