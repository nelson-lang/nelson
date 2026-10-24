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
asserts.isequal(nargin('nanmean'), -2);
asserts.isequal(nargout('nanmean'), 1);
%=============================================================================
X = magic(3);
X([1 6:9]) = NaN;
asserts.isapprox(nanmean(X), [3.5 3 NaN], 1e-12);
asserts.isapprox(nanmean(X, 2), [1; 4; 4], 1e-12);
asserts.isapprox(nanmean(X, 'all'), 3.25, 1e-12);
%=============================================================================
A = reshape(1:24, [3 4 2]);
A([8:10 18]) = NaN;
M12 = nanmean(A, [1 2]);
asserts.isequal(size(M12), [1 1 2]);
asserts.isapprox(M12(:), [5.666666666666667; 18.545454545454547], 1e-12);
M23 = nanmean(A, [2 3]);
asserts.isequal(size(M23), [3 1]);
asserts.isapprox(M23(:), [11.714285714285714; 13.142857142857142; 13.5], 1e-12);
%=============================================================================
asserts.isequal(nanmean([1 NaN 3], 3), [1 NaN 3]);
asserts.isequal(nanmean([1 NaN 3], [3 4]), [1 NaN 3]);
asserts.istrue(isnan(nanmean([NaN NaN])));
asserts.istrue(isnan(nanmean([])));
S = single([1 NaN 4]);
Y = nanmean(S);
asserts.isequal(class(Y), 'single');
asserts.isapprox(double(Y), 2.5, 1e-6);
%=============================================================================
asserts.checkerror('nanmean()', _('Not enough input arguments.'));
asserts.checkerror('nanmean({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('nanmean([1 2], 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('nanmean([1 2], ''bad'')', _('Invalid option.'));
asserts.checkerror('nanmean([1 2], 1, 2)', _('Too many input arguments.'));
%=============================================================================
