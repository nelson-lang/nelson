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
asserts.isequal(nargin('nanmedian'), -2);
asserts.isequal(nargout('nanmedian'), 1);
asserts.isequal(nargin('nanstd'), -2);
asserts.isequal(nargout('nanstd'), 1);
asserts.isequal(nargin('nanvar'), -2);
asserts.isequal(nargout('nanvar'), 1);
%=============================================================================
X = [1 NaN 3; 4 5 NaN];
asserts.isapprox(nanmean(X), [2.5 5 3], 1e-12);
asserts.isapprox(nanmean(X, 2), [2; 4.5], 1e-12);
asserts.isapprox(nanmean(X, 'all'), 3.25, 1e-12);
asserts.isapprox(nanmedian(X), [2.5 5 3], 1e-12);
asserts.isapprox(nanmedian(X, 2), [2; 4.5], 1e-12);
asserts.isapprox(nanstd(X), [2.121320343559642 0 0], 1e-12);
asserts.isapprox(nanstd(X, 1, 2), [1; 0.5], 1e-12);
asserts.isapprox(nanvar(X), [4.5 0 0], 1e-12);
asserts.isapprox(nanvar(X, 1, 2), [1; 0.25], 1e-12);
%=============================================================================
Y = single([1 NaN; 3 5]);
asserts.isequal(class(nanmean(Y)), 'single');
asserts.isapprox(double(nanmean(Y)), [2 5], 1e-6);
asserts.isequal(class(nanstd(Y, 1, 'all')), 'single');
asserts.isapprox(double(nanstd(Y, 1, 'all')), sqrt(8 / 3), 1e-6);
%=============================================================================
asserts.istrue(isnan(nanmean([])));
asserts.isequal(class(nanmean(single([]))), 'single');
asserts.isequal(nanmean([1 2], 3), [1 2]);
asserts.isequal(nanstd([1 2], 0, 3), [0 0]);
%=============================================================================
asserts.checkerror('nanmean({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('nanmean([1 2], 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('nanmedian([1 2], ''bad'')', _('Invalid option.'));
asserts.checkerror('nanstd([1 2], 2)', sprintf(_('%s must be 0 or 1.'), 'Flag'));
asserts.checkerror('nanvar([1 2], [1 2 3], 2)', _('Weights length must match the operating dimension.'));
%=============================================================================
