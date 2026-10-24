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
asserts.isequal(nargin('kurtosis'), -2);
asserts.isequal(nargout('kurtosis'), 1);
%=============================================================================
y1 = kurtosis([1 2 3 4]);
asserts.isapprox(y1, 1.64, 1e-12);
%=============================================================================
A = [1 2 3; 4 NaN 6; 7 8 9];
y2 = kurtosis(A);
asserts.isapprox(y2, [1.5 1 1.5], 1e-12);
%=============================================================================
y3 = kurtosis(A, 0, 2);
asserts.istrue(all(isnan(y3)));
%=============================================================================
y4 = kurtosis(single([1 NaN 3]));
asserts.isequal(class(y4), 'single');
asserts.isequal(y4, single(1));
%=============================================================================
asserts.checkerror('kurtosis({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('kurtosis([1 2], 2)', sprintf(_('%s must be 0 or 1.'), 'Flag'));
asserts.checkerror('kurtosis([1 2], 1, 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
%=============================================================================
