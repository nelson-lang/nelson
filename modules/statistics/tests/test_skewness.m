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
asserts.isequal(nargin('skewness'), -2);
asserts.isequal(nargout('skewness'), 1);
%=============================================================================
y1 = skewness([1 2 3 4]);
asserts.isapprox(y1, 0, 1e-12);
%=============================================================================
A = [1 2 3; 4 NaN 6; 7 8 9];
y2 = skewness(A);
asserts.isapprox(y2, [0 0 0], 1e-12);
%=============================================================================
y3 = skewness(A, 0, 2);
asserts.istrue(isnan(y3(2)));
asserts.isapprox(y3([1 3]), [0; 0], 1e-12);
%=============================================================================
y4 = skewness(single([1 NaN 3]));
asserts.isequal(class(y4), 'single');
asserts.isapprox(y4, single(0), 1e-12);
%=============================================================================
asserts.checkerror('skewness({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('skewness([1 2], 2)', sprintf(_('%s must be 0 or 1.'), 'Flag'));
asserts.checkerror('skewness([1 2], 1, 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
%=============================================================================
