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
asserts.isequal(nargin('mad'), -2);
asserts.isequal(nargout('mad'), 1);
%=============================================================================
y1 = mad([1 2 3 4]);
asserts.isequal(y1, 1);
%=============================================================================
y2 = mad([1 2 100 4], 1);
asserts.isequal(y2, 1.5);
%=============================================================================
A = [1 2 3; 4 NaN 6; 7 8 9];
y3 = mad(A);
asserts.isapprox(y3, [2 3 2], 1e-12);
%=============================================================================
y4 = mad(A, 1, 2);
asserts.isapprox(y4, [1; 1; 1], 1e-12);
%=============================================================================
B = reshape(1:24, [4 3 2]);
y5 = mad(B, 0, [1 2]);
asserts.isequal(size(y5), [1 1 2]);
asserts.isapprox(y5(:), [3; 3], 1e-12);
%=============================================================================
y6 = mad(B, 1, 'all');
asserts.isequal(y6, 6);
%=============================================================================
y7 = mad(single([1 NaN 3]));
asserts.isequal(class(y7), 'double');
asserts.isequal(y7, 1);
%=============================================================================
y8 = mad(5);
asserts.isequal(y8, 0);
y9 = mad([]);
asserts.istrue(isnan(y9));
%=============================================================================
y10 = mad([1 2 3], 0, 3);
asserts.isequal(size(y10), [1 3]);
asserts.isequal(y10, [0 0 0]);
%=============================================================================
asserts.checkerror('mad({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('mad([1 2], 2)', sprintf(_('%s must be 0 or 1.'), 'FLAG'));
asserts.checkerror('mad([1 2], 0, 0)', _('Dimension argument must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('mad([1 2], 0, ''bad'')', _('Dimension argument must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('mad([1 2], 0, [1.5 2])', _('Dimension argument must be a positive integer scalar, vector, or ''all''.'));
%=============================================================================
