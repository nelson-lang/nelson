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
asserts.isequal(nargin('iqr'), -1);
asserts.isequal(nargout('iqr'), 2);
%=============================================================================
A = [2 5 6 10 11 13];
[r, q] = iqr(A);
asserts.isequal(r, 6);
asserts.isequal(q, [5 11]);
%=============================================================================
B = reshape(1:24, [3 4 2]);
[rPage, qPage] = iqr(B, [1 2]);
asserts.isequal(size(rPage), [1 1 2]);
asserts.isequal(rPage(:), [6; 6]);
asserts.isapprox(qPage(:), [3.5; 9.5; 15.5; 21.5], 1e-12);
[rSlice, qSlice] = iqr(B, [2 3]);
asserts.isequal(size(rSlice), [3 1]);
asserts.isapprox(rSlice(:), [12; 12; 12], 1e-12);
asserts.isapprox(qSlice(:), [5.5; 6.5; 7.5; 17.5; 18.5; 19.5], 1e-12);
%=============================================================================
rAll = iqr([1 NaN 5 10 20], 'all');
asserts.isequal(rAll, 12);
%=============================================================================
S = single([1 2 3]);
[rSingle, qSingle] = iqr(S);
asserts.isequal(class(rSingle), 'single');
asserts.isequal(class(qSingle), 'single');
asserts.isapprox(double(rSingle), 1.5, 1e-6);
asserts.isapprox(double(qSingle), [1.25 2.75], 1e-6);
%=============================================================================
asserts.checkerror('iqr()', _('Not enough input arguments.'));
asserts.checkerror('iqr({1})', sprintf(_('%s must be a real numeric array.'), 'A'));
asserts.checkerror('iqr([1 2], 0)', _('Dimension argument must be a positive integer scalar, vector, or ''all''.'));
%=============================================================================
