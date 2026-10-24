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
asserts.isequal(nargin('ndgrid'), -1)
asserts.isequal(nargout('ndgrid'), -1)
asserts.istrue(isbuiltin('ndgrid'))
asserts.isfalse(ismacro('ndgrid'))
%=============================================================================
asserts.checkerror('ndgrid()', _('Not enough input arguments.'));
asserts.checkerror('[X, Y, Z, W, K, U]=ndgrid(1, 2, 3, 4, 5)', _('Too many output arguments.'));
%=============================================================================
asserts.isequal(ndgrid(1, 2, 3, 4, 5), 1);
%=============================================================================
[X, Y] = ndgrid(1, 2);
asserts.isequal(X, 1);
asserts.isequal(Y, 2);
%=============================================================================
x = 1:3;
y = 4:6;
[X, Y] = ndgrid(x, y);
asserts.isequal(X, [1 1 1; 2 2 2; 3 3 3]);
asserts.isequal(Y, [4 5 6; 4 5 6; 4 5 6]);
%=============================================================================
x = 1:2;
y = 3:4;
z = 5:6;
[X, Y, Z] = ndgrid(x, y, z);
X_REF = [];
X_REF(:, :, 1) = [1 1; 2 2];
X_REF(:, :, 2) = [1 1; 2 2];
Y_REF = [];
Y_REF(:, :, 1) = [3 4; 3 4];
Y_REF(:, :, 2) = [3 4; 3 4];
Z_REF = [];
Z_REF(:, :, 1) = [5 5; 5 5];
Z_REF(:, :, 2) = [6 6 6 6];
asserts.isequal(X, X_REF);
asserts.isequal(Y, Y_REF);
asserts.isequal(Z, Z_REF);
%=============================================================================
[gridX, gridY] = ndgrid(1:2, 3:4);
asserts.isequal(gridX, [1 1; 2 2]);
asserts.isequal(gridY, [3 4; 3 4]);
%=============================================================================
[gridX, gridY] = ndgrid(single([]), int16(1:3));
asserts.isequal(size(gridX), [0 3]);
asserts.isequal(size(gridY), [0 3]);
asserts.isequal(class(gridX), 'single');
asserts.isequal(class(gridY), 'int16');
%=============================================================================
[gridX, gridY] = ndgrid(sparse(logical([1 0 1])), 1:2);
asserts.isequal(gridX, logical([1 1; 0 0; 1 1]));
asserts.isequal(gridY, [1 2; 1 2; 1 2]);
asserts.isfalse(issparse(gridX));
%=============================================================================
