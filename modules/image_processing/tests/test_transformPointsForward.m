%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 2-D affine rotation, separate coordinates
tform = affine2d([cosd(30) sind(30) 0; -sind(30) cosd(30) 0; 0 0 1]);
[x, y] = transformPointsForward(tform, 1, 0);
asserts.isapprox(x, cosd(30), 1e-12);
asserts.isapprox(y, sind(30), 1e-12);
%=============================================================================
% shape of matrix coordinates is preserved
u = [1 2; 3 4];
v = [0 1; 2 3];
[xx, yy] = transformPointsForward(tform, u, v);
asserts.isequal(size(xx), [2 2]);
asserts.isequal(size(yy), [2 2]);
asserts.isapprox(xx(2, 1), 3 * cosd(30) - 2 * sind(30), 1e-12);
%=============================================================================
% packed N-by-2 form returns an N-by-2 matrix
P = [1 0; 0 1; 2 3];
P2 = transformPointsForward(tform, P);
asserts.isequal(size(P2), [3 2]);
asserts.isapprox(P2(1, :), [cosd(30) sind(30)], 1e-12);
%=============================================================================
% 2-D affine translation
t = affine2d([1 0 0; 0 1 0; 4 3 1]);
[x, y] = transformPointsForward(t, 10, 20);
asserts.isequal([x y], [14 23]);
%=============================================================================
% 3-D affine translation, separate and packed
t3 = affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 2 3 4 1]);
[a, b, c] = transformPointsForward(t3, 1, 1, 1);
asserts.isequal([a b c], [3 4 5]);
Q = transformPointsForward(t3, [1 1 1; 0 0 0]);
asserts.isequal(Q, [3 4 5; 2 3 4]);
%=============================================================================
% projective 2-D forward applies homogeneous normalization
tp = projective2d([1 0 0.1; 0 1 0.2; 0 0 1]);
[px, py] = transformPointsForward(tp, 2, 3);
asserts.isapprox([px py], [2 3] / 1.8, 1e-12);
%=============================================================================
% error handling
asserts.checkerror('transformPointsForward(affine2d(), [1 2 3])', _('Packed points must be a matrix with one column per dimension.'));
asserts.checkerror('transformPointsForward(affine2d(), [1 2], [1 2 3])', _('Point coordinate arrays must all be the same size.'));
asserts.checkerror('transformPointsForward(5, 1, 0)', _('First argument must be a geometric transformation structure.'));
%=============================================================================
