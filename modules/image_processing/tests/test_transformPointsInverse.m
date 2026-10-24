%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% forward then inverse is a round trip (2-D affine rotation)
tform = affine2d([cosd(30) sind(30) 0; -sind(30) cosd(30) 0; 0 0 1]);
[x, y] = transformPointsForward(tform, 1.5, -0.5);
[u, v] = transformPointsInverse(tform, x, y);
asserts.isapprox([u v], [1.5 -0.5], 1e-12);
%=============================================================================
% inverse of a translation
t = affine2d([1 0 0; 0 1 0; 4 3 1]);
[u, v] = transformPointsInverse(t, 14, 23);
asserts.isequal([u v], [10 20]);
%=============================================================================
% packed N-by-2 round trip
P = [1 0; 0 1; 2 3];
asserts.isapprox(transformPointsInverse(tform, transformPointsForward(tform, P)), P, 1e-12);
%=============================================================================
% 3-D affine inverse
t3 = affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 2 3 4 1]);
[a, b, c] = transformPointsInverse(t3, 3, 4, 5);
asserts.isequal([a b c], [1 1 1]);
%=============================================================================
% projective 2-D inverse round trip
tp = projective2d([1 0 0.1; 0 1 0.2; 0 0 1]);
[px, py] = transformPointsForward(tp, 2, 3);
[qx, qy] = transformPointsInverse(tp, px, py);
asserts.isapprox([qx qy], [2 3], 1e-12);
%=============================================================================
