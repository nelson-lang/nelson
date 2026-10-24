%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
I = reshape(1:9, 3, 3);
R = imref2d([3 4]);
asserts.isequal(R.ImageSize, [3 4]);
asserts.isequal(R.XWorldLimits, [0.5 4.5]);
asserts.isequal(R.YWorldLimits, [0.5 3.5]);
asserts.isequal(R.XIntrinsicLimits, [0.5 4.5]);
asserts.isequal(R.YIntrinsicLimits, [0.5 3.5]);
asserts.isequal(R.ImageExtentInWorldX, 4);
asserts.isequal(R.ImageExtentInWorldY, 3);
R_DEFAULT = imref2d();
asserts.isequal(R_DEFAULT.ImageSize, [2 2]);
asserts.isequal(R_DEFAULT.XWorldLimits, [0.5 2.5]);
R_PIXEL = imref2d([2 3], 2, 3);
asserts.isequal(R_PIXEL.XWorldLimits, [1 7]);
asserts.isequal(R_PIXEL.YWorldLimits, [1.5 7.5]);
asserts.isequal(R_PIXEL.PixelExtentInWorldX, 2);
asserts.isequal(R_PIXEL.PixelExtentInWorldY, 3);
[XW, YW] = intrinsicToWorld(R_PIXEL, [1 3], [1 2]);
asserts.isequal(XW, [2 6]);
asserts.isequal(YW, [3 6]);
[XI, YI] = worldToIntrinsic(R_PIXEL, XW, YW);
asserts.isequal(XI, [1 3]);
asserts.isequal(YI, [1 2]);
[RS, CS] = worldToSubscript(R_PIXEL, [2 6 8], [3 6 6]);
asserts.isequal(RS(1:2), [1 2]);
asserts.isequal(CS(1:2), [1 3]);
asserts.istrue(isnan(RS(3)) && isnan(CS(3)));
asserts.istrue(sizesMatch(R_PIXEL, zeros(2, 3)));
asserts.istrue(sizesMatch(R_PIXEL, zeros(2, 3, 3)));
asserts.isfalse(sizesMatch(R_PIXEL, zeros(3, 2)));
[W_REF, R_REF] = imwarp(I, eye(3), 'nearest', 'OutputView', R);
asserts.isequal(W_REF, [1 4 7 0; 2 5 8 0; 3 6 9 0]);
asserts.isequal(R_REF.ImageSize, [3 4]);
asserts.isequal(imwarp(I, eye(3), 'nearest', 'OutputView', 'same'), I);
W_NUMERIC_VIEW = imwarp(I, eye(3), 'nearest', 'OutputView', [4 5]);
asserts.isequal(size(W_NUMERIC_VIEW), [4 5]);
asserts.isequal(W_NUMERIC_VIEW(1:3, 1:3), I);
W_FULL = imwarp(I, [1 0 0; 0 1 0; 1 0 1], 'nearest', 'OutputView', 'full');
asserts.isequal(W_FULL, I);
RI = imref2d([3 3], [10.5 13.5], [20.5 23.5]);
[W_SRC_REF, R_SRC_OUT] = imwarp(I, RI, eye(3), 'nearest');
asserts.isequal(W_SRC_REF, I);
asserts.isequal(R_SRC_OUT.ImageSize, [3 3]);
asserts.isequal(R_SRC_OUT.XWorldLimits, [10.5 13.5]);
asserts.isequal(R_SRC_OUT.YWorldLimits, [20.5 23.5]);
[W_SRC_FULL, R_SRC_FULL] = imwarp(I, RI, [1 0 0; 0 1 0; 1 0 1], 'nearest', 'OutputView', 'full');
asserts.isequal(W_SRC_FULL, I);
asserts.isequal(R_SRC_FULL.XWorldLimits, [11.5 14.5]);
asserts.isequal(R_SRC_FULL.YWorldLimits, [20.5 23.5]);
RI_SCALE = imref2d([2 2], [0 4], [0 2]);
[W_SCALE_REF, R_SCALE_OUT] = imwarp([1 3; 2 4], RI_SCALE, eye(3), 'nearest');
asserts.isequal(W_SCALE_REF, [1 3; 2 4]);
asserts.isequal(R_SCALE_OUT.PixelExtentInWorldX, 2);
asserts.isequal(R_SCALE_OUT.PixelExtentInWorldY, 1);
asserts.isequal(R_SCALE_OUT.ImageExtentInWorldX, 4);
asserts.isequal(R_SCALE_OUT.ImageExtentInWorldY, 2);
TA = affine2d([1 0 0; 0 1 0; 1 0 1]);
asserts.isequal(TA.Type, 'affine');
asserts.isequal(imwarp(I, TA, 'nearest'), I);
asserts.isequal(imwarp(I, TA, 'nearest', 'OutputView', 'same'), [0 1 4; 0 2 5; 0 3 6]);
TP = projective2d(eye(3));
asserts.isequal(TP.Type, 'projective');
asserts.isequal(imwarp(I, TP, 'nearest'), I);
%=============================================================================
