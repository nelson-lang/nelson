%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
skip_testsuite(isempty(which('fft2')), 'Image translation registration checks require fft2.');
%=============================================================================
I = zeros(24, 24);
I(8:14, 9:15) = 1;
J = imtranslate(I, [5 3], 'Interpolation', 'nearest', 'OutputView', 'full');
asserts.isequal(size(J), [27 29]);
asserts.isequal(sum(J(:)), sum(I(:)));
T = [1 0 0; 0 1 0; 4 3 1];
W = imwarp(I, T, 'Interpolation', 'nearest');
asserts.isequal(size(W), size(I));
asserts.istrue(sum(W(:)) <= sum(I(:)));
tform = affine2d([1 0 0; 0 1 0; 4 3 1]);
W2 = imwarp(I, tform, 'Interpolation', 'nearest');
asserts.isequal(W2, W);
tform = projective2d(eye(3));
asserts.isequal(imwarp(I, tform, 'Interpolation', 'nearest'), I);
moving = [0 0; 1 0; 0 1];
fixed = [4 3; 5 3; 4 4];
tform = fitgeotrans(moving, fixed, 'affine');
asserts.isapprox(tform.T, [1 0 0; 0 1 0; 4 3 1], 1e-12);
R = imref2d([32 40], [0.5 40.5], [0.5 32.5]);
[W3, R3] = imwarp(I, affine2d([1 0 0; 0 1 0; 4 3 1]), 'nearest', 'OutputView', R);
asserts.isequal(size(W3), [32 40]);
asserts.isequal(R3.XWorldLimits, [0.5 40.5]);
R_PIXEL = imref2d([2 3], 2, 3);
asserts.isequal(R_PIXEL.XWorldLimits, [1 7]);
asserts.isequal(R_PIXEL.YWorldLimits, [1.5 7.5]);
[XW_PIXEL, YW_PIXEL] = intrinsicToWorld(R_PIXEL, [1 3], [1 2]);
asserts.isequal(XW_PIXEL, [2 6]);
asserts.isequal(YW_PIXEL, [3 6]);
[XI_PIXEL, YI_PIXEL] = worldToIntrinsic(R_PIXEL, XW_PIXEL, YW_PIXEL);
asserts.isequal(XI_PIXEL, [1 3]);
asserts.isequal(YI_PIXEL, [1 2]);
[R_SUB, C_SUB] = worldToSubscript(R_PIXEL, [2 8], [3 6]);
asserts.isequal(R_SUB(1), 1);
asserts.isequal(C_SUB(1), 1);
asserts.istrue(isnan(R_SUB(2)) && isnan(C_SUB(2)));
asserts.istrue(sizesMatch(R_PIXEL, zeros(2, 3)));
RI = imref2d(size(I), [10.5 34.5], [20.5 44.5]);
[W4, R4] = imwarp(I, RI, affine2d(eye(3)), 'nearest');
asserts.isequal(W4, I);
asserts.isequal(R4.XWorldLimits, [10.5 34.5]);
asserts.isequal(R4.XIntrinsicLimits, [0.5 24.5]);
asserts.isequal(R4.ImageExtentInWorldX, 24);
V = zeros(16, 16, 8);
V(5:11, 5:11, 3:6) = 1;
W5 = imwarp(V, affine3d([1 0 0 0; 0 1 0 0; 0 0 1 0; 2 1 0 1]), ...
  'nearest', 'FillValues', 0);
asserts.isequal(size(W5), size(V));
asserts.isequal(sum(W5(:)), sum(V(:)));
R3_PIXEL = imref3d([2 3 4], 2, 3, 4);
asserts.isequal(R3_PIXEL.XWorldLimits, [1 7]);
asserts.isequal(R3_PIXEL.YWorldLimits, [1.5 7.5]);
asserts.isequal(R3_PIXEL.ZWorldLimits, [2 18]);
[XW3_PIXEL, YW3_PIXEL, ZW3_PIXEL] = intrinsicToWorld(R3_PIXEL, [1 3], [1 2], [1 4]);
asserts.isequal(XW3_PIXEL, [2 6]);
asserts.isequal(YW3_PIXEL, [3 6]);
asserts.isequal(ZW3_PIXEL, [4 16]);
asserts.istrue(sizesMatch(R3_PIXEL, zeros(2, 3, 4)));
I_MARK = [5 4 5; 4 3 4; 5 4 5];
markers = false(3, 3);
markers(1, 1) = true;
J_MARK = imimposemin(I_MARK, markers);
asserts.istrue(J_MARK(1, 1) < J_MARK(2, 2));
R_MARK = imregionalmin(J_MARK, 4);
asserts.istrue(R_MARK(1, 1));
H_MARK = imhmin(I_MARK, 1);
E_MARK = imextendedmin(I_MARK, 1);
asserts.isequal(size(H_MARK), size(I_MARK));
asserts.istrue(any(E_MARK(:)));
P_MARK = imregionalmax(-I_MARK, 4);
HM_MARK = imhmax(uint8([1 1 1; 1 5 1; 1 1 1]), 2);
EM_MARK = imextendedmax(-I_MARK, 1);
asserts.isequal(size(P_MARK), size(I_MARK));
asserts.isequal(HM_MARK(2, 2), uint8(3));
asserts.istrue(any(EM_MARK(:)));
mask = false(5, 5);
mask(2:4, 2:4) = true;
marker = false(5, 5);
marker(3, 3) = true;
asserts.isequal(imreconstruct(marker, mask, 4), mask);
I_CONN = [0 0.1 0.9; 0.1 0.15 0.9; 0.9 0.9 0.2];
G_CONN = grayconnected(I_CONN, 2, 2, 0.1, 8);
asserts.istrue(G_CONN(3, 3));
[optimizer, metric] = imregconfig('monomodal');
I_REG = zeros(32, 32);
I_REG(10:18, 12:20) = 1;
J_REG = imtranslate(I_REG, [4 -3], 'nearest');
tform = imregtform(I_REG, J_REG, 'translation', optimizer, metric, 'Interpolation', 'nearest');
K_REG = imregister(I_REG, J_REG, 'translation', optimizer, metric, 'Interpolation', 'nearest');
asserts.isapprox(tform.T, [1 0 0; 0 1 0; 4 -3 1], 1e-12);
asserts.isequal(K_REG, J_REG);
%=============================================================================
asserts.checkerror('imtranslate([1 2], [1 0 0])', _('Translation vector must have two elements.'));
%=============================================================================
asserts.checkerror('imtranslate([1 2], [1 Inf])', sprintf(_('%s must contain finite values.'), 'Translation vector'));
%=============================================================================
asserts.checkerror('imtranslate(sparse([1 2]), [1 0])', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imtranslate([1 2], [1 0], 1, 2)', sprintf(_('%s option name must be a string.'), 'imtranslate'));
%=============================================================================
asserts.checkerror('imtranslate([1 2], [1 0], ''Interpolation'', 1)', sprintf(_('%s must be a string.'), 'Interpolation method'));
%=============================================================================
asserts.checkerror('imtranslate([1 2], [1 0], ''Interpolation'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'interpolation method'));
%=============================================================================
asserts.checkerror('imtranslate([1 2], [1 0], ''OutputView'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'output view'));
