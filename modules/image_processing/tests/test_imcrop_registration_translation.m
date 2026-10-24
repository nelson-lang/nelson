%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
skip_testsuite(isempty(which('fft2')), 'Image registration correlation requires fft2.');
%=============================================================================
moving = [0 0; 1 0; 0 1];
fixed = [2 3; 3 3; 2 4];
TFIT = fitgeotrans(moving, fixed, 'affine');
asserts.isapprox(TFIT.T, [1 0 0; 0 1 0; 2 3 1], 1e-12);
moving4 = [0 0; 1 0; 1 1; 0 1];
fixed4 = [0 0; 2 0; 2 3; 0 3];
TPFIT = fitgeotrans(moving4, fixed4, 'projective');
p = [moving4 ones(4, 1)] * (TPFIT.T);
p = p(:, 1:2) ./ p(:, 3);
asserts.isapprox(p, fixed4, 1e-10);
I_REG = zeros(16, 16);
I_REG(5:8, 6:9) = 1;
J_REG = imtranslate(I_REG, [3 -2], 'nearest');
[TREG, PEAKREG] = imregcorr(I_REG, J_REG);
asserts.isapprox(TREG.T, [1 0 0; 0 1 0; 3 -2 1], 1e-12);
asserts.istrue(PEAKREG > 0.6);
asserts.isequal(imwarp(I_REG, TREG, 'nearest', 'OutputView', 'same'), J_REG);
TREG_UINT8 = imregcorr(uint8(I_REG * 255), uint8(J_REG * 255));
asserts.isapprox(TREG_UINT8.T, TREG.T, 1e-12);
RGB_REG = cat(3, I_REG, I_REG * 0.5, I_REG * 0.25);
RGB_REG_SHIFT = cat(3, J_REG, J_REG * 0.5, J_REG * 0.25);
asserts.isapprox(imregcorr(RGB_REG, RGB_REG_SHIFT, 'Translation').T, TREG.T, 1e-12);
asserts.isapprox(imregcorr(I_REG, J_REG, 'rigid').T, TREG.T, 1e-12);
asserts.isapprox(imregcorr(I_REG, J_REG, 'similarity').T, TREG.T, 1e-12);
asserts.isapprox(imregcorr(I_REG, J_REG, 'affine').T, TREG.T, 1e-12);
asserts.isequal(imregcorr(ones(4, 4), ones(4, 4)).T, eye(3));
[OPT_REG, MET_REG] = imregconfig('monomodal');
asserts.isequal(imregconfig(string('MonoModal')).Type, OPT_REG.Type);
TREG2 = imregtform(I_REG, J_REG, 'translation', OPT_REG, MET_REG, 'Interpolation', 'nearest');
asserts.isapprox(TREG2.T, TREG.T, 1e-12);
K_REG = imregister(I_REG, J_REG, 'translation', OPT_REG, MET_REG, 'Interpolation', 'nearest');
asserts.isequal(K_REG, J_REG);
TREG_STRING_OPTIONS = imregtform(I_REG, J_REG, string('Translation'), OPT_REG, MET_REG, ...
  string('Interpolation'), string('Nearest'), string('PyramidLevels'), uint8(1), ...
  string('DisplayOptimization'), false);
asserts.isapprox(TREG_STRING_OPTIONS.T, TREG.T, 1e-12);
K_REG_INIT = imregister(I_REG, J_REG, string('translation'), OPT_REG, MET_REG, ...
  string('Interpolation'), string('nearest'), string('InitialTransformation'), affine2d(eye(3)), ...
  string('PyramidLevels'), 1, string('DisplayOptimization'), false);
asserts.isequal(K_REG_INIT, J_REG);
asserts.isequal(MET_REG.Type, 'MeanSquares');
%=============================================================================
