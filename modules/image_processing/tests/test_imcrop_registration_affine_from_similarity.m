%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(isempty(which('fft2')), 'Image registration correlation requires fft2.');
%=============================================================================
[OPT_MULTI, MET_MULTI] = imregconfig('multimodal');
asserts.isequal(MET_MULTI.Type, 'Correlation');
OPT_MULTI.AngleSearch = 10;
OPT_MULTI.ScaleSearch = 0.1;
I_RIGID = zeros(48, 48);
I_RIGID(16:30, 20:27) = 1;
cx = 24.5;
cy = 24.5;
theta = -5 * pi / 180;
c = cos(theta);
s = sin(theta);
SIM_TRUE = [1 0 0; 0 1 0; -cx -cy 1] * [1.05 * c 1.05 * s 0; -1.05 * s 1.05 * c 0; 0 0 1] ...
  * [1 0 0; 0 1 0; cx cy 1] * [1 0 0; 0 1 0; -2 3 1];
J_SIM = imwarp(I_RIGID, SIM_TRUE, 'linear', 'OutputView', 'same');
T_AFFINE = imregtform(I_RIGID, J_SIM, 'affine', OPT_MULTI, MET_MULTI);
K_AFFINE_SIM = imwarp(I_RIGID, T_AFFINE, 'linear', 'OutputView', 'same');
asserts.istrue(mean((K_AFFINE_SIM(:) - J_SIM(:)) .^ 2) < 1e-4);
%=============================================================================
