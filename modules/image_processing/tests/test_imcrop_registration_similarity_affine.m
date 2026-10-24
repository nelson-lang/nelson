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
T_SIM = imregtform(I_RIGID, J_SIM, 'similarity', OPT_MULTI, MET_MULTI);
asserts.isapprox(T_SIM.T, SIM_TRUE, 1e-12);
%=============================================================================
