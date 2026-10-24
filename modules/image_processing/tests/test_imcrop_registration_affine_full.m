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
OPT_MULTI.AngleSearch = 0;
OPT_MULTI.ScaleSearch = 0.1;
OPT_MULTI.ShearSearch = 0.1;
cx = 24.5;
cy = 24.5;
I_AFF = zeros(48, 48);
I_AFF(12:24, 16:20) = 1;
I_AFF(30:36, 24:34) = 0.5;
AFF_TRUE = [1 0 0; 0 1 0; -cx -cy 1] * [1.1 0 0; 0.1 0.9 0; 0 0 1] ...
  * [1 0 0; 0 1 0; cx cy 1] * [1 0 0; 0 1 0; 2 -1 1];
J_AFF = imwarp(I_AFF, AFF_TRUE, 'linear', 'OutputView', 'same');
T_AFFINE_FULL = imregtform(I_AFF, J_AFF, 'affine', OPT_MULTI, MET_MULTI);
asserts.isapprox(T_AFFINE_FULL.T, AFF_TRUE, 1e-12);
%=============================================================================
