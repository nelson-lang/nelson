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
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Position', [120 120 640 480]);
t = 0:pi / 5:2 * pi;
patch(sin(t), cos(t), 'y');
axis equal
frame = assert_opengl_patch_frame(f);
yellowPixels = frame(:, :, 1) > 120 & frame(:, :, 2) > 120 & frame(:, :, 3) < 140;
asserts.istrue(nnz(yellowPixels) > numel(yellowPixels) * 0.002);
close(f);
%=============================================================================
