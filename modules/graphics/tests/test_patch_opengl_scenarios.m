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
close all
f = figure('Position', [120 120 640 480]);
hold on
x = [0 1 1 0];
y = [0 0 1 1];
patch(x, y, 'blue');
x2 = [2 5; 2 5; 8 8];
y2 = [4 0; 8 2; 4 0];
patch(x2, y2, 'green');
hold off
frame = assert_opengl_patch_frame(f);
bluePixels = frame(:, :, 3) > 120 & frame(:, :, 1) < 120 & frame(:, :, 2) < 160;
greenPixels = frame(:, :, 2) > 120 & frame(:, :, 1) < 160 & frame(:, :, 3) < 160;
asserts.istrue(nnz(bluePixels) + nnz(greenPixels) > numel(bluePixels) * 0.002);
close(f);
%=============================================================================
