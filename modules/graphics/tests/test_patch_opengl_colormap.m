%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Position', [120 120 640 480]);
colormap(jet(64));
x = [0 0; 1 1; 1 0];
y = [0 0; 0 1; 1 1];
patch(x, y, [32 1]);
frame = assert_opengl_patch_frame(f);
redPixels = frame(:, :, 1) > 90 & frame(:, :, 2) < 120 & frame(:, :, 3) < 140;
bluePixels = frame(:, :, 3) > 90 & frame(:, :, 1) < 120 & frame(:, :, 2) < 160;
asserts.istrue(nnz(redPixels) > numel(redPixels) * 0.002);
asserts.istrue(nnz(bluePixels) > numel(bluePixels) * 0.002);
[redRows, redCols] = find(redPixels);
[blueRows, blueCols] = find(bluePixels);
asserts.istrue(mean(redRows) < mean(blueRows));
asserts.istrue(mean(redCols) < mean(blueCols));
close(f);
%=============================================================================
