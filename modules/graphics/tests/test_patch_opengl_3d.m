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
x = [0 1 0 1]';
y = [0 0 1 1]';
c = [1 1 1];
patch(x, y, c);
axis equal
view(3)
frame = assert_opengl_patch_frame(f);
darkPixels = frame(:, :, 1) < 180 & frame(:, :, 2) < 180 & frame(:, :, 3) < 180;
asserts.istrue(nnz(darkPixels) > numel(darkPixels) * 0.002);
close(f);
%=============================================================================
