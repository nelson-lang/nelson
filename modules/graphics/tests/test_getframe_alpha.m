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
% Non-regression: getframe of a hidden figure must render semi-transparent
% patches faithfully. A red patch at FaceAlpha 0.4 over white blends to
% 0.4*[255 0 0] + 0.6*[255 255 255] = [255 153 153] (pink). A prior bug grabbed
% the hidden QOpenGLWidget and produced teal ([57 211 211]) instead.
%=============================================================================
close all;
f = figure('Visible', 'off');
set(f, 'Position', [100 100 400 400]);
set(f, 'Color', [1 1 1]);
p1 = patch([0 2 2 0], [0 0 2 2], 'r'); set(p1, 'FaceAlpha', 0.4);
hold on;
p2 = patch([1 3 3 1], [1 1 3 3], 'b'); set(p2, 'FaceAlpha', 0.4);
set(gca, 'XLim', [0 3], 'YLim', [0 3]);
axis off;
drawnow();
F = getframe(f);
img = F.cdata;
%=============================================================================
% getframe honors the figure pixel size.
ratio = f.DevicePixelRatio;
asserts.isequal(size(img, 1), round(400 * ratio));
asserts.isequal(size(img, 2), round(400 * ratio));
asserts.isequal(size(img, 3), 3);
%=============================================================================
% Red-only region (lower-left) is pink, not teal.
red = double(squeeze(img(round(300 * ratio), round(60 * ratio), :))');
asserts.istrue(red(1) > 230); % R high
asserts.istrue(red(2) > 120 && red(2) < 190); % G mid
asserts.istrue(red(3) > 120 && red(3) < 190); % B mid
% explicitly reject the teal failure mode (low R, high G/B).
asserts.istrue(~(red(1) < 120 && red(2) > 190 && red(3) > 190));
%=============================================================================
% Overlap region is a blended purple (both patches over white), not opaque.
ov = double(squeeze(img(round(200 * ratio), round(200 * ratio), :))');
asserts.istrue(ov(1) > 120 && ov(1) < 190);
asserts.istrue(ov(3) > 160);
%=============================================================================
close(f);
%=============================================================================
