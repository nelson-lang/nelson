%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--WITH DISPLAY-->
%=============================================================================
% An on-screen (OpenGL) textured surface must be visible. The CData->RGBA bake
% leaves the texel alpha at zero, so the GL texture must not carry it: the
% surface opacity comes from FaceAlpha, as in the software renderers.
%=============================================================================
f = figure('Visible', 'on', 'Position', [200 150 480 480]);
[X, Y, Z] = sphere(40);
orange = repmat(reshape(uint8([255 150 40]), 1, 1, 3), [64 128 1]);
surf(X, Y, Z, orange, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
daspect([1 1 1]);
view([90 0]);
axis off
frame = [];
for k = 1:5
  drawnow();
  pause(0.1);
  frame = __getframe__(f, true);
  if max(frame(:)) >= 8
    break
  end
end
if max(frame(:)) < 8
  skip_testsuite(true, 'OpenGL frame capture returned black frame.');
end
R = double(frame(:, :, 1));
G = double(frame(:, :, 2));
B = double(frame(:, :, 3));
orangePixels = sum(sum(R > 200 & G > 100 & G < 200 & B < 100));
% The sphere fills a large part of the axes.
asserts.istrue(orangePixels > numel(R) * 0.2);
close(f);
%=============================================================================
