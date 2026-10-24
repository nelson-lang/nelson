%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all;
controlPosition = [0.1 0.02 0.8 0.12];
f = figure('Units', 'pixels', 'Position', [100 100 500 300], ...
  'Color', [0.1 0.1 0.1]);
asserts.isequal(char(f.AutoResizeChildren), 'off');
uicontrol('Parent', f, 'Style', 'text', 'String', '', ...
  'Units', 'normalized', 'Position', controlPosition, ...
  'BackgroundColor', [0 1 0]);
drawnow();
assert_control_fills_normalized_position(getframe(f).cdata, controlPosition);

f.Position = [100 100 900 700];
drawnow();
assert_control_fills_normalized_position(getframe(f).cdata, controlPosition);
close(f);
%=============================================================================
function assert_control_fills_normalized_position(image, position)
  imageHeight = size(image, 1);
  imageWidth = size(image, 2);
  green = image(:, :, 2) > 220 & image(:, :, 1) < 40 & image(:, :, 3) < 40;

  left = max(1, floor(position(1) * imageWidth) + 3);
  right = min(imageWidth, ceil((position(1) + position(3)) * imageWidth) - 3);
  top = max(1, floor((1 - position(2) - position(4)) * imageHeight) + 3);
  bottom = min(imageHeight, ceil((1 - position(2)) * imageHeight) - 3);
  expectedInterior = green(top:bottom, left:right);

  asserts.istrue(mean(double(expectedInterior(:))) > 0.9);
end
%=============================================================================
