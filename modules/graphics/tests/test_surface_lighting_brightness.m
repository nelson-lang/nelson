%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
% A lit surface must not render abnormally dark: two-sided lighting keeps
% faces whose normals point away from the viewer from collapsing to the
% ambient term. AmbientStrength must visibly change the surface brightness.
%=============================================================================
function b = litBrightness(faceLighting, ambient)
  [x, y, z] = peaks(30);
  f = figure('Visible', 'on', 'Position', [100 100 500 400], 'Color', [1 1 1]);
  ax = axes('Parent', f);
  surf(ax, x, y, z, 'EdgeColor', 'none', 'FaceColor', 'flat', ...
    'FaceLighting', faceLighting, 'AmbientStrength', ambient);
  colormap(ax, copper(64));
  axis(ax, 'off');
  view(ax, 35, 30);
  if (~strcmp(faceLighting, 'none'))
    light(ax, 'Position', [1 -1 1], 'Style', 'infinite');
  end
  drawnow();
  img = double(getframe(f).cdata);
  bg = (img(:, :, 1) > 250) & (img(:, :, 2) > 250) & (img(:, :, 3) > 250);
  lum = 0.2126 * img(:, :, 1) + 0.7152 * img(:, :, 2) + 0.0722 * img(:, :, 3);
  b = mean(lum(~bg));
  close(f);
end
%=============================================================================
close all;
unlit = litBrightness('none', 0.3);
litLow = litBrightness('gouraud', 0.3);
litHigh = litBrightness('gouraud', 0.9);
%=============================================================================
% The lit surface is not black: it keeps a substantial share of its colors.
asserts.istrue(litLow > unlit * 0.5);
%=============================================================================
% AmbientStrength clearly brightens the surface.
asserts.istrue(litHigh > litLow * 1.1);
%=============================================================================
close all;
%=============================================================================
