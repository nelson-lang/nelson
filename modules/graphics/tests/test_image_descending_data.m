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
skip_if_portable_graphics_backend('descending image raster orientation differs in portable rendering.');
% XData or YData given the other way round mirrors the image along that axis.
% The image used to be dropped altogether, the visible span being clipped
% against an interval read as if it always ran upwards.
% The figure must stay hidden: a visible figure is captured through OpenGL.
%=============================================================================
ROWBANDS = zeros(16, 16, 3, 'uint8');
ROWBANDS(1:4, :, 1) = 255; % red on the FIRST rows
ROWBANDS(13:16, :, 3) = 255; % blue on the LAST rows
COLBANDS = zeros(16, 16, 3, 'uint8');
COLBANDS(:, 1:4, 1) = 255; % red on the FIRST columns
COLBANDS(:, 13:16, 3) = 255; % blue on the LAST columns
%=============================================================================
for tc = 1:4
  f = figure('Visible', 'off', 'Position', [10 10 400 300]);
  ax = axes('Parent', f);
  if (tc == 4)
    C = COLBANDS;
  else
    C = ROWBANDS;
  end
  im = image('Parent', ax, 'CData', C);
  axis(ax, 'off');
  im.XData = [0 1];
  im.YData = [0 1];
  if (tc == 1)
    caseName = 'YData upwards';
  elseif (tc == 2)
    caseName = 'YData downwards';
    im.YData = [1 0];
  elseif (tc == 3)
    caseName = 'YData upwards, YDir reverse';
    ax.YDir = 'reverse';
  else
    caseName = 'XData backwards';
    im.XData = [1 0];
  end
  ax.XLim = [0 1];
  ax.YLim = [0 1];
  drawnow();

  F = getframe(f);
  img = F.cdata;
  R = double(img(:, :, 1));
  G = double(img(:, :, 2));
  B = double(img(:, :, 3));
  isRed = (R > 150) & (G < 100) & (B < 100);
  isBlue = (B > 150) & (G < 100) & (R < 100);
  assert_istrue(any(isRed(:)), ['red band not painted (', caseName, ')']);
  assert_istrue(any(isBlue(:)), ['blue band not painted (', caseName, ')']);

  if (tc == 4)
    redMid = mean(find(sum(isRed, 1) > 0));
    blueMid = mean(find(sum(isBlue, 1) > 0));
    assert_istrue(redMid > blueMid, ['column 1 is not on the right (', caseName, ')']);
  else
    redMid = mean(find(sum(isRed, 2) > 0));
    blueMid = mean(find(sum(isBlue, 2) > 0));
    if (tc == 1)
      assert_istrue(redMid > blueMid, ['row 1 is not at the bottom (', caseName, ')']);
    else
      assert_istrue(redMid < blueMid, ['row 1 is not at the top (', caseName, ')']);
    end
  end
  close(f);
end
%=============================================================================
