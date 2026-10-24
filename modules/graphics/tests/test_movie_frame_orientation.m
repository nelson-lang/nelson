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
skip_if_portable_graphics_backend('movie frame raster orientation differs in portable rendering.');
% movie() paints row 1 of a frame at the TOP of the axes, keeps YDir as it
% found it and gives the axes limits back untouched.
% The figure must stay hidden: a visible figure is captured through OpenGL,
% which paints the frame the right way up even when the data says otherwise.
%=============================================================================
C = zeros(16, 16, 3, 'uint8');
C(1:4, :, 1) = 255; % red band on the FIRST rows of CData
C(13:16, :, 3) = 255; % blue band on the LAST rows of CData
clear('M');
M(1) = struct('cdata', C, 'colormap', []);
M(2) = struct('cdata', C, 'colormap', []);
%=============================================================================
for tc = 1:3
  f = figure('Visible', 'off', 'Position', [10 10 400 300]);
  ax = axes('Parent', f);
  if (tc == 1)
    caseName = 'current axes';
    movie(M, 1, 24);
  elseif (tc == 2)
    caseName = 'explicit axes';
    movie(ax, M, 1, 24);
  else
    caseName = 'replayed on the same axes';
    movie(M, 1, 24);
    movie(M, 1, 24);
  end

  F = getframe(f);
  img = F.cdata;
  R = double(img(:, :, 1));
  G = double(img(:, :, 2));
  B = double(img(:, :, 3));
  redRows = find(sum((R > 150) & (G < 100) & (B < 100), 2) > 0);
  blueRows = find(sum((B > 150) & (G < 100) & (R < 100), 2) > 0);

  assert_istrue(~isempty(redRows), ['red band not painted (', caseName, ')']);
  assert_istrue(~isempty(blueRows), ['blue band not painted (', caseName, ')']);
  assert_istrue(mean(redRows) < mean(blueRows), ...
    ['frame painted upside down (', caseName, ')']);

  assert_isequal(ax.YDir, 'normal');
  assert_isequal(ax.XLim, [0 1]);
  assert_isequal(ax.YLim, [0 1]);

  % replaying recycles the frame instead of stacking a new image on the axes
  assert_isequal(numel(findobj(ax, 'Tag', 'MovieFrame')), 1);

  close(f);
end
%=============================================================================
