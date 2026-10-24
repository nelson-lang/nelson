%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
internalWidth = 160;
internalHeight = 106;
frameCount = 120;
warmupCount = 12;
screenSize = get(0, 'ScreenSize');
screenWidth = screenSize(3);
screenHeight = screenSize(4);
if screenWidth < 1 || screenHeight < 1
  screenWidth = 1920;
  screenHeight = 1080;
end
windowSizes = [480 320; 960 640; screenWidth screenHeight];

fprintf('Fixed CData: %dx%dx3, frames: %d, screen: %dx%d\n', ...
  internalWidth, internalHeight, frameCount, screenWidth, screenHeight);
fprintf(['window       DPR   compute ms   convert ms   request ms   ' ...
  'repaint/sync ms   idle sync ms   FPS\n']);

conversionFigure = figure('Visible', 'off');
conversionAxes = axes('Parent', conversionFigure);
conversionFrame = zeros(internalHeight, internalWidth, 3);
conversionImage = image('Parent', conversionAxes, 'CData', conversionFrame);
conversionSeconds = 0;
for frameIndex = 1:frameCount
  conversionFrame(:, :, 1) = mod(frameIndex, 32) / 31;
  timer = tic();
  set(conversionImage, 'CData', conversionFrame);
  conversionSeconds = conversionSeconds + toc(timer);
end
close(conversionFigure);
conversionMilliseconds = 1000 * conversionSeconds / frameCount;

conversionFigure = figure('Visible', 'off');
conversionAxes = axes('Parent', conversionFigure);
conversionFrame8 = uint8(zeros(internalHeight, internalWidth, 3));
conversionImage = image('Parent', conversionAxes, 'CData', conversionFrame8);
conversionSeconds8 = 0;
for frameIndex = 1:frameCount
  conversionFrame8(:, :, 1) = uint8(mod(frameIndex, 32) * 255 / 31);
  timer = tic();
  set(conversionImage, 'CData', conversionFrame8);
  conversionSeconds8 = conversionSeconds8 + toc(timer);
end
close(conversionFigure);
conversionMilliseconds8 = 1000 * conversionSeconds8 / frameCount;
fprintf('CData conversion only: double %.3f ms, uint8 %.3f ms\n', ...
  conversionMilliseconds, conversionMilliseconds8);

for sizeIndex = 1:size(windowSizes, 1)
  windowWidth = windowSizes(sizeIndex, 1);
  windowHeight = windowSizes(sizeIndex, 2);
  f = figure('Visible', 'on', 'MenuBar', 'none', 'ToolBar', 'none', ...
    'Position', [80 80 windowWidth windowHeight], 'GraphicsSmoothing', 'on');
  if sizeIndex == size(windowSizes, 1)
    set(f, 'WindowState', 'fullscreen');
  end
  ax = axes('Parent', f, 'Position', [0 0 1 1]);
  frame = zeros(internalHeight, internalWidth, 3);
  imageHandle = image('Parent', ax, 'CData', frame);
  axis(ax, 'image');
  axis(ax, 'off');
  drawnow();

  for frameIndex = 1:warmupCount
    frame(:, :, 1) = mod(frameIndex, 2);
    set(imageHandle, 'CData', frame);
    drawnow();
  end

  computeSeconds = 0;
  setSeconds = 0;
  drawSeconds = 0;
  idleSyncSeconds = 0;
  for frameIndex = 1:frameCount
    timer = tic();
    phase = mod(frameIndex, 32) / 31;
    frame(:, :, 1) = phase;
    frame(:, :, 2) = 1 - phase;
    frame(:, :, 3) = 0.25 + 0.5 * phase;
    computeSeconds = computeSeconds + toc(timer);

    timer = tic();
    set(imageHandle, 'CData', frame);
    setSeconds = setSeconds + toc(timer);

    timer = tic();
    drawnow();
    drawSeconds = drawSeconds + toc(timer);

    timer = tic();
    drawnow();
    idleSyncSeconds = idleSyncSeconds + toc(timer);
  end

  dpr = get(f, 'DevicePixelRatio');
  position = get(f, 'Position');
  setMilliseconds = 1000 * setSeconds / frameCount;
  fprintf(['%4dx%-4d  %3.1f      %7.3f      %7.3f      %7.3f          ' ...
    '%7.3f          %7.3f   %5.1f\n'], position(3), position(4), dpr, ...
    1000 * computeSeconds / frameCount, conversionMilliseconds, ...
    max(0, setMilliseconds - conversionMilliseconds), ...
    1000 * drawSeconds / frameCount, 1000 * idleSyncSeconds / frameCount, ...
    frameCount / (computeSeconds + setSeconds + drawSeconds));
  close(f);
end
close all
%=============================================================================
