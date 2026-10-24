%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function boing_ball_3d()
  % Animate a checkered ball from elapsed time until the window is closed.
  backgroundColor = [0.025 0.035 0.065];
  floorColor = [0.105 0.135 0.215];
  gridColor = [0.185 0.245 0.365];
  ballRadius = 1.25;

  figureHandle = uifigure('Color', backgroundColor, ...
    'Name', 'Realtime 3D Boing Ball', ...
    'GraphicsSmoothing', 'on');
  set(figureHandle, 'HandleVisibility', 'on');
  axesHandle = uiaxes(figureHandle, 'Color', backgroundColor, ...
    'Units', 'normalized', 'Position', [0 0.08 1 0.92]);
  hold(axesHandle, 'on');

  wallY = 5.0;
  boingBallDrawRoom(axesHandle, wallY, floorColor, gridColor);

  shadowAngle = linspace(0, 2 * pi, 65);
  floorShadowBaseX = ballRadius * cos(shadowAngle);
  floorShadowBaseY = 0.58 * ballRadius * sin(shadowAngle);
  floorShadowHandle = patch('Parent', axesHandle, 'XData', floorShadowBaseX, ...
    'YData', floorShadowBaseY, 'ZData', 0.012 * ones(size(shadowAngle)), ...
    'FaceColor', [0 0 0], 'FaceAlpha', 0.38, 'FaceLighting', 'none', ...
    'EdgeColor', 'none');

  wallShadowBaseX = 0.86 * ballRadius * cos(shadowAngle);
  wallShadowBaseZ = 1.03 * ballRadius * sin(shadowAngle);
  wallShadowHandle = patch('Parent', axesHandle, ...
    'XData', wallShadowBaseX + 0.32, ...
    'YData', (wallY - 0.018) * ones(size(shadowAngle)), ...
    'ZData', wallShadowBaseZ + ballRadius + 0.18, ...
    'FaceColor', [0 0 0], 'FaceAlpha', 0.28, 'FaceLighting', 'none', ...
    'EdgeColor', 'none');

  [baseX, baseY, baseZ, checkerPattern] = boingBallMesh(ballRadius);
  ballHandle = surf(axesHandle, baseX, baseY, baseZ + ballRadius, ...
    checkerPattern, 'EdgeColor', 'none', 'FaceColor', 'flat', ...
    'FaceLighting', 'gouraud', 'CDataMapping', 'direct', ...
    'AmbientStrength', 0.34, 'DiffuseStrength', 0.72, ...
    'SpecularStrength', 0.38, 'SpecularExponent', 24, ...
    'SpecularColorReflectance', 0.45);
  colormap(axesHandle, [0.92 0.025 0.040; 1.00 0.96 0.84]);
  light(axesHandle, 'Position', [-4 -3 7], 'Color', [1.00 0.90 0.78], ...
    'Style', 'local');
  light(axesHandle, 'Position', [4 -1 4], 'Color', [0.26 0.42 0.78], ...
    'Style', 'local');

  hold(axesHandle, 'off');
  axis(axesHandle, 'equal');
  axis(axesHandle, [-6.0 6.0 -5.0 5.0 0 5.8]);
  axis(axesHandle, 'off');
  set(axesHandle, 'Projection', 'perspective');
  view(axesHandle, 32, 18);
  title(axesHandle, 'REALTIME 3D BOING BALL', 'Color', [0.88 0.92 1.00], ...
    'FontSize', 16, 'FontWeight', 'bold');

  hintText = uicontrol('Parent', figureHandle, 'Style', 'text', ...
    'String', 'Click to stop or resume', ...
    'ForegroundColor', [0.62 0.70 0.86], 'BackgroundColor', backgroundColor, ...
    'HorizontalAlignment', 'left', 'FontSize', 12, 'Units', 'normalized', ...
    'Position', [0.01 0.018 0.42 0.045]);
  statusText = uicontrol('Parent', figureHandle, 'Style', 'text', ...
    'String', 'Running - measuring FPS', ...
    'ForegroundColor', [0.35 0.90 0.58], 'BackgroundColor', backgroundColor, ...
    'HorizontalAlignment', 'right', 'FontSize', 12, 'Units', 'normalized', ...
    'Position', [0.57 0.018 0.42 0.045]);
  runToggle = uicontrol('Parent', figureHandle, 'Style', 'togglebutton', ...
    'String', 'Start / Stop', 'Value', 0, 'FontSize', 12, 'Units', 'normalized', ...
    'Position', [0.442 0.018 0.116 0.045]);

  animationState = struct('figure', figureHandle, 'axes', axesHandle, ...
    'ball', ballHandle, ...
    'floorShadow', floorShadowHandle, 'wallShadow', wallShadowHandle, ...
    'hintText', hintText, 'statusText', statusText, 'runToggle', runToggle, ...
    'baseX', baseX, 'baseY', baseY, 'baseZ', baseZ, ...
    'floorShadowBaseX', floorShadowBaseX, ...
    'floorShadowBaseY', floorShadowBaseY, ...
    'wallShadowBaseX', wallShadowBaseX, ...
    'wallShadowBaseZ', wallShadowBaseZ, 'radius', ballRadius, ...
    'bounceHeight', 2.8, 'bouncePeriod', 1.25, ...
    'horizontalLimit', 2.55, 'horizontalHalfPeriod', 2.6);
  boingBallAnimate(animationState, statusText);
end

function boingBallDrawRoom(axesHandle, wallY, floorColor, gridColor)
  % The rear wall shares the floor's back edge and full room width.
  wallHalfWidth = 6.0;
  patch('Parent', axesHandle, ...
    'XData', [-wallHalfWidth wallHalfWidth wallHalfWidth -wallHalfWidth], ...
    'YData', wallY * ones(1, 4), 'ZData', [0 0 5.8 5.8], ...
    'FaceColor', [0.085 0.105 0.155], 'FaceLighting', 'none', ...
    'EdgeColor', 'none', 'Clipping', 'off');
  wallGridX = zeros(4, 0);
  wallGridY = zeros(4, 0);
  wallGridZ = zeros(4, 0);
  gridThickness = 0.012;
  wallGridPlane = wallY - 0.018;
  for gridHeight = 0:0.65:5.8
    wallGridX(:, end + 1) = [-wallHalfWidth; wallHalfWidth; wallHalfWidth; -wallHalfWidth];
    wallGridY(:, end + 1) = wallGridPlane * ones(4, 1);
    wallGridZ(:, end + 1) = gridHeight + ...
      [-gridThickness; -gridThickness; gridThickness; gridThickness];
  end
  for gridPosition = -4:0.8:4
    wallGridX(:, end + 1) = gridPosition + ...
      [-gridThickness; gridThickness; gridThickness; -gridThickness];
    wallGridY(:, end + 1) = wallGridPlane * ones(4, 1);
    wallGridZ(:, end + 1) = [0; 0; 5.8; 5.8];
  end
  patch('Parent', axesHandle, 'XData', wallGridX, 'YData', wallGridY, ...
    'ZData', wallGridZ, 'FaceColor', gridColor, 'FaceLighting', 'none', ...
    'EdgeColor', 'none', 'Clipping', 'off');

  patch('Parent', axesHandle, 'XData', [-6 6 6 -6], ...
    'YData', [-5 -5 5 5], 'ZData', [0 0 0 0], ...
    'FaceColor', floorColor, 'FaceLighting', 'none', 'EdgeColor', 'none');
  floorGridX = zeros(4, 0);
  floorGridY = zeros(4, 0);
  floorGridZ = zeros(4, 0);
  for gridValue = -5:1:5
    floorGridX(:, end + 1) = [-6; 6; 6; -6];
    floorGridY(:, end + 1) = gridValue + ...
      [-gridThickness; -gridThickness; gridThickness; gridThickness];
    floorGridX(:, end + 1) = gridValue + ...
      [-gridThickness; gridThickness; gridThickness; -gridThickness];
    floorGridY(:, end + 1) = [-5; -5; 5; 5];
    floorGridZ(:, end + 1:end + 2) = 0.006 * ones(4, 2);
  end
  patch('Parent', axesHandle, 'XData', floorGridX, 'YData', floorGridY, ...
    'ZData', floorGridZ, 'FaceColor', gridColor, 'FaceLighting', 'none', ...
    'EdgeColor', 'none');
end

function boingBallAnimate(animationState, statusText)
  targetFrameRate = 63;
  targetFrameDuration = 1 / targetFrameRate;
  animationTime = 0;
  wallTimer = tic();
  previousWallTime = 0;
  fpsWindowStart = 0;
  renderedFrames = 0;
  measuredFps = 0;
  wasPaused = false;
  while boingBallHandlesValid(animationState)
    frameTimer = tic();
    currentWallTime = toc(wallTimer);
    wallStep = currentWallTime - previousWallTime;
    previousWallTime = currentWallTime;
    paused = get(animationState.runToggle, 'Value') ~= 0;
    if paused
      if ~wasPaused
        set(statusText, 'String', 'Stopped', ...
          'ForegroundColor', [1.00 0.68 0.28]);
      end
      fpsWindowStart = currentWallTime;
      renderedFrames = 0;
    elseif wasPaused
      measuredFps = 0;
      fpsWindowStart = currentWallTime;
      renderedFrames = 0;
      set(statusText, 'String', 'Running - measuring FPS', ...
        'ForegroundColor', [0.35 0.90 0.58]);
    end
    if ~paused
      animationTime = animationTime + wallStep;
      boingBallDrawFrame(animationState, animationTime);
      renderedFrames = renderedFrames + 1;
      fpsWindowDuration = currentWallTime - fpsWindowStart;
      if fpsWindowDuration >= 1
        measuredFps = renderedFrames / fpsWindowDuration;
        set(statusText, 'String', sprintf('Running - %.1f FPS', measuredFps), ...
          'ForegroundColor', [0.35 0.90 0.58]);
        fpsWindowStart = currentWallTime;
        renderedFrames = 0;
      end
    end
    wasPaused = paused;
    drawnow();
    if paused
      pause(0.02);
      continue
    end
    remainingFrameTime = targetFrameDuration - toc(frameTimer);
    spinMargin = 0.0015;
    if remainingFrameTime > spinMargin
      pause(remainingFrameTime - spinMargin);
    end
    while toc(frameTimer) < targetFrameDuration
    end
  end
end

function [x, y, z, checkerPattern] = boingBallMesh(radius)
  longitude = linspace(0, 2 * pi, 49);
  latitude = linspace(-pi / 2, pi / 2, 25).';
  longitudeGrid = repmat(longitude, numel(latitude), 1);
  latitudeGrid = repmat(latitude, 1, numel(longitude));
  x = radius * cos(latitudeGrid) .* cos(longitudeGrid);
  y = radius * cos(latitudeGrid) .* sin(longitudeGrid);
  z = radius * sin(latitudeGrid);
  longitudeSquare = floor(16 * longitudeGrid / (2 * pi));
  latitudeSquare = floor(8 * (latitudeGrid + pi / 2) / pi);
  checkerPattern = 1 + mod(longitudeSquare + latitudeSquare, 2);
end

function boingBallDrawFrame(state, elapsedSeconds)
  bouncePhase = mod(elapsedSeconds, state.bouncePeriod) / state.bouncePeriod;
  normalizedHeight = sin(pi * bouncePhase);
  height = state.bounceHeight * normalizedHeight;
  impact = exp(-(height / (0.18 * state.bounceHeight)) .^ 2);
  verticalScale = 1 - 0.08 * impact;
  horizontalScale = 1 / sqrt(verticalScale);

  horizontalPhase = mod(elapsedSeconds, 2 * state.horizontalHalfPeriod) / ...
    state.horizontalHalfPeriod;
  if horizontalPhase <= 1
    horizontalPosition = -1 + 2 * horizontalPhase;
  else
    horizontalPosition = 3 - 2 * horizontalPhase;
  end
  ballX = state.horizontalLimit * horizontalPosition;

  spinAngle = -ballX / state.radius;
  spinCosine = cos(spinAngle);
  spinSine = sin(spinAngle);
  spunX = spinCosine * state.baseX + spinSine * state.baseZ;
  spunZ = -spinSine * state.baseX + spinCosine * state.baseZ;
  ballZ = state.radius * verticalScale + height;
  set(state.ball, 'XData', horizontalScale * spunX + ballX, ...
    'YData', horizontalScale * state.baseY, ...
    'ZData', verticalScale * spunZ + ballZ);

  shadowScale = 1.32 - 0.34 * normalizedHeight;
  shadowAlpha = 0.40 - 0.27 * normalizedHeight;
  set(state.floorShadow, ...
    'XData', ballX + shadowScale * state.floorShadowBaseX, ...
    'YData', shadowScale * state.floorShadowBaseY, 'FaceAlpha', shadowAlpha);
  set(state.wallShadow, 'XData', ballX + state.wallShadowBaseX + 0.32, ...
    'ZData', ballZ + state.wallShadowBaseZ + 0.18);
end

function valid = boingBallHandlesValid(state)
  valid = isgraphics(state.figure) && isgraphics(state.axes) && ...
    isgraphics(state.ball) && ...
    isgraphics(state.floorShadow) && isgraphics(state.wallShadow) && ...
    isgraphics(state.hintText) && isgraphics(state.statusText) && ...
    isgraphics(state.runToggle);
end
%=============================================================================
