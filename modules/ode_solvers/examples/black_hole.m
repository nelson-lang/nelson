%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Orbit a point mass on the curved "black hole" funnel z = -1/r and let the
% user relaunch it from an interactive control panel (sliders + buttons).
%=============================================================================
if ~exist('blackHoleFigureVisible', 'var')
  blackHoleFigureVisible = 'on';
end
% Batch mode: run one trajectory with the default settings, animate a limited
% number of frames, then return (used for non-interactive testing / capture).
if ~exist('blackHoleBatch', 'var')
  blackHoleBatch = false;
end
if ~exist('blackHoleMaxFrames', 'var')
  blackHoleMaxFrames = 520;
end
if ~exist('blackHoleFramePause', 'var')
  blackHoleFramePause = 0;
end
if ~exist('blackHoleTrailLength', 'var')
  blackHoleTrailLength = 320;
end
if ~exist('blackHoleOutputImage', 'var')
  blackHoleOutputImage = '';
end

blackHoleSoftening = 0.12;
blackHoleGravity = 9.81;
blackHoleInnerRadius = 0.16;
blackHoleOuterRadius = 2.20;

% --- Figure -----------------------------------------------------------------
blackHoleFigure = figure('Name', 'Black hole', 'NumberTitle', 'off', ...
  'Color', [0.05 0.05 0.09], 'Visible', blackHoleFigureVisible, ...
  'Position', [80 80 1000 660]);
blackHoleAxes = axes('Parent', blackHoleFigure, ...
  'Position', [0.05 0.24 0.92 0.72], 'Color', [0.05 0.05 0.09]);

% --- Static funnel surface z = -1 / sqrt(x^2 + y^2 + soft^2) -----------------
blackHoleSurfaceRadius = linspace(0.14, blackHoleOuterRadius, 46);
blackHoleSurfaceAngle = linspace(0, 2 * pi, 60);
[blackHoleRadiusGrid, blackHoleAngleGrid] = ...
  meshgrid(blackHoleSurfaceRadius, blackHoleSurfaceAngle);
blackHoleSurfaceX = blackHoleRadiusGrid .* cos(blackHoleAngleGrid);
blackHoleSurfaceY = blackHoleRadiusGrid .* sin(blackHoleAngleGrid);
blackHoleSurfaceZ = blackHoleFunnelHeight(blackHoleSurfaceX, ...
  blackHoleSurfaceY, blackHoleSoftening);
hold(blackHoleAxes, 'on');
% Semi-transparent, gouraud-lit funnel: the renderer caches the lit static
% surface between frames, so the visual quality costs nothing per frame.
surf(blackHoleAxes, blackHoleSurfaceX, blackHoleSurfaceY, blackHoleSurfaceZ, ...
  'EdgeColor', 'none', 'FaceAlpha', 0.9, 'FaceLighting', 'gouraud');
light(blackHoleAxes, 'Position', [3 -4 3]);
colormap(blackHoleAxes, turbo(256));
blackHoleTrail = plot3(blackHoleAxes, NaN, NaN, NaN, ...
  'Color', [1.00 0.95 0.55], 'LineWidth', 1.8);
blackHoleParticle = plot3(blackHoleAxes, NaN, NaN, NaN, 'o', ...
  'MarkerFaceColor', [1.00 1.00 1.00], 'MarkerEdgeColor', [1.00 0.75 0.25], ...
  'MarkerSize', 9, 'LineStyle', 'none');
hold(blackHoleAxes, 'off');
axis(blackHoleAxes, [-blackHoleOuterRadius blackHoleOuterRadius ...
  -blackHoleOuterRadius blackHoleOuterRadius -1 / blackHoleSoftening 0]);
set(blackHoleAxes, 'XColor', [0.7 0.7 0.8], 'YColor', [0.7 0.7 0.8], ...
  'ZColor', [0.7 0.7 0.8]);
xlabel(blackHoleAxes, 'x');
ylabel(blackHoleAxes, 'y');
zlabel(blackHoleAxes, 'z = -1 / r');
view(blackHoleAxes, [-37.5 42]);
% Keep the title text handle: updating its String is much cheaper per frame
% than calling title() again (which re-runs the axes layout).
blackHoleTitleText = title(blackHoleAxes, ...
  'Set the launch conditions, then press Start', 'Color', [0.92 0.92 1.00]);

% --- Control panel ----------------------------------------------------------
blackHoleRadiusSlider = uicontrol('Parent', blackHoleFigure, ...
  'Style', 'slider', 'Min', 0.35, 'Max', 1.95, 'Value', 1.45, ...
  'Position', [60 70 170 20]);
blackHoleSpeedSlider = uicontrol('Parent', blackHoleFigure, ...
  'Style', 'slider', 'Min', 0.0, 'Max', 6.0, 'Value', 2.0, ...
  'Position', [260 70 170 20]);
blackHoleDirectionSlider = uicontrol('Parent', blackHoleFigure, ...
  'Style', 'slider', 'Min', -80, 'Max', 80, 'Value', 12, ...
  'Position', [460 70 170 20]);
blackHoleRadiusLabel = uicontrol('Parent', blackHoleFigure, 'Style', 'text', ...
  'Position', [60 92 170 18], 'String', 'Start radius');
blackHoleSpeedLabel = uicontrol('Parent', blackHoleFigure, 'Style', 'text', ...
  'Position', [260 92 170 18], 'String', 'Launch speed');
blackHoleDirectionLabel = uicontrol('Parent', blackHoleFigure, ...
  'Style', 'text', 'Position', [460 92 170 18], ...
  'String', 'Direction (deg from tangent)');
blackHoleReadout = uicontrol('Parent', blackHoleFigure, 'Style', 'text', ...
  'Position', [60 44 570 18], 'String', '');
blackHoleStartButton = uicontrol('Parent', blackHoleFigure, ...
  'Style', 'pushbutton', 'String', 'Start', 'Position', [680 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
blackHoleResetButton = uicontrol('Parent', blackHoleFigure, ...
  'Style', 'pushbutton', 'String', 'Reset', 'Position', [780 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
blackHoleStopButton = uicontrol('Parent', blackHoleFigure, ...
  'Style', 'pushbutton', 'String', 'Stop', 'Position', [880 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));

blackHoleControls = struct();
blackHoleControls.figure = blackHoleFigure;
blackHoleControls.axes = blackHoleAxes;
blackHoleControls.trail = blackHoleTrail;
blackHoleControls.particle = blackHoleParticle;
blackHoleControls.readout = blackHoleReadout;
blackHoleControls.titleText = blackHoleTitleText;
blackHoleControls.startButton = blackHoleStartButton;
blackHoleControls.resetButton = blackHoleResetButton;
blackHoleControls.stopButton = blackHoleStopButton;
blackHoleControls.gravity = blackHoleGravity;
blackHoleControls.softening = blackHoleSoftening;
blackHoleControls.innerRadius = blackHoleInnerRadius;
blackHoleControls.outerRadius = blackHoleOuterRadius;
blackHoleControls.maxFrames = blackHoleMaxFrames;
blackHoleControls.framePause = blackHoleFramePause;
blackHoleControls.trailLength = blackHoleTrailLength;

if blackHoleBatch
  % One deterministic run for testing / off-screen capture.
  blackHoleStreamTrajectory(blackHoleControls, 1.45, 2.0, 12);
  if ~isempty(blackHoleOutputImage) && isgraphics(blackHoleFigure)
    drawnow();
    saveas(blackHoleFigure, blackHoleOutputImage);
  end
else
  % Interactive loop: wait for Start, integrate, animate, repeat.
  while isgraphics(blackHoleFigure)
    if blackHoleStopButton.UserData
      % Stop interrupts the current run but keeps the panel live so Start /
      % Reset stay usable; the window close button ends the demo.
      set(blackHoleStopButton, 'UserData', false);
    end
    if blackHoleResetButton.UserData
      set(blackHoleResetButton, 'UserData', false);
      set(blackHoleTrail, 'XData', NaN, 'YData', NaN, 'ZData', NaN);
      set(blackHoleParticle, 'XData', NaN, 'YData', NaN, 'ZData', NaN);
      set(blackHoleTitleText, 'String', ...
        'Set the launch conditions, then press Start');
    end
    if blackHoleStartButton.UserData
      set(blackHoleStartButton, 'UserData', false);
      % Brief feedback while the first integration window runs; the streamed
      % animation takes over as soon as its samples are available.
      set(blackHoleTitleText, 'String', 'Integrating the trajectory ...');
      set(blackHoleReadout, 'String', 'computing the orbit, please wait');
      drawnow();
      blackHoleStreamTrajectory(blackHoleControls, ...
        blackHoleRadiusSlider.Value, blackHoleSpeedSlider.Value, ...
        blackHoleDirectionSlider.Value);
    end
    drawnow();
    pause(0.03);
  end
end
%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================
function height = blackHoleFunnelHeight(x, y, softening)
  % Softened gravitational well; finite everywhere so the surface and the
  % dynamics stay numerically well behaved near the centre.
  height = -1 ./ sqrt(x .^ 2 + y .^ 2 + softening .^ 2);
end
%=============================================================================
function derivative = blackHoleDynamics(~, state, gravity, softening)
  % State = [x; xdot; y; ydot]. Lagrange equations for a mass sliding on the
  % surface z = f(x, y): the normal reaction is eliminated analytically.
  x = state(1);
  velocityX = state(2);
  y = state(3);
  velocityY = state(4);
  radiusSquared = x .^ 2 + y .^ 2 + softening .^ 2;
  slopeX = x * radiusSquared ^ (-1.5);
  slopeY = y * radiusSquared ^ (-1.5);
  curvatureXX = (radiusSquared - 3 * x ^ 2) * radiusSquared ^ (-2.5);
  curvatureXY = -3 * x * y * radiusSquared ^ (-2.5);
  curvatureYY = (radiusSquared - 3 * y ^ 2) * radiusSquared ^ (-2.5);
  quadraticForm = curvatureXX * velocityX ^ 2 + ...
    2 * curvatureXY * velocityX * velocityY + curvatureYY * velocityY ^ 2;
  multiplier = (gravity + quadraticForm) / (1 + slopeX ^ 2 + slopeY ^ 2);
  accelerationX = -multiplier * slopeX;
  accelerationY = -multiplier * slopeY;
  derivative = [velocityX; accelerationX; velocityY; accelerationY];
end
%=============================================================================
function [value, isTerminal, direction] = blackHoleCaptureEvent(~, state, ...
  innerRadius, outerRadius)
  radius = hypot(state(1), state(3));
  % Stop when the bead is swallowed by the centre or escapes the funnel.
  value = [radius - innerRadius; outerRadius - radius];
  isTerminal = [1; 1];
  direction = [-1; -1];
end
%=============================================================================
function blackHoleShowFrame(controls, pathX, pathY, pathZ, sampleIndex)
  % Draw only a trailing window of the orbit each frame: the per-frame redraw
  % cost stays constant instead of growing with the whole path (the dominant
  % on-screen cost is redrawing the polyline, which scales with its length).
  trailStart = max(1, sampleIndex - controls.trailLength + 1);
  set(controls.trail, 'XData', pathX(trailStart:sampleIndex), ...
    'YData', pathY(trailStart:sampleIndex), 'ZData', pathZ(trailStart:sampleIndex));
  set(controls.particle, 'XData', pathX(sampleIndex), ...
    'YData', pathY(sampleIndex), 'ZData', pathZ(sampleIndex));
  currentRadius = hypot(pathX(sampleIndex), pathY(sampleIndex));
  set(controls.readout, 'String', sprintf(...
    'orbit radius = %.3f    depth z = %.3f', currentRadius, pathZ(sampleIndex)));
  set(controls.titleText, 'String', ...
    sprintf('Falling into the well  (r = %.2f)', currentRadius));
  drawnow();
  if controls.framePause > 0
    pause(controls.framePause);
  end
end
%=============================================================================
function blackHoleFinishAnimation(controls, pathX, pathY, pathZ)
  if ~blackHoleAnimationAlive(controls)
    return
  end
  set(controls.trail, 'XData', pathX, 'YData', pathY, 'ZData', pathZ);
  set(controls.particle, 'XData', pathX(end), 'YData', pathY(end), ...
    'ZData', pathZ(end));
  finalRadius = hypot(pathX(end), pathY(end));
  if finalRadius <= controls.innerRadius + 1e-3
    outcome = 'Swallowed by the centre';
  elseif finalRadius >= controls.outerRadius - 1e-3
    outcome = 'Escaped the funnel';
  else
    outcome = 'Bound precessing orbit';
  end
  set(controls.titleText, 'String', ...
    [outcome, ' - press Start to launch again']);
  drawnow();
end
%=============================================================================
function blackHoleStreamTrajectory(controls, startRadius, launchSpeed, ...
  directionDeg)
  % Integrate the trajectory in short time windows and animate each window as
  % soon as its samples are available: the bead starts moving a couple of
  % seconds after Start instead of only once the whole orbit is integrated
  % (the interpreted right-hand side makes a single 45 s solve take a while).
  directionRad = directionDeg * pi / 180;
  state = [startRadius; launchSpeed * sin(directionRad); 0; ...
    launchSpeed * cos(directionRad)];
  solverOptions = odeset('RelTol', 1e-6, 'AbsTol', 1e-8, 'MaxStep', 0.25, ...
    'Events', @(t, s) blackHoleCaptureEvent(t, s, controls.innerRadius, ...
    controls.outerRadius));
  sampleStep = 0.01;
  windowSeconds = 1.0;
  endTime = 45;
  totalSamples = round(endTime / sampleStep) + 1;
  frameStride = max(1, ceil(totalSamples / controls.maxFrames));
  pathX = [];
  pathY = [];
  pathZ = [];
  nextFrameIndex = 1;
  for windowStart = 0:windowSeconds:(endTime - windowSeconds)
    if ~blackHoleAnimationAlive(controls)
      return
    end
    windowSpan = windowStart:sampleStep:(windowStart + windowSeconds);
    [~, stateHistory] = ode45(@(t, s) blackHoleDynamics(t, s, ...
      controls.gravity, controls.softening), windowSpan, state, solverOptions);
    % The first sample of a window repeats the carried state.
    firstNew = 1;
    if ~isempty(pathX)
      firstNew = 2;
    end
    newX = stateHistory(firstNew:end, 1);
    newY = stateHistory(firstNew:end, 3);
    pathX = [pathX; newX];
    pathY = [pathY; newY];
    pathZ = [pathZ; blackHoleFunnelHeight(newX, newY, controls.softening)];
    state = stateHistory(end, :)';
    % A terminal event (captured or escaped) ends the window early.
    terminated = size(stateHistory, 1) < numel(windowSpan);
    while nextFrameIndex <= numel(pathX)
      if ~blackHoleAnimationAlive(controls)
        return
      end
      blackHoleShowFrame(controls, pathX, pathY, pathZ, nextFrameIndex);
      nextFrameIndex = nextFrameIndex + frameStride;
    end
    if terminated
      break
    end
  end
  if numel(pathX) >= 2
    blackHoleFinishAnimation(controls, pathX, pathY, pathZ);
  end
end
%=============================================================================
function alive = blackHoleAnimationAlive(controls)
  alive = isgraphics(controls.figure) && isgraphics(controls.trail) && ...
    isgraphics(controls.particle);
  if ~alive
    return
  end
  % Let Stop / Reset interrupt a running animation immediately.
  if isgraphics(controls.stopButton) && controls.stopButton.UserData
    alive = false;
  end
  if isgraphics(controls.resetButton) && controls.resetButton.UserData
    alive = false;
  end
end
%=============================================================================
