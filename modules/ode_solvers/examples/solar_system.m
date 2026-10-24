%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Precompute a softened 3D N-body solar system with ode45, then animate the
% planets on inclined orbits with fading trails and an interactive panel.
%=============================================================================
if ~exist('solarFigureVisible', 'var')
  solarFigureVisible = 'on';
end
% Batch mode: run one deterministic pass, animate a limited number of frames,
% then return (used for non-interactive testing / off-screen capture).
if ~exist('solarBatch', 'var')
  solarBatch = false;
end
if ~exist('solarMaxFrames', 'var')
  solarMaxFrames = 400;
end
if ~exist('solarFramePause', 'var')
  solarFramePause = 0.012;
end
if ~exist('solarOutputImage', 'var')
  solarOutputImage = '';
end

% --- Physical model ---------------------------------------------------------
% G * M is scaled so the central star has GM = 1. Planet masses are tiny, so
% their mutual pull only makes the bound orbits precess slowly.
solarParams = struct();
solarParams.starGM = 1.0;
solarParams.softening = 0.05;
solarParams.radii = [1.00, 1.55, 2.20, 3.00, 3.90, 5.00];
solarParams.inclinations = [0.05, -0.09, 0.14, -0.06, 0.11, -0.15];
solarParams.phases = [0.0, 1.1, 2.3, 3.4, 4.6, 5.7];
solarParams.planetGM = [3.0e-3, 2.2e-3, 4.0e-3, 1.6e-3, 3.4e-3, 2.6e-3];
solarParams.speedFactor = 1.0;
solarParams.count = numel(solarParams.radii);

% Distinct planet colors (bright on the dark background).
solarColors = [...
  0.20 0.60 1.00; ...
  0.95 0.45 0.25; ...
  0.35 0.85 0.45; ...
  0.85 0.35 0.85; ...
  0.95 0.80 0.20; ...
  0.30 0.90 0.90];

% --- Precompute the full coupled trajectory once ----------------------------
solarPath = solarComputeTrajectory(solarParams);

% --- Figure -----------------------------------------------------------------
solarFigure = figure('Name', 'Solar system (N-body gravity)', ...
  'NumberTitle', 'off', 'Color', [0.05 0.05 0.09], ...
  'Visible', solarFigureVisible, 'Position', [80 80 1000 680]);
solarAxes = axes('Parent', solarFigure, 'Position', [0.05 0.08 0.92 0.88], ...
  'Color', [0.05 0.05 0.09]);
hold(solarAxes, 'on');

% Central star: a big yellow marker at the origin.
solarStar = plot3(solarAxes, 0, 0, 0, 'o', ...
  'MarkerFaceColor', [1.00 0.90 0.30], 'MarkerEdgeColor', [1.00 0.70 0.10], ...
  'MarkerSize', 22, 'LineStyle', 'none');

% One trail line and one planet marker per body, created once.
solarTrails = gobjects(1, solarParams.count);
solarPlanets = gobjects(1, solarParams.count);
for solarIndex = 1:solarParams.count
  planetColor = solarColors(solarIndex, :);
  solarTrails(solarIndex) = plot3(solarAxes, NaN, NaN, NaN, ...
    'Color', planetColor, 'LineWidth', 1.4);
  solarPlanets(solarIndex) = plot3(solarAxes, NaN, NaN, NaN, 'o', ...
    'MarkerFaceColor', planetColor, 'MarkerEdgeColor', [0.95 0.95 1.00], ...
    'MarkerSize', 9, 'LineStyle', 'none');
end
hold(solarAxes, 'off');

% Size the cube from the full precomputed trajectory (orbits precess outward
% a little), so every orbit fits inside the box. Use a symmetric range and a
% 1:1:1 data aspect ratio; setting DataAspectRatio (instead of axis equal)
% keeps the limits we ask for instead of letting them be recomputed.
solarExtent = max([max(abs(solarPath.x(:))); max(abs(solarPath.y(:))); ...
  max(abs(solarPath.z(:)))]);
if ~isfinite(solarExtent) || solarExtent <= 0
  solarExtent = max(solarParams.radii);
end
solarLimit = solarExtent * 1.08;
set(solarAxes, 'DataAspectRatio', [1 1 1]);
xlim(solarAxes, [-solarLimit solarLimit]);
ylim(solarAxes, [-solarLimit solarLimit]);
zlim(solarAxes, [-solarLimit solarLimit]);
grid(solarAxes, 'on');
set(solarAxes, 'XColor', [0.7 0.7 0.8], 'YColor', [0.7 0.7 0.8], ...
  'ZColor', [0.7 0.7 0.8]);
xlabel(solarAxes, 'x');
ylabel(solarAxes, 'y');
zlabel(solarAxes, 'z');
view(solarAxes, [-37.5 26]);
title(solarAxes, 'Press Start to launch the orbits', ...
  'Color', [0.92 0.92 1.00]);

% --- Control panel ----------------------------------------------------------
solarTrailSlider = uicontrol('Parent', solarFigure, 'Style', 'slider', ...
  'Min', 20, 'Max', 400, 'Value', 180, 'Position', [60 24 200 20]);
solarTrailLabel = uicontrol('Parent', solarFigure, 'Style', 'text', ...
  'Position', [60 46 200 18], 'String', 'Trail length');
solarStartButton = uicontrol('Parent', solarFigure, 'Style', 'pushbutton', ...
  'String', 'Start', 'Position', [320 24 90 30], 'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
solarResetButton = uicontrol('Parent', solarFigure, 'Style', 'pushbutton', ...
  'String', 'Reset', 'Position', [420 24 90 30], 'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
solarStopButton = uicontrol('Parent', solarFigure, 'Style', 'pushbutton', ...
  'String', 'Stop', 'Position', [520 24 90 30], 'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));

solarControls = struct();
solarControls.figure = solarFigure;
solarControls.axes = solarAxes;
solarControls.star = solarStar;
solarControls.trails = solarTrails;
solarControls.planets = solarPlanets;
solarControls.trailSlider = solarTrailSlider;
solarControls.startButton = solarStartButton;
solarControls.resetButton = solarResetButton;
solarControls.stopButton = solarStopButton;
solarControls.count = solarParams.count;
solarControls.maxFrames = solarMaxFrames;
solarControls.framePause = solarFramePause;

if solarBatch
  % One deterministic run for testing / off-screen capture.
  solarRunAnimation(solarControls, solarPath);
  if ~isempty(solarOutputImage) && isgraphics(solarFigure)
    drawnow();
    saveas(solarFigure, solarOutputImage);
  end
else
  % Interactive loop: wait for Start, animate, allow Reset / Stop, repeat.
  while isgraphics(solarFigure)
    if solarStopButton.UserData
      % Stop interrupts the current run but keeps the panel live so Start /
      % Reset stay usable; the window close button ends the demo.
      set(solarStopButton, 'UserData', false);
    end
    if solarResetButton.UserData
      set(solarResetButton, 'UserData', false);
      solarClearTrails(solarControls);
      title(solarAxes, 'Press Start to launch the orbits', ...
        'Color', [0.92 0.92 1.00]);
    end
    if solarStartButton.UserData
      set(solarStartButton, 'UserData', false);
      solarRunAnimation(solarControls, solarPath);
    end
    drawnow();
    pause(0.03);
  end
end
%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================
function path = solarComputeTrajectory(params)
  % Build the initial state and integrate the softened N-body system once.
  count = params.count;
  positions = zeros(count, 3);
  velocities = zeros(count, 3);
  for index = 1:count
    radius = params.radii(index);
    phase = params.phases(index);
    inclination = params.inclinations(index);
    % Near-circular tangential speed for a bound orbit: v = sqrt(GM / r).
    speed = params.speedFactor * sqrt(params.starGM / radius);
    positions(index, :) = [radius * cos(phase), radius * sin(phase), 0];
    tangential = [-sin(phase), cos(phase), 0];
    velocities(index, :) = speed * (cos(inclination) * tangential + ...
      sin(inclination) * [0, 0, 1]);
  end
  initialState = [reshape(positions, 3 * count, 1); ...
    reshape(velocities, 3 * count, 1)];
  % Cover several orbits of the slowest planet.
  timeSpan = linspace(0, 130, 1600);
  solverOptions = odeset('RelTol', 1e-7, 'AbsTol', 1e-9, 'MaxStep', 0.25);
  [~, stateHistory] = ode45(@(t, state) solarDynamics(t, state, params), ...
    timeSpan, initialState, solverOptions);
  sampleCount = size(stateHistory, 1);
  path = struct();
  path.count = count;
  path.samples = sampleCount;
  path.x = stateHistory(:, 1:count);
  path.y = stateHistory(:, count + 1:2 * count);
  path.z = stateHistory(:, 2 * count + 1:3 * count);
end
%=============================================================================
function derivative = solarDynamics(~, state, params)
  % State = [positions(:); velocities(:)] with positions stored as count x 3.
  % Softened Newtonian gravity from the fixed central star plus every planet.
  count = params.count;
  positions = reshape(state(1:3 * count), count, 3);
  velocities = reshape(state(3 * count + 1:6 * count), count, 3);
  softening2 = params.softening ^ 2;
  % Pull from the central star at the origin.
  starDistance = sqrt(sum(positions .^ 2, 2) + softening2);
  acceleration = -params.starGM * positions ./ (starDistance .^ 3);
  % Mutual planet-planet perturbations.
  for index = 1:count
    offset = positions - positions(index, :);
    distance = sqrt(sum(offset .^ 2, 2) + softening2);
    distance(index) = 1;
    contribution = (params.planetGM(:) ./ (distance .^ 3)) .* offset;
    contribution(index, :) = 0;
    acceleration(index, :) = acceleration(index, :) + sum(contribution, 1);
  end
  derivative = [reshape(velocities, 3 * count, 1); ...
    reshape(acceleration, 3 * count, 1)];
end
%=============================================================================
function solarRunAnimation(controls, path)
  if path.samples < 2
    return
  end
  frameStride = max(1, ceil(path.samples / controls.maxFrames));
  for sampleIndex = 1:frameStride:path.samples
    if ~solarAnimationAlive(controls)
      return
    end
    trailCap = round(controls.trailSlider.Value);
    if trailCap < 2
      trailCap = 2;
    end
    windowStart = max(1, sampleIndex - trailCap + 1);
    for index = 1:controls.count
      trailX = path.x(windowStart:sampleIndex, index);
      trailY = path.y(windowStart:sampleIndex, index);
      trailZ = path.z(windowStart:sampleIndex, index);
      set(controls.trails(index), 'XData', trailX, 'YData', trailY, ...
        'ZData', trailZ);
      set(controls.planets(index), 'XData', path.x(sampleIndex, index), ...
        'YData', path.y(sampleIndex, index), ...
        'ZData', path.z(sampleIndex, index));
    end
    title(controls.axes, sprintf('N-body orbits  (frame %d / %d)', ...
      sampleIndex, path.samples), 'Color', [0.92 0.92 1.00]);
    drawnow();
    if controls.framePause > 0
      pause(controls.framePause);
    end
  end
  if solarAnimationAlive(controls)
    title(controls.axes, 'Orbits complete - press Start to replay', ...
      'Color', [0.92 0.92 1.00]);
    drawnow();
  end
end
%=============================================================================
function solarClearTrails(controls)
  for index = 1:controls.count
    if isgraphics(controls.trails(index))
      set(controls.trails(index), 'XData', NaN, 'YData', NaN, 'ZData', NaN);
    end
    if isgraphics(controls.planets(index))
      set(controls.planets(index), 'XData', NaN, 'YData', NaN, 'ZData', NaN);
    end
  end
end
%=============================================================================
function alive = solarAnimationAlive(controls)
  alive = isgraphics(controls.figure) && isgraphics(controls.axes);
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
