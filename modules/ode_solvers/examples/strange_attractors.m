%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Gallery of famous 3D strange attractors (Aizawa, Rossler, Thomas,
% Halvorsen, Lorenz): pick one from the dropdown, integrate it with ode45
% and watch a growing, slowly rotating comet trace out the chaotic orbit.
%=============================================================================
if ~exist('attractorsFigureVisible', 'var')
  attractorsFigureVisible = 'on';
end
% Batch mode: run one deterministic pass with the default attractor, animate a
% limited number of frames, then return (used for non-interactive testing).
if ~exist('attractorsBatch', 'var')
  attractorsBatch = false;
end
if ~exist('attractorsMaxFrames', 'var')
  attractorsMaxFrames = 600;
end
if ~exist('attractorsFramePause', 'var')
  attractorsFramePause = 0;
end
if ~exist('attractorsTrailLength', 'var')
  attractorsTrailLength = 900;
end
if ~exist('attractorsOutputImage', 'var')
  attractorsOutputImage = '';
end

attractorsNames = {'Aizawa', 'Rossler', 'Thomas', 'Halvorsen', 'Lorenz'};

% --- Figure -----------------------------------------------------------------
attractorsFigure = figure('Name', 'Strange attractors', ...
  'NumberTitle', 'off', 'Color', [0.04 0.04 0.08], ...
  'Visible', attractorsFigureVisible, 'Position', [80 80 1000 700]);
attractorsAxes = axes('Parent', attractorsFigure, ...
  'Position', [0.05 0.20 0.92 0.75], 'Color', [0.04 0.04 0.08]);
hold(attractorsAxes, 'on');
attractorsTrail = plot3(attractorsAxes, NaN, NaN, NaN, ...
  'Color', [0.30 0.85 1.00], 'LineWidth', 1.2);
attractorsHead = plot3(attractorsAxes, NaN, NaN, NaN, 'o', ...
  'MarkerFaceColor', [1.00 0.95 0.55], 'MarkerEdgeColor', [1.00 0.70 0.20], ...
  'MarkerSize', 9, 'LineStyle', 'none');
hold(attractorsAxes, 'off');
grid(attractorsAxes, 'on');
set(attractorsAxes, 'XColor', [0.7 0.7 0.8], 'YColor', [0.7 0.7 0.8], ...
  'ZColor', [0.7 0.7 0.8]);
xlabel(attractorsAxes, 'x');
ylabel(attractorsAxes, 'y');
zlabel(attractorsAxes, 'z');
view(attractorsAxes, [-37.5 30]);
title(attractorsAxes, 'Pick an attractor, then press Start', ...
  'Color', [0.92 0.92 1.00]);

% --- Control panel ----------------------------------------------------------
uicontrol('Parent', attractorsFigure, 'Style', 'text', ...
  'Position', [60 96 170 18], 'String', 'Attractor');
attractorsPopup = uicontrol('Parent', attractorsFigure, ...
  'Style', 'popupmenu', 'String', attractorsNames, 'Value', 1, ...
  'Position', [60 70 170 22]);
attractorsReadout = uicontrol('Parent', attractorsFigure, 'Style', 'text', ...
  'Position', [260 66 400 30], 'String', '');
attractorsStartButton = uicontrol('Parent', attractorsFigure, ...
  'Style', 'pushbutton', 'String', 'Start', 'Position', [680 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
attractorsResetButton = uicontrol('Parent', attractorsFigure, ...
  'Style', 'pushbutton', 'String', 'Reset', 'Position', [780 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
attractorsStopButton = uicontrol('Parent', attractorsFigure, ...
  'Style', 'pushbutton', 'String', 'Stop', 'Position', [880 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));

attractorsControls = struct();
attractorsControls.figure = attractorsFigure;
attractorsControls.axes = attractorsAxes;
attractorsControls.trail = attractorsTrail;
attractorsControls.head = attractorsHead;
attractorsControls.readout = attractorsReadout;
attractorsControls.startButton = attractorsStartButton;
attractorsControls.resetButton = attractorsResetButton;
attractorsControls.stopButton = attractorsStopButton;
attractorsControls.maxFrames = attractorsMaxFrames;
attractorsControls.framePause = attractorsFramePause;
attractorsControls.trailLength = attractorsTrailLength;

if attractorsBatch
  % One deterministic run for testing / off-screen capture (Aizawa).
  attractorsPath = attractorsCompute('Aizawa');
  attractorsRunAnimation(attractorsControls, attractorsPath, 'Aizawa');
  if ~isempty(attractorsOutputImage) && isgraphics(attractorsFigure)
    drawnow();
    saveas(attractorsFigure, attractorsOutputImage);
  end
else
  % Interactive loop: wait for Start, integrate the selected attractor, animate.
  while isgraphics(attractorsFigure)
    if attractorsStopButton.UserData
      % Stop interrupts the current run but keeps the panel live so Start /
      % Reset stay usable; the window close button ends the demo.
      set(attractorsStopButton, 'UserData', false);
    end
    if attractorsResetButton.UserData
      set(attractorsResetButton, 'UserData', false);
      set(attractorsTrail, 'XData', NaN, 'YData', NaN, 'ZData', NaN);
      set(attractorsHead, 'XData', NaN, 'YData', NaN, 'ZData', NaN);
      title(attractorsAxes, 'Pick an attractor, then press Start', ...
        'Color', [0.92 0.92 1.00]);
    end
    if attractorsStartButton.UserData
      set(attractorsStartButton, 'UserData', false);
      attractorsChoice = attractorsNames{attractorsPopup.Value};
      attractorsPath = attractorsCompute(attractorsChoice);
      attractorsRunAnimation(attractorsControls, attractorsPath, ...
        attractorsChoice);
    end
    drawnow();
    pause(0.03);
  end
end
%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================
function path = attractorsCompute(name)
  % Integrate the requested attractor and return the sampled orbit.
  [rhs, y0, tspan, maxStep] = attractorsDefinition(name);
  solverOptions = odeset('RelTol', 1e-8, 'AbsTol', 1e-9, 'MaxStep', maxStep);
  [~, stateHistory] = ode45(rhs, tspan, y0, solverOptions);
  path = struct('x', stateHistory(:, 1), 'y', stateHistory(:, 2), ...
    'z', stateHistory(:, 3));
end
%=============================================================================
function [rhs, y0, tspan, maxStep] = attractorsDefinition(name)
  % Return right-hand side, initial state, time span and MaxStep per attractor.
  switch name
    case 'Lorenz'
      rhs = @(t, s)[10 * (s(2) - s(1)); ...
        s(1) * (28 - s(3)) - s(2); ...
        s(1) * s(2) - (8 / 3) * s(3)];
      y0 = [-3; -6; 12];
      tspan = [0 45];
      maxStep = 0.01;
    case 'Rossler'
      % Note: write -y - z explicitly (not -(y + z)); a unary minus applied
      % to a parenthesised sum inside an anonymous function body is not
      % negated correctly, which would send this orbit to infinity.
      rhs = @(t, s)[-s(2) - s(3); ...
        s(1) + 0.2 * s(2); ...
        0.2 + s(3) * (s(1) - 5.7)];
      y0 = [0.1; 0; 0];
      tspan = [0 220];
      maxStep = 0.05;
    case 'Aizawa'
      a = 0.95; b = 0.7; c = 0.6; d = 3.5; e = 0.25; f = 0.1;
      rhs = @(t, s)[(s(3) - b) * s(1) - d * s(2); ...
        d * s(1) + (s(3) - b) * s(2); ...
        c + a * s(3) - (s(3) ^ 3) / 3 ...
        - (s(1) ^ 2 + s(2) ^ 2) * (1 + e * s(3)) + f * s(3) * s(1) ^ 3];
      y0 = [0.1; 0; 0];
      tspan = [0 100];
      maxStep = 0.02;
    case 'Thomas'
      b = 0.208;
      rhs = @(t, s)[sin(s(2)) - b * s(1); ...
        sin(s(3)) - b * s(2); ...
        sin(s(1)) - b * s(3)];
      y0 = [1.1; 1.1; -0.01];
      tspan = [0 320];
      maxStep = 0.05;
    case 'Halvorsen'
      a = 1.89;
      rhs = @(t, s)[-a * s(1) - 4 * s(2) - 4 * s(3) - s(2) ^ 2; ...
        -a * s(2) - 4 * s(3) - 4 * s(1) - s(3) ^ 2; ...
        -a * s(3) - 4 * s(1) - 4 * s(2) - s(1) ^ 2];
      y0 = [-5; 0; 0];
      tspan = [0 100];
      maxStep = 0.02;
    otherwise
      error('Nelson:strangeAttractors:unknownName', ...
        'Unknown attractor name.');
  end
end
%=============================================================================
function attractorsRunAnimation(controls, path, name)
  % Grow a comet along the orbit while slowly rotating the view for depth.
  sampleCount = numel(path.x);
  if sampleCount < 2
    return
  end
  % Fit the axes to the whole orbit so limits stay steady while it grows.
  attractorsFitAxes(controls.axes, path);
  azimuth = -37.5;
  elevation = 30;
  frameStride = max(1, ceil(sampleCount / controls.maxFrames));
  % Redraw only a trailing window of the comet each frame so the per-frame
  % cost stays constant (redrawing the full growing polyline is the dominant
  % on-screen cost); the complete orbit is drawn once at the end.
  for sampleIndex = 1:frameStride:sampleCount
    if ~attractorsAnimationAlive(controls)
      return
    end
    trailStart = max(1, sampleIndex - controls.trailLength + 1);
    set(controls.trail, 'XData', path.x(trailStart:sampleIndex), ...
      'YData', path.y(trailStart:sampleIndex), 'ZData', path.z(trailStart:sampleIndex));
    set(controls.head, 'XData', path.x(sampleIndex), ...
      'YData', path.y(sampleIndex), 'ZData', path.z(sampleIndex));
    azimuth = azimuth + 0.35;
    view(controls.axes, [azimuth elevation]);
    set(controls.readout, 'String', sprintf(...
      '%s:   x = %+.2f   y = %+.2f   z = %+.2f', name, ...
      path.x(sampleIndex), path.y(sampleIndex), path.z(sampleIndex)));
    title(controls.axes, sprintf('%s attractor', name), ...
      'Color', [0.92 0.92 1.00]);
    drawnow();
    if controls.framePause > 0
      pause(controls.framePause);
    end
  end
  if attractorsAnimationAlive(controls)
    set(controls.trail, 'XData', path.x, 'YData', path.y, 'ZData', path.z);
    set(controls.head, 'XData', path.x(end), 'YData', path.y(end), ...
      'ZData', path.z(end));
    title(controls.axes, [name, ' attractor - press Start to run again'], ...
      'Color', [0.92 0.92 1.00]);
    drawnow();
  end
end
%=============================================================================
function attractorsFitAxes(ax, path)
  % Pad the data range a little so the comet never touches the box edges.
  xLo = min(path.x); xHi = max(path.x);
  yLo = min(path.y); yHi = max(path.y);
  zLo = min(path.z); zHi = max(path.z);
  padX = 0.06 * max(xHi - xLo, 1e-3);
  padY = 0.06 * max(yHi - yLo, 1e-3);
  padZ = 0.06 * max(zHi - zLo, 1e-3);
  axis(ax, [xLo - padX xHi + padX yLo - padY yHi + padY zLo - padZ zHi + padZ]);
end
%=============================================================================
function alive = attractorsAnimationAlive(controls)
  alive = isgraphics(controls.figure) && isgraphics(controls.trail) && ...
    isgraphics(controls.head);
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
