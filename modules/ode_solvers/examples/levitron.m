%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Levitating spinning top: integrate the heavy symmetric-top (gyroscope)
% equations, then animate the hovering, precessing and nutating top in 3D
% from an interactive control panel (sliders + buttons).
%=============================================================================
if ~exist('levitronFigureVisible', 'var')
  levitronFigureVisible = 'on';
end
% Batch mode: one deterministic run with the default settings, a limited
% number of frames, then return (used for non-interactive testing / capture).
if ~exist('levitronBatch', 'var')
  levitronBatch = false;
end
if ~exist('levitronMaxFrames', 'var')
  levitronMaxFrames = 600;
end
if ~exist('levitronFramePause', 'var')
  levitronFramePause = 0.012;
end
if ~exist('levitronOutputImage', 'var')
  levitronOutputImage = '';
end

levitronParameters = struct();
levitronParameters.transverseInertia = 0.089;
levitronParameters.polarInertia = 0.139;
levitronParameters.gravityTorque = 1.0 * 9.81 * 0.30;
levitronParameters.duration = 30;

% --- Body geometry (a surface of revolution: cone + rim + stem) -------------
levitronBody = levitronBuildBody();

% --- Figure -----------------------------------------------------------------
levitronFigure = figure('Name', 'Levitron', 'NumberTitle', 'off', ...
  'Color', [0.06 0.07 0.10], 'Visible', levitronFigureVisible, ...
  'Position', [80 80 1000 680]);
levitronAxes = axes('Parent', levitronFigure, ...
  'Position', [0.05 0.24 0.92 0.72], 'Color', [0.06 0.07 0.10]);
hold(levitronAxes, 'on');

% Static base magnet (the launch platform) and reference floor.
levitronBaseAngle = linspace(0, 2 * pi, 60);
levitronBaseRadius = linspace(0, 0.55, 12);
[levitronBaseA, levitronBaseR] = meshgrid(levitronBaseAngle, levitronBaseRadius);
surf(levitronAxes, levitronBaseR .* cos(levitronBaseA), ...
  levitronBaseR .* sin(levitronBaseA), zeros(size(levitronBaseA)), ...
  'EdgeColor', 'none', 'FaceColor', [0.20 0.24 0.34], 'FaceAlpha', 0.9);

% The spinning top itself (updated every frame).
levitronTopSurface = surf(levitronAxes, levitronBody.x, levitronBody.y, ...
  levitronBody.z, 'EdgeColor', [0.25 0.18 0.05], 'LineWidth', 0.4, ...
  'FaceColor', [0.86 0.70 0.28], 'FaceLighting', 'gouraud');
levitronAxisLine = plot3(levitronAxes, NaN, NaN, NaN, ...
  'Color', [0.95 0.95 1.00], 'LineWidth', 1.6);
levitronSpinMarker = plot3(levitronAxes, NaN, NaN, NaN, 'o', ...
  'MarkerFaceColor', [0.90 0.20 0.20], 'MarkerEdgeColor', [0.30 0.05 0.05], ...
  'MarkerSize', 8, 'LineStyle', 'none');
levitronTipTrail = plot3(levitronAxes, NaN, NaN, NaN, ...
  'Color', [0.40 0.75 1.00], 'LineWidth', 1.0);
hold(levitronAxes, 'off');

axis(levitronAxes, [-0.9 0.9 -0.9 0.9 0 1.7]);
set(levitronAxes, 'XColor', [0.7 0.7 0.8], 'YColor', [0.7 0.7 0.8], ...
  'ZColor', [0.7 0.7 0.8]);
xlabel(levitronAxes, 'x');
ylabel(levitronAxes, 'y');
zlabel(levitronAxes, 'height');
view(levitronAxes, [-40 22]);
title(levitronAxes, 'Set spin, tilt and height, then press Start', ...
  'Color', [0.92 0.92 1.00]);

% --- Control panel ----------------------------------------------------------
levitronSpinSlider = uicontrol('Parent', levitronFigure, 'Style', 'slider', ...
  'Min', 10, 'Max', 40, 'Value', 26, 'Position', [60 70 170 20]);
levitronTiltSlider = uicontrol('Parent', levitronFigure, 'Style', 'slider', ...
  'Min', 8, 'Max', 55, 'Value', 24, 'Position', [260 70 170 20]);
levitronHeightSlider = uicontrol('Parent', levitronFigure, ...
  'Style', 'slider', 'Min', 0.7, 'Max', 1.4, 'Value', 1.0, ...
  'Position', [460 70 170 20]);
uicontrol('Parent', levitronFigure, 'Style', 'text', ...
  'Position', [60 92 170 18], 'String', 'Spin rate');
uicontrol('Parent', levitronFigure, 'Style', 'text', ...
  'Position', [260 92 170 18], 'String', 'Initial tilt (deg)');
uicontrol('Parent', levitronFigure, 'Style', 'text', ...
  'Position', [460 92 170 18], 'String', 'Hover height');
levitronReadout = uicontrol('Parent', levitronFigure, 'Style', 'text', ...
  'Position', [60 44 570 18], 'String', '');
levitronStartButton = uicontrol('Parent', levitronFigure, ...
  'Style', 'pushbutton', 'String', 'Start', 'Position', [680 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
levitronResetButton = uicontrol('Parent', levitronFigure, ...
  'Style', 'pushbutton', 'String', 'Reset', 'Position', [780 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
levitronStopButton = uicontrol('Parent', levitronFigure, ...
  'Style', 'pushbutton', 'String', 'Stop', 'Position', [880 66 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));

levitronControls = struct();
levitronControls.figure = levitronFigure;
levitronControls.axes = levitronAxes;
levitronControls.topSurface = levitronTopSurface;
levitronControls.axisLine = levitronAxisLine;
levitronControls.spinMarker = levitronSpinMarker;
levitronControls.tipTrail = levitronTipTrail;
levitronControls.readout = levitronReadout;
levitronControls.startButton = levitronStartButton;
levitronControls.resetButton = levitronResetButton;
levitronControls.stopButton = levitronStopButton;
levitronControls.body = levitronBody;
levitronControls.parameters = levitronParameters;
levitronControls.maxFrames = levitronMaxFrames;
levitronControls.framePause = levitronFramePause;

if levitronBatch
  levitronMotion = levitronSimulate(26, 24, 1.0, levitronParameters);
  levitronRunAnimation(levitronControls, levitronMotion);
  if ~isempty(levitronOutputImage) && isgraphics(levitronFigure)
    drawnow();
    saveas(levitronFigure, levitronOutputImage);
  end
else
  while isgraphics(levitronFigure)
    if levitronStopButton.UserData
      % Stop interrupts the current run but keeps the panel live so Start /
      % Reset stay usable; the window close button ends the demo.
      set(levitronStopButton, 'UserData', false);
    end
    if levitronResetButton.UserData
      set(levitronResetButton, 'UserData', false);
      set(levitronTipTrail, 'XData', NaN, 'YData', NaN, 'ZData', NaN);
      title(levitronAxes, 'Set spin, tilt and height, then press Start', ...
        'Color', [0.92 0.92 1.00]);
    end
    if levitronStartButton.UserData
      set(levitronStartButton, 'UserData', false);
      levitronMotion = levitronSimulate(levitronSpinSlider.Value, ...
        levitronTiltSlider.Value, levitronHeightSlider.Value, ...
        levitronParameters);
      levitronRunAnimation(levitronControls, levitronMotion);
    end
    drawnow();
    pause(0.03);
  end
end
%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================
function body = levitronBuildBody()
  % Revolve a silhouette (radius, height) into the top's surface mesh.
  profileRadius = [0.00 0.26 0.30 0.30 0.05 0.03 0.03];
  profileHeight = [-0.34 -0.06 0.00 0.06 0.10 0.12 0.34];
  revolveAngle = linspace(0, 2 * pi, 48);
  [heightGrid, angleGrid] = meshgrid(profileHeight, revolveAngle);
  radiusGrid = meshgrid(profileRadius, revolveAngle);
  bodyX = (radiusGrid .* cos(angleGrid)).';
  bodyY = (radiusGrid .* sin(angleGrid)).';
  bodyZ = heightGrid.';
  body = struct('x', bodyX, 'y', bodyY, 'z', bodyZ, ...
    'rows', size(bodyX, 1), 'cols', size(bodyX, 2), ...
    'markerRadius', 0.30, 'markerHeight', 0.03, ...
    'axisBottom', [0; 0; -0.34], 'axisTop', [0; 0; 0.34]);
end
%=============================================================================
function motion = levitronSimulate(spinRate, tiltDeg, hoverHeight, parameters)
  % Integrate the heavy symmetric top and store the hover trajectory.
  transverseInertia = parameters.transverseInertia;
  polarInertia = parameters.polarInertia;
  gravityTorque = parameters.gravityTorque;
  tilt0 = tiltDeg * pi / 180;
  spinMomentum = polarInertia * spinRate; % p_psi (constant)
  precessionMomentum = spinMomentum * cos(tilt0); % p_phi (phiDot0 = 0)
  initialState = [tilt0; 0; 0; 0]; % [theta; thetaDot; phi; psi]
  timeSpan = 0:0.01:parameters.duration;
  solverOptions = odeset('RelTol', 1e-7, 'AbsTol', 1e-9, 'MaxStep', 0.02);
  [timeValues, stateValues] = ode45(@(t, state) levitronTopRhs(t, state, ...
    transverseInertia, spinMomentum, spinRate, precessionMomentum, ...
    gravityTorque), timeSpan, initialState, solverOptions);
  hoverZ = hoverHeight + 0.06 * sin(2.0 * timeValues);
  hoverX = 0.10 * cos(0.7 * timeValues);
  hoverY = 0.10 * sin(0.7 * timeValues);
  motion = struct('time', timeValues, 'theta', stateValues(:, 1), ...
    'phi', stateValues(:, 3), 'psi', stateValues(:, 4), ...
    'hoverX', hoverX, 'hoverY', hoverY, 'hoverZ', hoverZ);
end
%=============================================================================
function derivative = levitronTopRhs(~, state, transverseInertia, ...
  spinMomentum, axialSpin, precessionMomentum, gravityTorque)
  % Lagrange equations of the heavy symmetric top in Euler angles.
  %   state = [theta (nutation); thetaDot; phi (precession); psi (spin)]
  %   spinMomentum   = p_psi = I3 * omega3   (constant)
  %   axialSpin      = omega3 = psiDot + phiDot cos(theta)   (constant)
  %   precessionMomentum = p_phi   (constant)
  tilt = state(1);
  tiltRate = state(2);
  cosTilt = cos(tilt);
  sinTilt = sin(tilt);
  sinTilt = sign(sinTilt + (sinTilt == 0)) * max(abs(sinTilt), 1e-4);
  precessionRate = (precessionMomentum - spinMomentum * cosTilt) / ...
    (transverseInertia * sinTilt ^ 2);
  tiltAcceleration = (transverseInertia * precessionRate ^ 2 * sinTilt * ...
    cosTilt - spinMomentum * precessionRate * sinTilt + ...
    gravityTorque * sinTilt) / transverseInertia;
  spinRate = axialSpin - precessionRate * cosTilt;
  derivative = [tiltRate; tiltAcceleration; precessionRate; spinRate];
end
%=============================================================================
function rotation = levitronRotationZXZ(precession, tilt, spin)
  cp = cos(precession); sp = sin(precession);
  ct = cos(tilt); st = sin(tilt);
  cs = cos(spin); ss = sin(spin);
  rotationZphi = [cp -sp 0; sp cp 0; 0 0 1];
  rotationXtheta = [1 0 0; 0 ct -st; 0 st ct];
  rotationZpsi = [cs -ss 0; ss cs 0; 0 0 1];
  rotation = rotationZphi * rotationXtheta * rotationZpsi;
end
%=============================================================================
function levitronRunAnimation(controls, motion)
  sampleCount = numel(motion.time);
  if sampleCount < 2
    return
  end
  body = controls.body;
  rowCount = body.rows;
  columnCount = body.cols;
  bodyPoints = [body.x(:).'; body.y(:).'; body.z(:).'];
  tipHistoryX = [];
  tipHistoryY = [];
  tipHistoryZ = [];
  frameStride = max(1, ceil(sampleCount / controls.maxFrames));
  for sampleIndex = 1:frameStride:sampleCount
    if ~levitronAnimationAlive(controls)
      return
    end
    rotation = levitronRotationZXZ(motion.phi(sampleIndex), ...
      motion.theta(sampleIndex), motion.psi(sampleIndex));
    center = [motion.hoverX(sampleIndex); motion.hoverY(sampleIndex); ...
      motion.hoverZ(sampleIndex)];
    worldPoints = rotation * bodyPoints + center;
    set(controls.topSurface, ...
      'XData', reshape(worldPoints(1, :), rowCount, columnCount), ...
      'YData', reshape(worldPoints(2, :), rowCount, columnCount), ...
      'ZData', reshape(worldPoints(3, :), rowCount, columnCount));
    axisBottom = rotation * body.axisBottom + center;
    axisTop = rotation * body.axisTop + center;
    set(controls.axisLine, 'XData', [axisBottom(1) axisTop(1)], ...
      'YData', [axisBottom(2) axisTop(2)], 'ZData', [axisBottom(3) axisTop(3)]);
    markerLocal = [body.markerRadius; 0; body.markerHeight];
    markerWorld = rotation * markerLocal + center;
    set(controls.spinMarker, 'XData', markerWorld(1), ...
      'YData', markerWorld(2), 'ZData', markerWorld(3));
    tipHistoryX = [tipHistoryX axisTop(1)];
    tipHistoryY = [tipHistoryY axisTop(2)];
    tipHistoryZ = [tipHistoryZ axisTop(3)];
    trailLimit = 160;
    if numel(tipHistoryX) > trailLimit
      tipHistoryX = tipHistoryX(end - trailLimit + 1:end);
      tipHistoryY = tipHistoryY(end - trailLimit + 1:end);
      tipHistoryZ = tipHistoryZ(end - trailLimit + 1:end);
    end
    set(controls.tipTrail, 'XData', tipHistoryX, 'YData', tipHistoryY, ...
      'ZData', tipHistoryZ);
    set(controls.readout, 'String', sprintf(...
      'tilt = %.1f deg    precession = %.1f deg    height = %.2f', ...
      motion.theta(sampleIndex) * 180 / pi, ...
      mod(motion.phi(sampleIndex) * 180 / pi, 360), ...
      motion.hoverZ(sampleIndex)));
    title(controls.axes, sprintf('Levitating top  (t = %.1f s)', ...
      motion.time(sampleIndex)), 'Color', [0.92 0.92 1.00]);
    drawnow();
    if controls.framePause > 0
      pause(controls.framePause);
    end
  end
end
%=============================================================================
function alive = levitronAnimationAlive(controls)
  alive = isgraphics(controls.figure) && isgraphics(controls.topSurface) && ...
    isgraphics(controls.axisLine);
  if ~alive
    return
  end
  if isgraphics(controls.stopButton) && controls.stopButton.UserData
    alive = false;
  end
  if isgraphics(controls.resetButton) && controls.resetButton.UserData
    alive = false;
  end
end
%=============================================================================
