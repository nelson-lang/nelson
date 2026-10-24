%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Ball-on-plate demo: design a state-feedback controller, integrate the
% closed-loop rolling-ball model, then animate the plate, ball, linkages and
% tracking traces.
%=============================================================================
if ~exist('ballOnPlateFigureVisible', 'var')
  ballOnPlateFigureVisible = 'on';
end
if ~exist('ballOnPlateBatch', 'var')
  ballOnPlateBatch = false;
end
if ~exist('ballOnPlateAutoStart', 'var')
  ballOnPlateAutoStart = true;
end
if ~exist('ballOnPlateLoop', 'var')
  ballOnPlateLoop = true;
end
if ~exist('ballOnPlateMaxFrames', 'var')
  ballOnPlateMaxFrames = 560;
end
if ~exist('ballOnPlateFramePause', 'var')
  ballOnPlateFramePause = 0.012;
end
if ~exist('ballOnPlateOutputImage', 'var')
  ballOnPlateOutputImage = '';
end

ballOnPlateParameters = ballOnPlateDefaultParameters();
[ballOnPlateA, ballOnPlateB] = ballOnPlateStateSpace(ballOnPlateParameters);
[ballOnPlateK, ballOnPlateS, ballOnPlateClosedLoopPoles, ...
  ballOnPlateControllerSource] = ballOnPlateDesignController(ballOnPlateA, ...
  ballOnPlateB);
ballOnPlatePath = ballOnPlateSimulate(ballOnPlateK, ballOnPlateParameters);

[ballOnPlateFigure, ballOnPlateControls] = ...
  ballOnPlateCreateScene(ballOnPlatePath, ballOnPlateParameters, ...
  ballOnPlateFigureVisible);
ballOnPlateControls.K = ballOnPlateK;
ballOnPlateControls.maxFrames = ballOnPlateMaxFrames;
ballOnPlateControls.framePause = ballOnPlateFramePause;
ballOnPlateControls.loop = ballOnPlateLoop;

if ballOnPlateBatch
  ballOnPlateRunAnimation(ballOnPlateControls, ballOnPlatePath);
  if ~isempty(ballOnPlateOutputImage) && isgraphics(ballOnPlateFigure)
    drawnow();
    saveas(ballOnPlateFigure, ballOnPlateOutputImage);
  end
else
  if ballOnPlateAutoStart
    set(ballOnPlateControls.startButton, 'UserData', true);
  end
  while isgraphics(ballOnPlateFigure)
    if ballOnPlateControls.resetButton.UserData
      set(ballOnPlateControls.resetButton, 'UserData', false);
      ballOnPlateClearAnimation(ballOnPlateControls);
    end
    if ballOnPlateControls.startButton.UserData
      set(ballOnPlateControls.startButton, 'UserData', false);
      ballOnPlateClearAnimation(ballOnPlateControls);
      completed = ballOnPlateRunAnimation(ballOnPlateControls, ...
        ballOnPlatePath);
      if completed && ballOnPlateLoop && isgraphics(ballOnPlateFigure)
        pause(0.35);
        ballOnPlateClearAnimation(ballOnPlateControls);
        set(ballOnPlateControls.startButton, 'UserData', true);
      end
    end
    if ballOnPlateControls.stopButton.UserData
      set(ballOnPlateControls.stopButton, 'UserData', false);
    end
    drawnow();
    pause(0.03);
  end
end
%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================
function parameters = ballOnPlateDefaultParameters()
  parameters = struct();
  parameters.gravity = 9.81;
  parameters.ballAccelerationGain = 5 * parameters.gravity / 7;
  parameters.viscousDamping = 0.22;
  parameters.actuatorTimeConstant = 0.12;
  parameters.maxTilt = 10 * pi / 180;
  parameters.feedForwardLimit = 7 * pi / 180;
  parameters.stepAngle = 0.06 * pi / 180;
  parameters.duration = 20;
  parameters.sampleStep = 0.02;
  parameters.referenceAmplitude = 0.18;
  parameters.referencePeriod = 12;
  parameters.plateHalfSize = 0.43;
  parameters.plateHeight = 0.22;
  parameters.ballRadius = 0.035;
  parameters.supportHeight = 0.02;
end
%=============================================================================
function [A, B] = ballOnPlateStateSpace(parameters)
  c = parameters.ballAccelerationGain;
  d = parameters.viscousDamping;
  tau = parameters.actuatorTimeConstant;
  A = [0 1 0 0 0 0; ...
    0 -d 0 0 c 0; ...
    0 0 0 1 0 0; ...
    0 0 0 -d 0 c; ...
    0 0 0 0 -1 / tau 0; ...
    0 0 0 0 0 -1 / tau];
  B = [0 0; 0 0; 0 0; 0 0; 1 / tau 0; 0 1 / tau];
end
%=============================================================================
function [K, S, poles, source] = ballOnPlateDesignController(A, B)
  Q = diag([70 7 70 7 1 1]);
  R = diag([0.35 0.35]);
  K = [14.14213562 5.80047367 0 0 2.68942845 0; ...
    0 0 14.14213562 5.80047367 0 2.68942845];
  S = [];
  source = 'preset';
  if ismodule('slicot')
    try
      [K, S, poles] = lqr(A, B, Q, R);
      source = 'lqr';
      return
    catch
      source = 'preset';
    end
  end
  poles = eig(A - B * K);
end
%=============================================================================
function path = ballOnPlateSimulate(K, parameters)
  [initialReference, ~] = ballOnPlateReference(0, parameters);
  initialState = [initialReference(1) - 0.40; ...
    0; ...
    initialReference(3) + 0.14; ...
    0; ...
    0; ...
    0];
  timeSpan = 0:parameters.sampleStep:parameters.duration;
  solverOptions = odeset('RelTol', 1e-6, 'AbsTol', 1e-8, ...
    'MaxStep', 0.04);
  [timeValues, stateValues] = ode45(@(t, state) ...
    ballOnPlateRhs(t, state, K, parameters), timeSpan, initialState, ...
    solverOptions);
  path = ballOnPlateBuildPath(timeValues, stateValues, K, parameters);
end
%=============================================================================
function derivative = ballOnPlateRhs(t, state, K, parameters)
  command = ballOnPlateControlCommand(t, state, K, parameters);
  alpha = state(5);
  beta = state(6);
  c = parameters.ballAccelerationGain;
  d = parameters.viscousDamping;
  tau = parameters.actuatorTimeConstant;
  derivative = [state(2); ...
    c * sin(alpha) - d * state(2); ...
    state(4); ...
    c * sin(beta) - d * state(4); ...
    (command(1) - alpha) / tau; ...
    (command(2) - beta) / tau];
end
%=============================================================================
function command = ballOnPlateControlCommand(t, state, K, parameters)
  [referenceState, referenceAcceleration] = ballOnPlateReference(t, parameters);
  alphaFeedForward = ballOnPlateClamp(referenceAcceleration(1) / ...
    parameters.ballAccelerationGain, -parameters.feedForwardLimit, ...
    parameters.feedForwardLimit);
  betaFeedForward = ballOnPlateClamp(referenceAcceleration(2) / ...
    parameters.ballAccelerationGain, -parameters.feedForwardLimit, ...
    parameters.feedForwardLimit);
  targetState = [referenceState(1); referenceState(2); ...
    referenceState(3); referenceState(4); alphaFeedForward; betaFeedForward];
  command = [alphaFeedForward; betaFeedForward] - K * (state(:) - targetState);
  command = ballOnPlateClamp(command, -parameters.maxTilt, ...
    parameters.maxTilt);
end
%=============================================================================
function [referenceState, referenceAcceleration] = ...
  ballOnPlateReference(t, parameters)
  amplitude = parameters.referenceAmplitude;
  omega = 2 * pi / parameters.referencePeriod;
  phase = omega * t;
  x = amplitude * cos(phase);
  vx = -amplitude * omega * sin(phase);
  ax = -amplitude * omega ^ 2 * cos(phase);
  y = 0.65 * amplitude * sin(2 * phase);
  vy = 1.30 * amplitude * omega * cos(2 * phase);
  ay = -2.60 * amplitude * omega ^ 2 * sin(2 * phase);
  referenceState = [x; vx; y; vy];
  referenceAcceleration = [ax; ay];
end
%=============================================================================
function value = ballOnPlateClamp(value, lowerBound, upperBound)
  value = min(upperBound, max(lowerBound, value));
end
%=============================================================================
function path = ballOnPlateBuildPath(timeValues, stateValues, K, parameters)
  sampleCount = numel(timeValues);
  referenceX = zeros(sampleCount, 1);
  referenceY = zeros(sampleCount, 1);
  commandAlpha = zeros(sampleCount, 1);
  commandBeta = zeros(sampleCount, 1);
  motorSteps = zeros(sampleCount, 2);
  trackingError = zeros(sampleCount, 1);
  for index = 1:sampleCount
    state = stateValues(index, :).';
    [referenceState, ~] = ballOnPlateReference(timeValues(index), parameters);
    command = ballOnPlateControlCommand(timeValues(index), state, K, ...
      parameters);
    referenceX(index) = referenceState(1);
    referenceY(index) = referenceState(3);
    commandAlpha(index) = command(1);
    commandBeta(index) = command(2);
    motorSteps(index, :) = round(command(:).' / parameters.stepAngle);
    trackingError(index) = hypot(state(1) - referenceState(1), ...
      state(3) - referenceState(3));
  end
  path = struct();
  path.time = timeValues;
  path.state = stateValues;
  path.x = stateValues(:, 1);
  path.y = stateValues(:, 3);
  path.alpha = stateValues(:, 5);
  path.beta = stateValues(:, 6);
  path.referenceX = referenceX;
  path.referenceY = referenceY;
  path.commandAlpha = commandAlpha;
  path.commandBeta = commandBeta;
  path.motorSteps = motorSteps;
  path.trackingError = trackingError;
  path.finalError = trackingError(end);
end
%=============================================================================
function [figureHandle, controls] = ballOnPlateCreateScene(path, parameters, ...
  figureVisible)
  figureHandle = figure('Name', 'Ball-on-plate LQR tracking', ...
    'NumberTitle', 'off', 'Color', [0.06 0.07 0.09], ...
    'Visible', figureVisible, 'Position', [70 70 1180 720]);
  sceneAxes = subplot(2, 2, [1 3], 'Parent', figureHandle);
  trajectoryAxes = subplot(2, 2, 2, 'Parent', figureHandle);
  angleAxes = subplot(2, 2, 4, 'Parent', figureHandle);

  scene = ballOnPlateCreateSceneObjects(sceneAxes, path, parameters);
  traces = ballOnPlateCreateTraceObjects(trajectoryAxes, angleAxes, path);
  buttons = ballOnPlateCreateButtons(figureHandle);

  controls = scene;
  controls.figure = figureHandle;
  controls.trajectoryAxes = trajectoryAxes;
  controls.angleAxes = angleAxes;
  controls.actualTrace = traces.actualTrace;
  controls.referenceTrace = traces.referenceTrace;
  controls.currentMarker = traces.currentMarker;
  controls.targetMarker = traces.targetMarker;
  controls.alphaTrace = traces.alphaTrace;
  controls.betaTrace = traces.betaTrace;
  controls.commandAlphaTrace = traces.commandAlphaTrace;
  controls.commandBetaTrace = traces.commandBetaTrace;
  controls.startButton = buttons.startButton;
  controls.resetButton = buttons.resetButton;
  controls.stopButton = buttons.stopButton;
  controls.parameters = parameters;
end
%=============================================================================
function scene = ballOnPlateCreateSceneObjects(sceneAxes, path, parameters)
  [plateX, plateY, plateZ] = ballOnPlatePlateSurface(0, 0, parameters);
  [ballX, ballY, ballZ] = ballOnPlateBallSurface(path.x(1), path.y(1), ...
    0, 0, parameters);
  hold(sceneAxes, 'on');
  plateSurface = surf(sceneAxes, plateX, plateY, plateZ, ...
    zeros(size(plateX)), 'EdgeColor', [0.16 0.18 0.22], ...
    'FaceColor', [0.30 0.52 0.78], 'FaceAlpha', 0.92);
  [supportLines, supportTop] = ballOnPlateCreateSupports(sceneAxes, ...
    plateX, plateY, plateZ, parameters);
  ballSurface = surf(sceneAxes, ballX, ballY, ballZ, ...
    'EdgeColor', 'none', 'FaceColor', [1.00 0.58 0.16], ...
    'FaceLighting', 'gouraud');
  ballTrail = animatedline(sceneAxes, 'Color', [1.00 0.72 0.20], ...
    'LineWidth', 1.8, 'MaximumNumPoints', 260);
  referenceTrail = animatedline(sceneAxes, 'Color', [0.80 0.90 1.00], ...
    'LineStyle', '--', 'LineWidth', 1.2, 'MaximumNumPoints', 260);
  plot3(sceneAxes, path.referenceX, path.referenceY, ...
    ballOnPlatePlaneHeight(path.referenceX, path.referenceY, 0, 0, ...
    parameters) + parameters.ballRadius, ':', ...
    'Color', [0.55 0.66 0.78], 'LineWidth', 1.0);
  hold(sceneAxes, 'off');
  axis(sceneAxes, [-0.55 0.55 -0.55 0.55 -0.02 0.40]);
  set(sceneAxes, 'DataAspectRatio', [1 1 1]);
  grid(sceneAxes, 'on');
  set(sceneAxes, 'Color', [0.06 0.07 0.09], 'XColor', [0.75 0.78 0.84], ...
    'YColor', [0.75 0.78 0.84], 'ZColor', [0.75 0.78 0.84]);
  xlabel(sceneAxes, 'x (m)');
  ylabel(sceneAxes, 'y (m)');
  zlabel(sceneAxes, 'height (m)');
  view(sceneAxes, [-42 26]);
  light(sceneAxes, 'Position', [1 -2 3]);
  titleText = title(sceneAxes, 'Press Start to track the reference path', ...
    'Color', [0.94 0.96 1.00]);

  scene = struct();
  scene.sceneAxes = sceneAxes;
  scene.plateSurface = plateSurface;
  scene.ballSurface = ballSurface;
  scene.supportLines = supportLines;
  scene.supportTop = supportTop;
  scene.ballTrail = ballTrail;
  scene.referenceTrail = referenceTrail;
  scene.titleText = titleText;
end
%=============================================================================
function [supportLines, supportTop] = ballOnPlateCreateSupports(sceneAxes, ...
  plateX, plateY, plateZ, parameters)
  top = [plateX(1, 1) plateY(1, 1) plateZ(1, 1); ...
    plateX(1, 2) plateY(1, 2) plateZ(1, 2); ...
    plateX(2, 2) plateY(2, 2) plateZ(2, 2); ...
    plateX(2, 1) plateY(2, 1) plateZ(2, 1)];
  supportTop = top;
  supportLines = gobjects(1, 4);
  for index = 1:4
    base = [top(index, 1) top(index, 2) parameters.supportHeight];
    supportLines(index) = plot3(sceneAxes, [base(1) top(index, 1)], ...
      [base(2) top(index, 2)], [base(3) top(index, 3)], ...
      'Color', [0.88 0.90 0.94], 'LineWidth', 2.0);
  end
end
%=============================================================================
function traces = ballOnPlateCreateTraceObjects(trajectoryAxes, angleAxes, path)
  hold(trajectoryAxes, 'on');
  plot(trajectoryAxes, path.referenceX, path.referenceY, ...
    'Color', [0.65 0.68 0.74], 'LineStyle', '--', 'LineWidth', 1.2);
  actualTrace = animatedline(trajectoryAxes, 'Color', [0.05 0.38 0.85], ...
    'LineWidth', 2.0);
  referenceTrace = animatedline(trajectoryAxes, 'Color', [0.95 0.45 0.12], ...
    'LineWidth', 1.4);
  currentMarker = plot(trajectoryAxes, NaN, NaN, 'o', ...
    'MarkerFaceColor', [0.05 0.38 0.85], 'MarkerEdgeColor', 'w', ...
    'MarkerSize', 7);
  targetMarker = plot(trajectoryAxes, NaN, NaN, 'o', ...
    'MarkerFaceColor', [0.95 0.45 0.12], 'MarkerEdgeColor', 'w', ...
    'MarkerSize', 6);
  hold(trajectoryAxes, 'off');
  axis(trajectoryAxes, [-0.30 0.30 -0.22 0.22]);
  axis(trajectoryAxes, 'equal');
  grid(trajectoryAxes, 'on');
  set(trajectoryAxes, 'XColor', [0.82 0.84 0.88], ...
    'YColor', [0.82 0.84 0.88]);
  trajectoryXLabel = xlabel(trajectoryAxes, 'x (m)');
  trajectoryYLabel = ylabel(trajectoryAxes, 'y (m)');
  trajectoryTitle = title(trajectoryAxes, 'Ball position vs reference');
  set(trajectoryXLabel, 'Color', [0.82 0.84 0.88]);
  set(trajectoryYLabel, 'Color', [0.82 0.84 0.88]);
  set(trajectoryTitle, 'Color', [0.92 0.94 0.98]);
  legend(trajectoryAxes, 'reference path', 'ball', 'reference', ...
    'Location', 'best');

  hold(angleAxes, 'on');
  alphaTrace = animatedline(angleAxes, 'Color', [0.05 0.38 0.85], ...
    'LineWidth', 1.5);
  betaTrace = animatedline(angleAxes, 'Color', [0.05 0.62 0.30], ...
    'LineWidth', 1.5);
  commandAlphaTrace = animatedline(angleAxes, 'Color', [0.88 0.34 0.14], ...
    'LineStyle', '--', 'LineWidth', 1.2);
  commandBetaTrace = animatedline(angleAxes, 'Color', [0.58 0.28 0.86], ...
    'LineStyle', '--', 'LineWidth', 1.2);
  hold(angleAxes, 'off');
  xlim(angleAxes, [path.time(1) path.time(end)]);
  ylim(angleAxes, [-12 12]);
  grid(angleAxes, 'on');
  set(angleAxes, 'XColor', [0.82 0.84 0.88], ...
    'YColor', [0.82 0.84 0.88]);
  angleXLabel = xlabel(angleAxes, 'Time (s)');
  angleYLabel = ylabel(angleAxes, 'Plate angle (deg)');
  angleTitle = title(angleAxes, 'Actuator angles and commands');
  set(angleXLabel, 'Color', [0.82 0.84 0.88]);
  set(angleYLabel, 'Color', [0.82 0.84 0.88]);
  set(angleTitle, 'Color', [0.92 0.94 0.98]);
  legend(angleAxes, 'alpha', 'beta', 'alpha cmd', 'beta cmd', ...
    'Location', 'best');

  traces = struct('actualTrace', actualTrace, ...
    'referenceTrace', referenceTrace, 'currentMarker', currentMarker, ...
    'targetMarker', targetMarker, 'alphaTrace', alphaTrace, ...
    'betaTrace', betaTrace, 'commandAlphaTrace', commandAlphaTrace, ...
    'commandBetaTrace', commandBetaTrace);
end
%=============================================================================
function buttons = ballOnPlateCreateButtons(figureHandle)
  startButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
    'String', 'Start', 'Position', [60 18 90 30], 'UserData', false, ...
    'Callback', @(source, event) set(source, 'UserData', true));
  resetButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
    'String', 'Reset', 'Position', [160 18 90 30], 'UserData', false, ...
    'Callback', @(source, event) set(source, 'UserData', true));
  stopButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
    'String', 'Stop', 'Position', [260 18 90 30], 'UserData', false, ...
    'Callback', @(source, event) set(source, 'UserData', true));
  buttons = struct('startButton', startButton, ...
    'resetButton', resetButton, 'stopButton', stopButton);
end
%=============================================================================
function completed = ballOnPlateRunAnimation(controls, path)
  completed = false;
  sampleCount = numel(path.time);
  frameCount = min(sampleCount, max(2, controls.maxFrames));
  for frameIndex = 1:frameCount
    if controls.stopButton.UserData || ~ballOnPlateAnimationAlive(controls)
      break
    end
    sampleIndex = round(1 + (sampleCount - 1) * (frameIndex - 1) / ...
      (frameCount - 1));
    ballOnPlateShowFrame(controls, path, sampleIndex);
    drawnow();
    if controls.framePause > 0
      pause(controls.framePause);
    end
  end
  if ballOnPlateAnimationAlive(controls)
    completed = ~controls.stopButton.UserData;
    set(controls.titleText, 'String', ...
      sprintf('Final tracking error: %.3f m', path.finalError));
  end
end
%=============================================================================
function ballOnPlateShowFrame(controls, path, sampleIndex)
  parameters = controls.parameters;
  alpha = path.alpha(sampleIndex);
  beta = path.beta(sampleIndex);
  [plateX, plateY, plateZ] = ballOnPlatePlateSurface(alpha, beta, parameters);
  [ballX, ballY, ballZ] = ballOnPlateBallSurface(path.x(sampleIndex), ...
    path.y(sampleIndex), alpha, beta, parameters);
  set(controls.plateSurface, 'XData', plateX, 'YData', plateY, ...
    'ZData', plateZ);
  set(controls.ballSurface, 'XData', ballX, 'YData', ballY, 'ZData', ballZ);
  ballZCenter = ballOnPlatePlaneHeight(path.x(sampleIndex), ...
    path.y(sampleIndex), alpha, beta, parameters) + parameters.ballRadius;
  targetZ = ballOnPlatePlaneHeight(path.referenceX(sampleIndex), ...
    path.referenceY(sampleIndex), alpha, beta, parameters) + ...
    parameters.ballRadius;
  addpoints(controls.ballTrail, path.x(sampleIndex), path.y(sampleIndex), ...
    ballZCenter);
  addpoints(controls.referenceTrail, path.referenceX(sampleIndex), ...
    path.referenceY(sampleIndex), targetZ);
  addpoints(controls.actualTrace, path.x(sampleIndex), path.y(sampleIndex));
  addpoints(controls.referenceTrace, path.referenceX(sampleIndex), ...
    path.referenceY(sampleIndex));
  set(controls.currentMarker, 'XData', path.x(sampleIndex), ...
    'YData', path.y(sampleIndex));
  set(controls.targetMarker, 'XData', path.referenceX(sampleIndex), ...
    'YData', path.referenceY(sampleIndex));
  addpoints(controls.alphaTrace, path.time(sampleIndex), alpha * 180 / pi);
  addpoints(controls.betaTrace, path.time(sampleIndex), beta * 180 / pi);
  addpoints(controls.commandAlphaTrace, path.time(sampleIndex), ...
    path.commandAlpha(sampleIndex) * 180 / pi);
  addpoints(controls.commandBetaTrace, path.time(sampleIndex), ...
    path.commandBeta(sampleIndex) * 180 / pi);
  ballOnPlateUpdateSupports(controls, plateX, plateY, plateZ);
  set(controls.titleText, 'String', sprintf(...
    't = %.2f s   error = %.3f m   steps = [%d, %d]', ...
    path.time(sampleIndex), path.trackingError(sampleIndex), ...
    path.motorSteps(sampleIndex, 1), path.motorSteps(sampleIndex, 2)));
end
%=============================================================================
function ballOnPlateUpdateSupports(controls, plateX, plateY, plateZ)
  parameters = controls.parameters;
  top = [plateX(1, 1) plateY(1, 1) plateZ(1, 1); ...
    plateX(1, 2) plateY(1, 2) plateZ(1, 2); ...
    plateX(2, 2) plateY(2, 2) plateZ(2, 2); ...
    plateX(2, 1) plateY(2, 1) plateZ(2, 1)];
  controls.supportTop = top;
  for index = 1:4
    base = [top(index, 1) top(index, 2) parameters.supportHeight];
    set(controls.supportLines(index), 'XData', [base(1) top(index, 1)], ...
      'YData', [base(2) top(index, 2)], ...
      'ZData', [base(3) top(index, 3)]);
  end
end
%=============================================================================
function ballOnPlateClearAnimation(controls)
  clearpoints(controls.ballTrail);
  clearpoints(controls.referenceTrail);
  clearpoints(controls.actualTrace);
  clearpoints(controls.referenceTrace);
  clearpoints(controls.alphaTrace);
  clearpoints(controls.betaTrace);
  clearpoints(controls.commandAlphaTrace);
  clearpoints(controls.commandBetaTrace);
  set(controls.currentMarker, 'XData', NaN, 'YData', NaN);
  set(controls.targetMarker, 'XData', NaN, 'YData', NaN);
  set(controls.titleText, 'String', 'Press Start to track the reference path');
end
%=============================================================================
function alive = ballOnPlateAnimationAlive(controls)
  alive = isgraphics(controls.figure) && isgraphics(controls.plateSurface) && ...
    isgraphics(controls.ballSurface) && isgraphics(controls.ballTrail) && ...
    isgraphics(controls.referenceTrail) && isgraphics(controls.actualTrace) && ...
    isgraphics(controls.referenceTrace) && isgraphics(controls.titleText);
end
%=============================================================================
function [X, Y, Z] = ballOnPlatePlateSurface(alpha, beta, parameters)
  s = parameters.plateHalfSize;
  localX = [-s s; -s s];
  localY = [-s -s; s s];
  localZ = zeros(2, 2);
  [X, Y, Z] = ballOnPlateTransformSurface(localX, localY, localZ, alpha, ...
    beta, [0 0 parameters.plateHeight]);
end
%=============================================================================
function height = ballOnPlatePlaneHeight(x, y, alpha, beta, parameters)
  rotation = ballOnPlatePlateRotation(alpha, beta);
  normal = rotation * [0; 0; 1];
  height = parameters.plateHeight - (normal(1) * x + normal(2) * y) / ...
    normal(3);
end
%=============================================================================
function [X, Y, Z] = ballOnPlateBallSurface(x, y, alpha, beta, parameters)
  [sphereX, sphereY, sphereZ] = sphere(18);
  z = ballOnPlatePlaneHeight(x, y, alpha, beta, parameters) + ...
    parameters.ballRadius;
  X = x + parameters.ballRadius * sphereX;
  Y = y + parameters.ballRadius * sphereY;
  Z = z + parameters.ballRadius * sphereZ;
end
%=============================================================================
function [X, Y, Z] = ballOnPlateTransformSurface(localX, localY, localZ, ...
  alpha, beta, offset)
  points = [localX(:) localY(:) localZ(:)] * ...
    ballOnPlatePlateRotation(alpha, beta).';
  points = points + repmat(offset, size(points, 1), 1);
  X = reshape(points(:, 1), size(localX));
  Y = reshape(points(:, 2), size(localY));
  Z = reshape(points(:, 3), size(localZ));
end
%=============================================================================
function rotation = ballOnPlatePlateRotation(alpha, beta)
  rotationY = [cos(alpha) 0 sin(alpha); 0 1 0; ...
    -sin(alpha) 0 cos(alpha)];
  rotationX = [1 0 0; 0 cos(beta) -sin(beta); ...
    0 sin(beta) cos(beta)];
  rotation = rotationY * rotationX;
end
%=============================================================================
