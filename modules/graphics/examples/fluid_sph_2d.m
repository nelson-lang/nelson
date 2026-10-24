%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Interactive 2D fluid made of separate particles, solved by Smoothed
% Particle Hydrodynamics (SPH): each particle carries mass and velocity, the
% density comes from a smoothing kernel over its neighbours, pressure keeps
% the particles apart, viscosity smooths the flow. The particles fill the
% whole tank and bounce off its walls; they are drawn as distinct dots tinted
% by their speed. Drag with the mouse to stir them (left button pushes them
% away, right button pulls them in). Start and Stop run the motion and Reset
% refills the tank. Close the window to stop.
if ~exist('sphFigureVisible', 'var')
  sphFigureVisible = 'on';
end
if ~exist('sphFrameCount', 'var')
  sphFrameCount = Inf;
end
if ~exist('sphParticleCount', 'var')
  % 3000 particles still run around 30 frames/s: the neighbour pairs are
  % filtered to the interaction radius (see sphNeighborPairs) and the scatter
  % render is nearly free, so the SPH step is the only real cost.
  sphParticleCount = 3000;
end
if ~exist('sphAutoStart', 'var')
  sphAutoStart = false;
end
if ~exist('sphGravity', 'var')
  sphGravity = 'Off';
end
if ~exist('sphBatch', 'var')
  sphBatch = false;
end

params = sphParameters();
sph = sphCreateState(sphParticleCount, params, sphGravity);
backgroundColor = params.background;

figureHandle = figure('Name', '2D SPH particles', 'NumberTitle', 'off', ...
  'Color', backgroundColor, 'Visible', sphFigureVisible);
axesHandle = axes('Parent', figureHandle, 'Position', [0.04 0.10 0.92 0.85]);
hold(axesHandle, 'on');
tankHandle = plot(axesHandle, sph.box([1 2 2 1 1]), sph.box([3 3 4 4 3]), ...
  'Color', [0.30 0.55 0.92], 'LineWidth', 1.6);
particleHandle = scatter(axesHandle, sph.x, sph.y, params.markerSize, ...
  sphSpeedColors(hypot(sph.vx, sph.vy), params), 'filled');
hold(axesHandle, 'off');
axis(axesHandle, 'equal');
axis(axesHandle, sph.box + [-1 1 -1 1] * params.spacing);
axis(axesHandle, 'off');
set(axesHandle, 'YDir', 'normal', 'UserData', sphMouseState());
set(figureHandle, 'WindowButtonMotionFcn', @(s, e) sphMouseMove(s, e, axesHandle), ...
  'WindowButtonDownFcn', @(s, e) sphMouseButton(s, e, axesHandle, true), ...
  'WindowButtonUpFcn', @(s, e) sphMouseButton(s, e, axesHandle, false));
titleHandle = title(axesHandle, 'SPH particles - press Start', ...
  'Color', [0.90 0.94 1.00], 'FontSize', 12);
text(axesHandle, mean(sph.box(1:2)), sph.box(4) + params.spacing, ...
  'Drag to stir - left button pushes - right button pulls', ...
  'Color', [0.55 0.68 0.92], 'FontSize', 9, ...
  'HorizontalAlignment', 'center', 'VerticalAlignment', 'bottom');
startStopButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Start', 'Position', [12 10 84 28], 'UserData', 'none', ...
  'Callback', @sphStartStopRequest);
resetButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Reset', 'Position', [104 10 84 28], 'UserData', 'none', ...
  'Callback', @sphResetRequest);

running = false;
frameIndex = 0;
clockStart = tic();
if sphAutoStart
  running = true;
end
while frameIndex < sphFrameCount && isgraphics(particleHandle)
  [running, sph, frameIndex, clockStart] = sphApplyRequests(startStopButton, ...
    resetButton, running, sph, params, frameIndex, clockStart, titleHandle);
  if ~running
    drawnow();
    if sphBatch
      break
    end
    pause(0.02);
    continue
  end
  frameIndex = frameIndex + 1;
  sph = sphStep(sph, params, sphMouseForce(axesHandle, sph, params));
  set(particleHandle, 'XData', sph.x, 'YData', sph.y, ...
    'CData', sphSpeedColors(hypot(sph.vx, sph.vy), params));
  if mod(frameIndex, 12) == 0
    set(titleHandle, 'String', sprintf('SPH particles - %d particles - %.0f frames/s', ...
      sph.count, frameIndex / max(toc(clockStart), eps)));
  end
  drawnow();
  if sphBatch && frameIndex >= sphFrameCount
    break
  end
end
%=============================================================================
function params = sphParameters()
  % Kernel and material constants. The smoothing radius h sets the scale;
  % the 2D kernel norms below are the standard Poly6 (density), Spiky
  % gradient (pressure) and viscosity Laplacian.
  h = 3.2;
  params.h = h;
  params.hSquared = h * h;
  params.mass = 1.0;
  params.poly6 = 4 / (pi * h ^ 8);
  params.spikyGradient = 30 / (pi * h ^ 5);
  params.viscosityLaplacian = 40 / (pi * h ^ 5);
  params.stiffness = 400.0;
  % Very low viscosity and almost perfectly elastic walls barely dissipate
  % energy, so the particles keep churning vigorously across the tank.
  params.viscosity = 0.04;
  params.gravity = 9.0;
  % A single, larger substep advances the flow the same amount per frame as
  % two smaller ones did and stays stable, but halves the force evaluations.
  params.timeStep = 0.012;
  params.substeps = 1;
  params.boundaryDamping = 0.99;
  params.spacing = h * 0.62;
  params.initialSpeed = 22.0;
  % A small random velocity kick each step keeps the agitation lively for
  % good instead of letting the flow settle: it balances the slow viscous and
  % wall losses, holding a steady, energetic churn.
  params.agitation = 0.5;
  params.speedScale = 22.0;
  params.markerSize = 22;
  params.mouseRadius = 6 * h;
  params.mousePush = 120;
  params.mousePull = 90;
  params.background = [0.02 0.03 0.07];
  params.slowColor = [0.16 0.42 0.85];
  params.fastColor = [0.85 0.96 1.00];
end
%=============================================================================
function state = sphCreateState(count, params, gravityName)
  % Fill the whole rectangular tank with a regular grid of particles and give
  % each one a random initial velocity, so they bounce across the tank from
  % the start. The tank is a landscape rectangle sized to hold the grid.
  rng(7);
  aspectRatio = 1.4;
  columns = max(2, round(sqrt(count * aspectRatio)));
  rows = ceil(count / columns);
  spacing = params.spacing;
  margin = spacing;
  [gridX, gridY] = meshgrid(0:columns - 1, 0:rows - 1);
  x = margin + gridX(:) * spacing;
  y = margin + gridY(:) * spacing;
  x = x(1:count) + 0.02 * spacing * (rand(count, 1) - 0.5);
  y = y(1:count) + 0.02 * spacing * (rand(count, 1) - 0.5);
  angle = 2 * pi * rand(count, 1);
  speed = params.initialSpeed * (0.4 + rand(count, 1));
  state.count = count;
  state.x = x;
  state.y = y;
  state.vx = speed .* cos(angle);
  state.vy = speed .* sin(angle);
  state.box = [0, (columns - 1) * spacing + 2 * margin, ...
    0, (rows - 1) * spacing + 2 * margin];
  state.frame = 0;
  state = sphSetGravity(state, gravityName);
  % Rest density = the density of the filled packing, so the pressure only
  % reacts to compression above it.
  [pairI, pairJ] = sphNeighborPairs(state.x, state.y, params.h);
  density = sphDensity(state.x, state.y, pairI, pairJ, params);
  state.restDensity = median(density);
end
%=============================================================================
function state = sphSetGravity(state, gravityName)
  switch gravityName
    case 'Off'
      direction = [0, 0];
    case 'Down'
      direction = [0, -1];
    case 'Up'
      direction = [0, 1];
    case 'Left'
      direction = [-1, 0];
    case 'Right'
      direction = [1, 0];
    otherwise
      error('Nelson:sph:unknownGravity', 'Unknown gravity: %s', gravityName);
  end
  state.gravityName = gravityName;
  state.gravityDirection = direction;
end
%=============================================================================
function [pairI, pairJ] = sphNeighborPairs(x, y, h)
  % Build the list of neighbour pairs (pairI, pairJ) within the interaction
  % radius h using a uniform grid, so the forces cost O(neighbours) instead
  % of the O(count^2) of a full pairwise sweep. Each particle is placed in a
  % cell of side h; candidate pairs are gathered from the cell and its eight
  % neighbours. A one-cell pad keeps every shifted cell index in range.
  % The 3x3 block spans 3h, but only pairs closer than h contribute a force,
  % so the candidates are filtered to r < h at the end: this drops about two
  % thirds of them and makes the force step roughly three times cheaper.
  % Every particle keeps its self pair (r = 0, the density self term).
  cellSize = h;
  gridX = floor((x - (min(x) - h)) / cellSize);
  gridY = floor((y - (min(y) - h)) / cellSize);
  strideX = max(gridX) + 3;
  cellCount = strideX * (max(gridY) + 3);
  cell = gridX + gridY * strideX;
  [~, order] = sort(cell);
  counts = accumarray(cell + 1, 1, [cellCount 1]);
  cellStart = cumsum(counts) - counts;
  pairI = [];
  pairJ = [];
  for deltaCellY = -1:1
    for deltaCellX = -1:1
      targetCell = cell + deltaCellX + deltaCellY * strideX;
      startInSorted = cellStart(targetCell + 1);
      neighbourCount = counts(targetCell + 1);
      source = find(neighbourCount > 0);
      if isempty(source)
        continue
      end
      perSource = neighbourCount(source);
      total = sum(perSource);
      segmentId = repelem((1:numel(source))', perSource);
      within = (1:total)' - repelem([0; cumsum(perSource(1:end - 1))], perSource);
      sortedPosition = repelem(startInSorted(source), perSource) + within;
      pairJ = [pairJ; order(sortedPosition)];
      pairI = [pairI; source(segmentId)];
    end
  end
  % Keep only the candidates within the interaction radius (self pairs, at
  % r = 0, are kept). Everything downstream then works on this smaller set.
  deltaX = x(pairI) - x(pairJ);
  deltaY = y(pairI) - y(pairJ);
  withinRadius = deltaX .* deltaX + deltaY .* deltaY < h * h;
  pairI = pairI(withinRadius);
  pairJ = pairJ(withinRadius);
end
%=============================================================================
function density = sphDensity(x, y, pairI, pairJ, params)
  % Poly6 estimate summed over the neighbour pairs: density(i) = mass *
  % sum_j W(|x_i - x_j|). The r = 0 self pair is included, so every particle
  % keeps a finite density.
  deltaX = x(pairI) - x(pairJ);
  deltaY = y(pairI) - y(pairJ);
  % x .* x is markedly faster than x .^ 2 in the interpreter, and this runs
  % over every neighbour pair, so the kernel powers are written as products.
  weight = max(params.hSquared - deltaX .* deltaX - deltaY .* deltaY, 0);
  weight = weight .* weight .* weight;
  density = accumarray(pairI, weight, [numel(x) 1]) * params.mass * params.poly6;
end
%=============================================================================
function state = sphStep(state, params, mouseForce)
  % Neighbours are rebuilt once per frame (the particles move little across
  % the few substeps) and reused by every substep. Several small substeps
  % keep the stiff pressure stable while the frame still advances visibly.
  [pairI, pairJ] = sphNeighborPairs(state.x, state.y, params.h);
  particleCount = numel(state.x);
  for substep = 1:params.substeps
    [accelerationX, accelerationY] = sphAcceleration(state, params, ...
      pairI, pairJ, mouseForce);
    state.vx = state.vx + accelerationX * params.timeStep + ...
      params.agitation * randn(particleCount, 1);
    state.vy = state.vy + accelerationY * params.timeStep + ...
      params.agitation * randn(particleCount, 1);
    state.x = state.x + state.vx * params.timeStep;
    state.y = state.y + state.vy * params.timeStep;
    state = sphBounce(state, params);
  end
  state.frame = state.frame + 1;
end
%=============================================================================
function [accelerationX, accelerationY] = sphAcceleration(state, params, ...
  pairI, pairJ, mouseForce)
  % Forces summed over the neighbour pairs. Pressure and viscosity are added
  % into a single accumulation per axis, so the whole step needs only three
  % scatter-adds (density, force x, force y).
  x = state.x;
  y = state.y;
  count = numel(x);
  deltaX = x(pairI) - x(pairJ);
  deltaY = y(pairI) - y(pairJ);
  % Kernel powers are written as products (x .* x rather than x .^ 2): the
  % interpreter evaluates them several times faster over every pair.
  radiusSquared = deltaX .* deltaX + deltaY .* deltaY;
  weight = max(params.hSquared - radiusSquared, 0);
  density = accumarray(pairI, weight .* weight .* weight, ...
    [count 1]) * params.mass * params.poly6;
  pressure = params.stiffness * (density - state.restDensity);

  radius = sqrt(radiusSquared);
  near = radiusSquared < params.hSquared & radiusSquared > 0;
  safeRadius = radius + (radius == 0);
  edge = max(params.h - radius, 0);

  % Pressure force: symmetric (p_i + p_j) / (2 rho_j) along the Spiky
  % gradient (dense clusters push their neighbours away). Viscosity force:
  % pulls each particle towards the mean velocity of its neighbours.
  pressurePair = params.mass * (pressure(pairI) + pressure(pairJ)) ./ ...
    (2 * density(pairJ)) .* params.spikyGradient .* edge .* edge ./ safeRadius .* near;
  viscousPair = params.viscosity * params.mass * ...
    params.viscosityLaplacian * edge ./ density(pairJ) .* near;
  forceX = accumarray(pairI, ...
    pressurePair .* deltaX + viscousPair .* (state.vx(pairJ) - state.vx(pairI)), ...
    [count 1]);
  forceY = accumarray(pairI, ...
    pressurePair .* deltaY + viscousPair .* (state.vy(pairJ) - state.vy(pairI)), ...
    [count 1]);

  accelerationX = forceX ./ density + params.gravity * state.gravityDirection(1);
  accelerationY = forceY ./ density + params.gravity * state.gravityDirection(2);
  accelerationX = accelerationX + mouseForce.x;
  accelerationY = accelerationY + mouseForce.y;
end
%=============================================================================
function state = sphBounce(state, params)
  % Reflect at the tank walls and keep most of the normal speed on impact,
  % so the particles bounce back into the tank instead of sticking.
  damping = params.boundaryDamping;
  low = state.x < state.box(1);
  state.x(low) = state.box(1);
  state.vx(low) = -damping * state.vx(low);
  high = state.x > state.box(2);
  state.x(high) = state.box(2);
  state.vx(high) = -damping * state.vx(high);
  low = state.y < state.box(3);
  state.y(low) = state.box(3);
  state.vy(low) = -damping * state.vy(low);
  high = state.y > state.box(4);
  state.y(high) = state.box(4);
  state.vy(high) = -damping * state.vy(high);
end
%=============================================================================
function rgb = sphSpeedColors(speed, params)
  % Slow particles are deep blue, fast particles brighten towards white.
  level = min(speed / params.speedScale, 1);
  rgb = params.slowColor + level .* (params.fastColor - params.slowColor);
  rgb = min(max(rgb, 0), 1);
end
%=============================================================================
function force = sphMouseForce(axesHandle, state, params)
  % A radial pulse around the cursor while a mouse button is held: the left
  % button pushes the particles away, the right button draws them in.
  force.x = 0;
  force.y = 0;
  if ~isgraphics(axesHandle)
    return
  end
  mouse = get(axesHandle, 'UserData');
  if ~isstruct(mouse) || ~mouse.active
    return
  end
  deltaX = state.x - mouse.x;
  deltaY = state.y - mouse.y;
  distance = hypot(deltaX, deltaY) + 1e-6;
  inside = distance < params.mouseRadius;
  falloff = inside .* (1 - distance / params.mouseRadius);
  strength = params.mousePush;
  sign = 1;
  if mouse.button == 3
    strength = params.mousePull;
    sign = -1;
  end
  magnitude = sign * strength * falloff ./ distance;
  force.x = magnitude .* deltaX;
  force.y = magnitude .* deltaY;
end
%=============================================================================
function mouse = sphMouseState()
  mouse.active = false;
  mouse.button = 1;
  mouse.x = 0;
  mouse.y = 0;
end
%=============================================================================
function sphMouseMove(~, ~, axesHandle)
  if ~isgraphics(axesHandle)
    return
  end
  mouse = get(axesHandle, 'UserData');
  [mouse.x, mouse.y] = sphCursor(axesHandle, mouse.x, mouse.y);
  set(axesHandle, 'UserData', mouse);
end
%=============================================================================
function sphMouseButton(figureHandle, ~, axesHandle, pressed)
  if ~isgraphics(axesHandle)
    return
  end
  mouse = get(axesHandle, 'UserData');
  mouse.active = pressed;
  if pressed
    selection = get(figureHandle, 'SelectionType');
    mouse.button = 1 + 2 * strcmp(selection, 'alt');
    [mouse.x, mouse.y] = sphCursor(axesHandle, mouse.x, mouse.y);
  end
  set(axesHandle, 'UserData', mouse);
end
%=============================================================================
function [cursorX, cursorY] = sphCursor(axesHandle, cursorX, cursorY)
  % Read the cursor in data coordinates, keeping the last value when the
  % current point is unavailable (for instance on a hidden figure).
  point = get(axesHandle, 'CurrentPoint');
  if size(point, 1) >= 1 && size(point, 2) >= 2
    cursorX = point(1, 1);
    cursorY = point(1, 2);
  end
end
%=============================================================================
function [running, sph, frameIndex, clockStart] = sphApplyRequests(startStopButton, ...
  resetButton, running, sph, params, frameIndex, clockStart, titleHandle)
  if isgraphics(resetButton) && strcmp(get(resetButton, 'UserData'), 'reset')
    set(resetButton, 'UserData', 'none');
    sph = sphCreateState(sph.count, params, sph.gravityName);
    running = false;
    frameIndex = 0;
    set(startStopButton, 'String', 'Start');
    set(titleHandle, 'String', 'SPH particles - press Start');
  end
  if ~isgraphics(startStopButton)
    return
  end
  request = get(startStopButton, 'UserData');
  if strcmp(request, 'none')
    return
  end
  set(startStopButton, 'UserData', 'none');
  if strcmp(request, 'start')
    running = true;
    clockStart = tic();
    frameIndex = 0;
    set(startStopButton, 'String', 'Stop');
  else
    running = false;
    set(startStopButton, 'String', 'Start');
  end
end
%=============================================================================
function sphStartStopRequest(source, ~)
  if strcmp(get(source, 'String'), 'Start')
    set(source, 'UserData', 'start');
  else
    set(source, 'UserData', 'stop');
  end
end
%=============================================================================
function sphResetRequest(source, ~)
  set(source, 'UserData', 'reset');
end
%=============================================================================
