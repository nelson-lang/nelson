%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate a 2D incompressible jet hitting an obstacle: dye injected on the
% left is carried by the flow (pressure projection by Jacobi iterations,
% midpoint semi-Lagrangian advection, MacCormack correction for a sharp dye).
% Start runs the flow, Reset brings it back to rest, the list switches the
% obstacle, "Two jets" adds a second colour, the colour list shows the
% smoke, the vorticity or the smoke tinted by its vorticity, and "Fine smoke"
% trades the MacCormack correction for speed, all while the fluid keeps
% flowing. Close the window to stop.
if ~exist('fluidFigureVisible', 'var')
  fluidFigureVisible = 'on';
end
if ~exist('fluidFrameCount', 'var')
  fluidFrameCount = Inf;
end
if ~exist('fluidGridSize', 'var')
  fluidGridSize = 128;
end
if ~exist('fluidJacobiIterations', 'var')
  % Warm started from the previous frame, 20 sweeps leave the same residual
  % divergence as 40 or 80 (it comes from advection, not from the solver).
  fluidJacobiIterations = 20;
end
if ~exist('fluidAutoStart', 'var')
  fluidAutoStart = false;
end
if ~exist('fluidObstacle', 'var')
  fluidObstacle = 'Vertical plate';
end
if ~exist('fluidJetCount', 'var')
  fluidJetCount = 1;
end
if ~exist('fluidColorMode', 'var')
  fluidColorMode = 'Smoke';
end
if ~exist('fluidFineSmoke', 'var')
  fluidFineSmoke = true;
end

obstacleNames = fluidObstacleNames();
fluid = fluidCreateState(fluidGridSize, 2, fluidObstacle, fluidJetCount);
palette = fluidPalette();
backgroundColor = [0.02 0.03 0.06];
figureHandle = figure('Name', '2D fluid simulation', 'NumberTitle', 'off', ...
  'Color', backgroundColor, 'Visible', fluidFigureVisible);
axesHandle = axes('Parent', figureHandle, 'Position', [0.02 0.08 0.96 0.84]);
imageHandle = image('Parent', axesHandle, 'CData', ...
  fluidRender(fluid, fluidColorMode, palette));
obstacleHandle = patch('Parent', axesHandle, 'XData', fluid.outlineX, ...
  'YData', fluid.outlineY, 'FaceColor', [0.55 0.58 0.65], ...
  'EdgeColor', [0.85 0.88 0.95], 'LineWidth', 1.2);
axis(axesHandle, 'image');
axis(axesHandle, 'off');
set(axesHandle, 'YDir', 'normal');
titleHandle = title(axesHandle, '2D fluid jet - press Start', ...
  'Color', [0.90 0.93 1.00]);
startResetButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Start', 'Position', [12 8 86 28], 'UserData', 'none', ...
  'Callback', @fluidStartResetRequest);
obstacleMenu = uicontrol('Parent', figureHandle, 'Style', 'popupmenu', ...
  'String', obstacleNames, 'Position', [108 8 180 28], ...
  'Value', find(strcmp(obstacleNames, fluidObstacle)));
jetsCheckbox = uicontrol('Parent', figureHandle, 'Style', 'checkbox', ...
  'String', 'Two jets', 'Position', [300 8 110 28], 'Value', fluidJetCount == 2, ...
  'BackgroundColor', backgroundColor, 'ForegroundColor', [0.90 0.93 1.00]);
colorMenu = uicontrol('Parent', figureHandle, 'Style', 'popupmenu', ...
  'String', fluidColorModes(), 'Position', [420 8 180 28], ...
  'Value', find(strcmp(fluidColorModes(), fluidColorMode)));
fineSmokeCheckbox = uicontrol('Parent', figureHandle, 'Style', 'checkbox', ...
  'String', 'Fine smoke', 'Position', [612 8 110 28], 'Value', fluidFineSmoke, ...
  'BackgroundColor', backgroundColor, 'ForegroundColor', [0.90 0.93 1.00]);
fluidDraw(imageHandle, obstacleHandle, colorMenu, palette, fluid);
if fluidAutoStart
  set(startResetButton, 'UserData', 'start');
end

running = false;
frameIndex = 0;
clockStart = tic();
while frameIndex < fluidFrameCount && isgraphics(imageHandle)
  [running, fluid, frameIndex, clockStart] = fluidApplyRequest(startResetButton, ...
    running, fluid, frameIndex, clockStart, titleHandle);
  fluid = fluidApplyObstacleChoice(obstacleMenu, obstacleNames, fluid);
  fluid = fluidApplyJetChoice(jetsCheckbox, fluid);
  if isgraphics(fineSmokeCheckbox)
    fluid.fineSmoke = get(fineSmokeCheckbox, 'Value') ~= 0;
  end
  if ~running
    fluidDraw(imageHandle, obstacleHandle, colorMenu, palette, fluid);
    pause(0.03);
    drawnow();
    continue
  end
  frameIndex = frameIndex + 1;
  fluid = fluidStep(fluid, fluidJacobiIterations);
  fluidDraw(imageHandle, obstacleHandle, colorMenu, palette, fluid);
  if mod(frameIndex, 15) == 0
    set(titleHandle, 'String', sprintf('2D fluid jet - step %d - %.0f frames/s', ...
      frameIndex, frameIndex / toc(clockStart)));
  end
  drawnow();
end
%=============================================================================
function fluidStartResetRequest(source, event)
  if strcmp(get(source, 'String'), 'Start')
    set(source, 'UserData', 'start');
  else
    set(source, 'UserData', 'reset');
  end
end
%=============================================================================
function [running, fluid, frameIndex, clockStart] = fluidApplyRequest(button, ...
  running, fluid, frameIndex, clockStart, titleHandle)
  if ~isgraphics(button)
    return
  end
  request = get(button, 'UserData');
  if strcmp(request, 'none')
    return
  end
  set(button, 'UserData', 'none');
  if strcmp(request, 'start')
    running = true;
    clockStart = tic();
    set(button, 'String', 'Reset');
    return
  end
  running = false;
  frameIndex = 0;
  fluid = fluidCreateState(fluid.rows, fluid.cols / fluid.rows, fluid.obstacleName, ...
    numel(fluid.jets));
  set(button, 'String', 'Start');
  set(titleHandle, 'String', '2D fluid jet - press Start');
end
%=============================================================================
function fluid = fluidApplyObstacleChoice(menu, names, fluid)
  % The obstacle changes in place: the flow keeps its momentum and adapts.
  if ~isgraphics(menu)
    return
  end
  name = names{get(menu, 'Value')};
  if ~strcmp(name, fluid.obstacleName)
    fluid = fluidSetObstacle(fluid, name);
  end
end
%=============================================================================
function fluid = fluidApplyJetChoice(checkbox, fluid)
  % Adding or removing the second jet keeps the flow and the first dye.
  if ~isgraphics(checkbox)
    return
  end
  jetCount = 1 + (get(checkbox, 'Value') ~= 0);
  if jetCount ~= numel(fluid.jets)
    fluid = fluidSetJets(fluid, jetCount);
  end
end
%=============================================================================
function fluidDraw(imageHandle, obstacleHandle, colorMenu, palette, fluid)
  % The colour mode is read at every frame: switching it is immediate.
  colorModes = fluidColorModes();
  colorMode = colorModes{1};
  if isgraphics(colorMenu)
    colorMode = colorModes{get(colorMenu, 'Value')};
  end
  set(imageHandle, 'CData', fluidRender(fluid, colorMode, palette));
  % The outline only changes with the obstacle (or while it spins).
  if fluid.obstacleMoving || ~strcmp(get(obstacleHandle, 'UserData'), fluid.obstacleName)
    visibility = {'on', 'off'};
    set(obstacleHandle, 'XData', fluid.outlineX, 'YData', fluid.outlineY, ...
      'Visible', visibility{1 + isempty(fluid.outlineX)}, 'UserData', fluid.obstacleName);
  end
end
%=============================================================================
function state = fluidCreateState(rows, aspectRatio, obstacleName, jetCount)
  cols = rows * aspectRatio;
  [state.X, state.Y] = meshgrid(1:cols, 1:rows);
  state.rows = rows;
  state.cols = cols;
  state.vx = zeros(rows, cols);
  state.vy = zeros(rows, cols);
  state.dye = zeros(rows, cols, 0);
  state.pressure = zeros(rows + 2, cols + 2);
  state.inflowCols = max(2, round(rows / 20)) + (0:max(2, round(rows / 40)));
  state.inflowSpeed = 1;
  state.time = 0;
  state.fineSmoke = true;
  state = fluidSetJets(state, jetCount);
  state = fluidSetObstacle(state, obstacleName);
end
%=============================================================================
function state = fluidSetJets(state, jetCount)
  % One jet on the axis, or two jets aimed at each other so their dyes
  % collide and mix; each jet has its own dye layer.
  center = round(state.rows / 2);
  halfWidth = max(1, round(state.rows / 64));
  if jetCount == 1
    offsets = 0;
    aims = 0;
  else
    offsets = [1, -1] * round(state.rows / 8);
    aims = [-0.5, 0.5];
  end
  state.jets = struct('rows', {}, 'aim', {}, 'phase', {});
  for k = 1:jetCount
    state.jets(k).rows = center + offsets(k) + (-halfWidth:halfWidth);
    state.jets(k).aim = aims(k);
    % Quarter-period shift: a mirror-symmetric pair would keep a sharp,
    % unmixed interface on the axis.
    state.jets(k).phase = (k - 1) * pi / 2;
  end
  dye = zeros(state.rows, state.cols, jetCount);
  kept = min(jetCount, size(state.dye, 3));
  dye(:, :, 1:kept) = state.dye(:, :, 1:kept);
  state.dye = dye;
end
%=============================================================================
function state = fluidStep(state, jacobiIterations)
  state.time = state.time + 1;
  if state.obstacleMoving
    state = fluidSetObstacle(state, state.obstacleName);
  end
  % Light stream from the left edge (the zero-pressure border lets it leave
  % on the right) and jets with a slow transverse wobble.
  state.vx(:, 1:2) = 0.35 * state.inflowSpeed;
  for k = 1:numel(state.jets)
    jet = state.jets(k);
    state.vx(jet.rows, state.inflowCols) = state.inflowSpeed;
    state.vy(jet.rows, state.inflowCols) = state.inflowSpeed * ...
      (jet.aim + 0.12 * sin(state.time / 20 + jet.phase));
    state.dye(jet.rows, state.inflowCols, k) = 1;
  end
  state = fluidImposeSolid(state);
  state = fluidProject(state, jacobiIterations);

  % Backward midpoint trace: the first stage sits on grid nodes (no lookup).
  [x, y] = fluidClamp(state.X - state.vx / 2, state.Y - state.vy / 2, ...
    state.rows, state.cols);
  [x, y] = fluidClamp(state.X - interp2(state.vx, x, y), ...
    state.Y - interp2(state.vy, x, y), state.rows, state.cols);
  if state.fineSmoke
    state.dye = fluidAdvectSharp(state.dye, x, y, state);
  else
    % Plain semi-Lagrangian: one lookup per layer, softer smoke.
    for k = 1:size(state.dye, 3)
      state.dye(:, :, k) = 0.998 * interp2(state.dye(:, :, k), x, y);
    end
  end
  state.vx = interp2(state.vx, x, y);
  state.vy = interp2(state.vy, x, y);
  state = fluidImposeSolid(state);
end
%=============================================================================
function state = fluidImposeSolid(state)
  % Inside the obstacle the fluid moves with it (at rest unless it spins).
  % Linear indices touch only the obstacle cells, not the whole grid.
  index = state.solidIndex;
  if isempty(index)
    return
  end
  state.vx(index) = state.solidVx(index);
  state.vy(index) = state.solidVy(index);
  cellCount = state.rows * state.cols;
  for k = 1:size(state.dye, 3)
    state.dye(index + (k - 1) * cellCount) = 0;
  end
end
%=============================================================================
function state = fluidProject(state, jacobiIterations)
  % Pressure projection: divergence, Jacobi sweeps (warm started from the
  % previous frame), then gradient subtraction on interior nodes.
  % The pressure keeps a zero border so each sweep is four shifted slices.
  halfRhs = -fluidDivergence(state.vx, state.vy) / 2;
  q = state.pressure;
  I = 2:state.rows + 1;
  J = 2:state.cols + 1;
  for iteration = 1:jacobiIterations
    q(I, J) = (q(I - 1, J) + q(I + 1, J) + q(I, J - 1) + q(I, J + 1)) / 4 + halfRhs;
  end
  state.pressure = q;
  I = 3:state.rows;
  J = 3:state.cols;
  state.vx(2:end - 1, 2:end - 1) = state.vx(2:end - 1, 2:end - 1) - ...
    (q(I, J + 1) - q(I, J - 1)) / 2;
  state.vy(2:end - 1, 2:end - 1) = state.vy(2:end - 1, 2:end - 1) - ...
    (q(I + 1, J) - q(I - 1, J)) / 2;
end
%=============================================================================
function d = fluidDivergence(vx, vy)
  dx = [vx(:, 2) - vx(:, 1), (vx(:, 3:end) - vx(:, 1:end - 2)) / 2, ...
    vx(:, end) - vx(:, end - 1)];
  dy = [vy(2, :) - vy(1, :); (vy(3:end, :) - vy(1:end - 2, :)) / 2; ...
    vy(end, :) - vy(end - 1, :)];
  d = dx + dy;
end
%=============================================================================
function dye = fluidAdvectSharp(dye, x, y, state)
  % MacCormack: advect back, advect the result forward, correct half of the
  % round-trip error, then limit to the values of the source cell so the
  % correction never creates new extrema. The forward positions and the
  % source cells depend on the flow only: every dye layer shares them.
  rows = state.rows;
  cols = state.cols;
  [xf, yf] = fluidClamp(2 * state.X - x, 2 * state.Y - y, rows, cols);
  cell = min(floor(y), rows - 1) + (min(floor(x), cols - 1) - 1) * (rows - 1);
  for k = 1:size(dye, 3)
    layer = dye(:, :, k);
    predicted = interp2(layer, x, y);
    corrected = predicted + (layer - interp2(predicted, xf, yf)) / 2;
    a = layer(1:end - 1, 1:end - 1);
    b = layer(2:end, 1:end - 1);
    c = layer(1:end - 1, 2:end);
    d = layer(2:end, 2:end);
    cellMin = min(min(a, b), min(c, d));
    cellMax = max(max(a, b), max(c, d));
    dye(:, :, k) = 0.998 * min(max(corrected, cellMin(cell)), cellMax(cell));
  end
end
%=============================================================================
function [x, y] = fluidClamp(x, y, rows, cols)
  % Keep the traced positions inside the domain: interp2 returns NaN outside.
  x = min(max(x, 1), cols);
  y = min(max(y, 1), rows);
end
%=============================================================================
function names = fluidObstacleNames()
  names = {'None', 'Square', 'Vertical plate', 'Horizontal plate', 'Disc', ...
    'Airfoil', 'Wedge', 'Cylinder array', 'Rotating propeller'};
end
%=============================================================================
function state = fluidSetObstacle(state, name)
  % Each shape gives a solid mask, the velocity of the solid and its outline
  % (one polygon per column), all in grid coordinates.
  cx = state.cols / 3;
  cy = round(state.rows / 2);
  s = state.rows;
  dx = state.X - cx;
  dy = state.Y - cy;
  state.solidVx = zeros(state.rows, state.cols);
  state.solidVy = zeros(state.rows, state.cols);
  state.obstacleMoving = false;
  switch name
    case 'None'
      state.solid = false(state.rows, state.cols);
      state.outlineX = zeros(0, 1);
      state.outlineY = zeros(0, 1);
    case 'Square'
      [state.solid, state.outlineX, state.outlineY] = fluidBox(dx, dy, cx, cy, ...
        s / 10, s / 10, 0);
    case 'Vertical plate'
      [state.solid, state.outlineX, state.outlineY] = fluidBox(dx, dy, cx, cy, ...
        max(1, s / 32), s / 8, 0);
    case 'Horizontal plate'
      [state.solid, state.outlineX, state.outlineY] = fluidBox(dx, dy, cx, cy, ...
        s / 8, max(1, s / 32), 0);
    case 'Disc'
      [state.solid, state.outlineX, state.outlineY] = fluidDiscs(dx, dy, cx, cy, ...
        s / 10, [0 0]);
    case 'Airfoil'
      [state.solid, state.outlineX, state.outlineY] = fluidAirfoil(dx, dy, cx, cy, ...
        s / 2.5, 15 * pi / 180);
    case 'Wedge'
      [state.solid, state.outlineX, state.outlineY] = fluidWedge(dx, dy, cx, cy, ...
        s / 5, s / 8);
    case 'Cylinder array'
      r = max(1.5, s / 16);
      [state.solid, state.outlineX, state.outlineY] = fluidDiscs(dx, dy, cx, cy, ...
        r, [0 -2.6 * r; 0 2.6 * r; 5 * r 0]);
    case 'Rotating propeller'
      omega = 0.03;
      [state.solid, state.outlineX, state.outlineY] = fluidPropeller(dx, dy, cx, ...
        cy, s / 7, max(1, s / 40), omega * state.time);
      state.solidVx = -omega * dy;
      state.solidVy = omega * dx;
      state.obstacleMoving = true;
    otherwise
      error('Nelson:fluid:unknownObstacle', 'Unknown obstacle: %s', name);
  end
  state.solidIndex = find(state.solid);
  state.obstacleName = name;
end
%=============================================================================
function [solid, outlineX, outlineY] = fluidBox(dx, dy, cx, cy, halfX, halfY, angle)
  u = dx * cos(angle) + dy * sin(angle);
  v = -dx * sin(angle) + dy * cos(angle);
  solid = abs(u) <= halfX & abs(v) <= halfY;
  [outlineX, outlineY] = fluidRotate([-1; 1; 1; -1] * halfX, ...
    [-1; -1; 1; 1] * halfY, angle, cx, cy);
end
%=============================================================================
function [solid, outlineX, outlineY] = fluidDiscs(dx, dy, cx, cy, radius, centers)
  angle = linspace(0, 2 * pi, 48)';
  solid = false(size(dx));
  outlineX = zeros(numel(angle), size(centers, 1));
  outlineY = outlineX;
  for k = 1:size(centers, 1)
    solid = solid | hypot(dx - centers(k, 1), dy - centers(k, 2)) <= radius;
    outlineX(:, k) = cx + centers(k, 1) + radius * cos(angle);
    outlineY(:, k) = cy + centers(k, 2) + radius * sin(angle);
  end
end
%=============================================================================
function [solid, outlineX, outlineY] = fluidAirfoil(dx, dy, cx, cy, chord, attack)
  % Symmetric NACA 00xx profile centred on (cx, cy), nose up by attack.
  thickness = @(t) 5 * 0.15 * (0.2969 * sqrt(t) - 0.1260 * t - 0.3516 * t .^ 2 + ...
    0.2843 * t .^ 3 - 0.1015 * t .^ 4);
  u = dx * cos(attack) - dy * sin(attack) + chord / 2;
  v = dx * sin(attack) + dy * cos(attack);
  t = min(max(u / chord, 0), 1);
  solid = u >= 0 & u <= chord & abs(v) <= chord * thickness(t);
  t = (1 - cos(linspace(0, pi, 40)')) / 2;
  profileU = chord * [t; flipud(t)] - chord / 2;
  profileV = chord * [thickness(t); -flipud(thickness(t))];
  [outlineX, outlineY] = fluidRotate(profileU, profileV, -attack, cx, cy);
end
%=============================================================================
function [solid, outlineX, outlineY] = fluidWedge(dx, dy, cx, cy, len, halfBase)
  % Triangle pointing upstream, its flat base facing the wake.
  u = dx + len / 2;
  solid = u >= 0 & u <= len & abs(dy) <= halfBase * u / len;
  outlineX = cx + [-len / 2; len / 2; len / 2];
  outlineY = cy + [0; -halfBase; halfBase];
end
%=============================================================================
function [solid, outlineX, outlineY] = fluidPropeller(dx, dy, cx, cy, halfLength, ...
  halfWidth, angle)
  % Two crossed blades spinning about (cx, cy).
  u = dx * cos(angle) + dy * sin(angle);
  v = -dx * sin(angle) + dy * cos(angle);
  solid = (abs(u) <= halfLength & abs(v) <= halfWidth) | ...
    (abs(v) <= halfLength & abs(u) <= halfWidth);
  L = halfLength;
  w = halfWidth;
  crossU = [L; w; w; -w; -w; -L; -L; -w; -w; w; w; L];
  crossV = [w; w; L; L; w; w; -w; -w; -L; -L; -w; -w];
  [outlineX, outlineY] = fluidRotate(crossU, crossV, angle, cx, cy);
end
%=============================================================================
function [x, y] = fluidRotate(u, v, angle, cx, cy)
  x = cx + u * cos(angle) - v * sin(angle);
  y = cy + u * sin(angle) + v * cos(angle);
end
%=============================================================================
function modes = fluidColorModes()
  modes = {'Smoke', 'Vorticity', 'Smoke + vorticity'};
end
%=============================================================================
function rgb = fluidRender(fluid, colorMode, palette)
  % Colours come from lookup tables: one gather per channel and per frame.
  switch colorMode
    case 'Smoke'
      rgb = fluidSmokeColors(fluid.dye, palette);
    case 'Vorticity'
      spin = fluidSpinIndex(fluid, palette);
      rgb = cat(3, palette.vorticity{1}(spin), palette.vorticity{2}(spin), ...
        palette.vorticity{3}(spin));
    case 'Smoke + vorticity'
      amount = min(sum(fluid.dye, 3), 1);
      index = fluidSpinIndex(fluid, palette) + ...
        round(amount * (palette.tintLevels - 1)) * palette.spinLevels;
      rgb = cat(3, palette.tint{1}(index), palette.tint{2}(index), palette.tint{3}(index));
    otherwise
      error('Nelson:fluid:unknownColorMode', 'Unknown colour mode: %s', colorMode);
  end
end
%=============================================================================
function rgb = fluidSmokeColors(dye, palette)
  % Additive colours: jet 1 glows blue to white, jet 2 orange to white, and
  % where they mix the colours add up.
  index = round(dye(:, :, 1) * (palette.smokeLevels - 1)) + 1;
  red = palette.smoke{1, 1}(index);
  green = palette.smoke{1, 2}(index);
  blue = palette.smoke{1, 3}(index);
  for k = 2:size(dye, 3)
    index = round(dye(:, :, k) * (palette.smokeLevels - 1)) + 1;
    red = min(red + palette.smoke{k, 1}(index), 1);
    green = min(green + palette.smoke{k, 2}(index), 1);
    blue = min(blue + palette.smoke{k, 3}(index), 1);
  end
  rgb = cat(3, red, green, blue);
end
%=============================================================================
function index = fluidSpinIndex(fluid, palette)
  % Vorticity dvy/dx - dvx/dy on interior nodes, quantized on the palette:
  % positive is counter-clockwise, negative clockwise.
  omega = zeros(fluid.rows, fluid.cols);
  omega(2:end - 1, 2:end - 1) = ...
    (fluid.vy(2:end - 1, 3:end) - fluid.vy(2:end - 1, 1:end - 2) - ...
    fluid.vx(3:end, 2:end - 1) + fluid.vx(1:end - 2, 2:end - 1)) / 2;
  omega(fluid.solidIndex) = 0;
  % The shear against the open border is a boundary artefact, not a vortex.
  omega([1:2, end - 1:end], :) = 0;
  omega(:, [1:2, end - 1:end]) = 0;
  index = round(omega * palette.spinPerVorticity) + palette.spinCenter;
  index = min(max(index, 1), palette.spinLevels);
end
%=============================================================================
function palette = fluidPalette()
  % Lookup tables built once. Smoke: per jet and channel, indexed by the
  % dye. Vorticity: red counter-clockwise, blue clockwise, fading to the
  % background where the fluid does not turn. Tint: the same hue, with the
  % brightness of the smoke.
  background = [0.02 0.03 0.06];
  exponents = [2.2 1.1 0.55; 0.55 1.1 2.2];
  neutral = [0.92 0.94 1.00];
  counterClockwise = [1.00 0.32 0.16];
  clockwise = [0.18 0.55 1.00];
  vorticityScale = 0.08;
  palette.smokeLevels = 256;
  palette.spinLevels = 257;
  palette.tintLevels = 64;
  palette.spinCenter = (palette.spinLevels + 1) / 2;
  palette.spinPerVorticity = (palette.spinLevels - 1) / (6 * vorticityScale);
  level = linspace(0, 1, palette.smokeLevels)';
  spin = tanh(linspace(-3, 3, palette.spinLevels)');
  amount = linspace(0, 1, palette.tintLevels) .^ 0.8;
  palette.smoke = cell(2, 3);
  palette.vorticity = cell(1, 3);
  palette.tint = cell(1, 3);
  for c = 1:3
    palette.smoke{1, c} = background(c) + (1 - background(c)) * level .^ exponents(1, c);
    palette.smoke{2, c} = (1 - background(c)) * level .^ exponents(2, c);
    % Pure vorticity: the sign picks the hue, the magnitude its strength; a
    % small dead zone keeps the weak background swirl black.
    signHue = clockwise(c) + (spin > 0) * (counterClockwise(c) - clockwise(c));
    strength = max(abs(spin) - 0.1, 0) / 0.9;
    palette.vorticity{c} = background(c) + strength .^ 0.7 .* (signHue - background(c));
    % Tinted smoke: pale where the fluid does not turn.
    hue = neutral(c) + max(spin, 0) * (counterClockwise(c) - neutral(c)) + ...
      max(-spin, 0) * (clockwise(c) - neutral(c));
    tint = background(c) + (hue - background(c)) * amount;
    palette.tint{c} = tint(:);
  end
end
%=============================================================================