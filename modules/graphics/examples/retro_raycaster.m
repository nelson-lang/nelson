%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Explore a procedural maze with a software-rendered raycasting engine.
if ~exist('raycasterFigureVisible', 'var')
  raycasterFigureVisible = 'on';
end
if ~exist('raycasterBatch', 'var')
  raycasterBatch = false;
end
if ~exist('raycasterMaxFrames', 'var')
  raycasterMaxFrames = Inf;
end
if ~exist('raycasterWidth', 'var')
  raycasterWidth = 400;
end
if ~exist('raycasterHeight', 'var')
  raycasterHeight = 225;
end
if ~exist('raycasterAutopilot', 'var')
  raycasterAutopilot = false;
end
if ~exist('raycasterStartFullscreen', 'var')
  raycasterStartFullscreen = true;
end
if ~exist('raycasterShowMinimap', 'var')
  raycasterShowMinimap = false;
end
if ~exist('raycasterOutputImage', 'var')
  raycasterOutputImage = '';
end

raycasterWorldMap = raycasterCreateWorld();
[raycasterTextures, raycasterSpriteColor, raycasterSpriteAlpha, ...
  raycasterWeaponColor, raycasterWeaponAlpha] = raycasterLoadTextures();
raycasterEnemies = [6.5 3.5; 15.5 12.5; 5.5 14.5];
raycasterPlayerX = 3.5;
raycasterPlayerY = 3.5;
raycasterPlayerAngle = 0;
raycasterState = raycasterInitialState(raycasterPlayerX, ...
  raycasterPlayerY, raycasterPlayerAngle);

screenSize = get(0, 'ScreenSize');
figureWidth = min(960, max(640, screenSize(3) - 80));
figureHeight = min(690, max(480, screenSize(4) - 100));
figureLeft = screenSize(1) + max(20, (screenSize(3) - figureWidth) / 2);
figureBottom = screenSize(2) + max(35, (screenSize(4) - figureHeight) / 2);
figureHandle = figure('SelectionHighlight', 'off', 'Name', 'Nelson retro raycaster', ...
  'NumberTitle', 'off', 'Color', [0.015 0.018 0.028], ...
  'MenuBar', 'none', 'ToolBar', 'none', 'Resize', 'on', ...
  'Visible', raycasterFigureVisible, ...
  'Position', [figureLeft figureBottom figureWidth figureHeight], ...
  'WindowKeyPressFcn', @(source, event) ...
  raycasterKeyEvent(source, event, true), ...
  'WindowKeyReleaseFcn', @(source, event) ...
  raycasterKeyEvent(source, event, false), ...
  'WindowScrollWheelFcn', @(source, event) ...
  raycasterScrollWheelEvent(source, event), 'UserData', raycasterState);
axesHandle = axes('Parent', figureHandle, 'Position', [0 0 1 1]);
raycasterRenderWidth = raycasterWidth;
raycasterRenderHeight = raycasterHeight;
raycasterFrame = zeros(raycasterRenderHeight, raycasterRenderWidth, 3);
imageHandle = image('Parent', axesHandle, 'CData', raycasterFrame, ...
  'XData', [1 raycasterRenderWidth], 'YData', [1 raycasterRenderHeight]);
axis(axesHandle, 'tight');
axis(axesHandle, 'off');
hold(axesHandle, 'on');
raycasterEnemyHandles = cell(1, size(raycasterEnemies, 1));
for enemyIndex = 1:numel(raycasterEnemyHandles)
  raycasterEnemyHandles{enemyIndex} = image('Parent', axesHandle, ...
    'CData', raycasterSpriteColor, 'AlphaData', raycasterSpriteAlpha, ...
    'AlphaDataMapping', 'none', 'XData', [1 2], 'YData', [1 2], ...
    'Visible', 'off');
end
raycasterWeaponHandle = image('Parent', axesHandle, ...
  'CData', raycasterWeaponColor, 'AlphaData', raycasterWeaponAlpha, ...
  'AlphaDataMapping', 'none', 'XData', [1 2], 'YData', [1 2], ...
  'Visible', 'off');
set(axesHandle, 'XLim', [0.5 raycasterWidth + 0.5], ...
  'YLim', [0.5 raycasterHeight + 0.5], ...
  'XLimMode', 'manual', 'YLimMode', 'manual', 'YDir', 'reverse');
statusVisibility = 'on';
if raycasterStartFullscreen
  statusVisibility = 'off';
end
statusText = uicontrol('Parent', figureHandle, 'Style', 'text', ...
  'String', 'Z/W, S, Q/A, D, arrows | F fullscreen | Esc quit', ...
  'HorizontalAlignment', 'center', 'FontName', 'Monospace', 'FontSize', 11, ...
  'ForegroundColor', [0.72 0.92 0.86], 'BackgroundColor', [0.015 0.018 0.028], ...
  'Tag', 'raycaster-status', 'Visible', statusVisibility);
set(statusText, 'Units', 'normalized');
set(statusText, 'Position', [0.01 0.006 0.98 0.052]);
set(statusText, 'KeyPressFcn', @(source, event) ...
  raycasterKeyEvent(source, event, true));
set(statusText, 'KeyReleaseFcn', @(source, event) ...
  raycasterKeyEvent(source, event, false));
if raycasterStartFullscreen
  delete(statusText);
  statusText = [];
end
fpsText = text(axesHandle, raycasterWidth - 4, 5, '-- FPS', ...
  'HorizontalAlignment', 'right', 'VerticalAlignment', 'top', ...
  'FontName', 'Monospace', 'FontSize', 13, 'FontWeight', 'bold', ...
  'Color', [0.72 1.00 0.78], 'Tag', 'raycaster-fps');
if raycasterStartFullscreen
  set(figureHandle, 'WindowState', 'fullscreen');
end

targetFrameRate = 61;
targetFrameDuration = 1 / targetFrameRate;
raycasterFrameIndex = 0;
raycasterClock = tic();
nextFrameTime = targetFrameDuration;
previousTime = 0;
fpsStartTime = 0;
fpsFrameCount = 0;
measuredFps = 0;
raycasterLoopTimer = tic();
while isgraphics(figureHandle) && isgraphics(imageHandle) && ...
  raycasterFrameIndex < raycasterMaxFrames
  frameTimer = tic();
  currentTime = toc(raycasterClock);
  if raycasterBatch
    deltaTime = targetFrameDuration;
  else
    deltaTime = min(0.05, max(0.001, currentTime - previousTime));
  end
  previousTime = currentTime;
  raycasterState = get(figureHandle, 'UserData');
  if raycasterState.quit
    break
  end
  if raycasterAutopilot
    raycasterState.forward = true;
    raycasterState.turnRight = mod(raycasterFrameIndex, 150) >= 105;
  end
  raycasterState = raycasterMovePlayer(raycasterState, ...
    raycasterWorldMap, deltaTime);
  raycasterPlayerX = raycasterState.x;
  raycasterPlayerY = raycasterState.y;
  raycasterPlayerAngle = raycasterState.angle;
  [raycasterFrame, raycasterRayDistance] = raycasterRenderFrame(...
    raycasterWorldMap, raycasterTextures, raycasterState, ...
    raycasterRenderWidth, raycasterRenderHeight, raycasterShowMinimap);
  set(imageHandle, 'CData', raycasterFrame);
  raycasterUpdateTextureOverlays(raycasterEnemyHandles, ...
    raycasterWeaponHandle, raycasterEnemies, raycasterState, ...
    raycasterRayDistance, raycasterRenderWidth, raycasterRenderHeight, ...
    currentTime);
  % The key callbacks write the input flags into UserData and may run at any
  % statement boundary of this loop body (the web host dispatches a key event
  % as soon as its command is drained). Writing back the copy read at the top
  % of the frame would overwrite a press or release that landed meanwhile and
  % leave a movement flag latched (e.g. turning forever after the key is let
  % go). Re-read the live state and store only the fields this loop owns.
  raycasterLiveState = get(figureHandle, 'UserData');
  raycasterLiveState.x = raycasterState.x;
  raycasterLiveState.y = raycasterState.y;
  raycasterLiveState.angle = raycasterState.angle;
  raycasterLiveState.moving = raycasterState.moving;
  set(figureHandle, 'UserData', raycasterLiveState);
  raycasterFrameIndex = raycasterFrameIndex + 1;
  fpsFrameCount = fpsFrameCount + 1;
  fpsDuration = currentTime - fpsStartTime;
  if fpsDuration >= 1
    measuredFps = fpsFrameCount / fpsDuration;
    fpsStartTime = currentTime;
    fpsFrameCount = 0;
    if isgraphics(statusText)
      set(statusText, 'String', sprintf(...
        ['Z/W forward  S back  Q/A/D strafe  arrows turn  F fullscreen  ' ...
        'Esc quit  |  ' ...
        '%.1f FPS  %dx%d'], measuredFps, raycasterRenderWidth, ...
        raycasterRenderHeight));
    end
    set(fpsText, 'String', sprintf('%.1f FPS', measuredFps));
  end
  drawnow();
  if ~raycasterBatch
    remainingTime = nextFrameTime - toc(raycasterClock);
    if remainingTime > 0
      % Cooperative wait: pause() keeps servicing key/mouse callbacks while it
      % paces the frame. An empty busy-wait spin would run no statement, so on
      % the single-threaded WebAssembly build it would pump no input at all.
      pause(remainingTime);
    end
    nextFrameTime = nextFrameTime + targetFrameDuration;
    if nextFrameTime < toc(raycasterClock)
      nextFrameTime = toc(raycasterClock) + targetFrameDuration;
    end
  end
end
raycasterLoopElapsed = toc(raycasterLoopTimer);
if ~isempty(raycasterOutputImage) && isgraphics(figureHandle)
  drawnow();
  saveas(figureHandle, raycasterOutputImage);
end
%=============================================================================
function worldMap = raycasterCreateWorld()
  worldMap = zeros(18, 22);
  worldMap([1 end], :) = 1;
  worldMap(:, [1 end]) = 1;
  worldMap(6, 3:10) = 1;
  worldMap(2:6, 11) = 2;
  worldMap(4, 14:19) = 3;
  worldMap(4:10, 19) = 3;
  worldMap(10, 13:19) = 4;
  worldMap(8:14, 7) = 2;
  worldMap(14, 3:7) = 2;
  worldMap(13, 10:15) = 1;
  worldMap(13:17, 15) = 1;
  worldMap(16, 17:21) = 3;
  worldMap(8, 10:13) = 4;
  worldMap(8:11, 13) = 4;
end
%=============================================================================
function state = raycasterInitialState(x, y, angle)
  state = struct('x', x, 'y', y, 'angle', angle, ...
    'forward', false, 'backward', false, 'strafeLeft', false, ...
    'strafeRight', false, 'turnLeft', false, 'turnRight', false, ...
    'quit', false, 'moving', false, 'fullscreenKeyDown', false);
end
%=============================================================================
function raycasterKeyEvent(source, event, pressed)
  if ~isgraphics(source)
    return
  end
  figureSource = source;
  if ~isgraphics(figureSource, 'figure')
    figureSource = ancestor(source, 'figure');
  end
  if ~isgraphics(figureSource, 'figure')
    return
  end
  state = get(figureSource, 'UserData');
  keyName = lower(event.Key);
  character = lower(event.Character);
  inputName = character;
  if isempty(inputName)
    inputName = keyName;
  end
  switch inputName
    case {'z', 'w', 'up'}
      state.forward = pressed;
    case {'s', 'down'}
      state.backward = pressed;
    case {'q', 'a'}
      state.strafeLeft = pressed;
    case 'd'
      state.strafeRight = pressed;
    case 'left'
      state.turnLeft = pressed;
    case 'right'
      state.turnRight = pressed;
    case 'f'
      if pressed && ~state.fullscreenKeyDown
        if strcmp(get(figureSource, 'WindowState'), 'fullscreen')
          set(figureSource, 'WindowState', 'normal');
          status = findobj(figureSource, 'Tag', 'raycaster-status');
          set(status, 'Visible', 'on');
        else
          set(figureSource, 'WindowState', 'fullscreen');
          status = findobj(figureSource, 'Tag', 'raycaster-status');
          set(status, 'Visible', 'off');
        end
      end
      state.fullscreenKeyDown = pressed;
    case {'esc', 'escape'}
      state.quit = pressed;
  end
  set(figureSource, 'UserData', state);
end
%=============================================================================
function raycasterScrollWheelEvent(source, event)
  % The scroll wheel has no role in the game, but a figure that defines a
  % WindowScrollWheelFcn owns the wheel: the figure hosts then keep their
  % built-in axes zoom off, which would otherwise stretch the first-person
  % view. Nothing to do here.
end
%=============================================================================
function state = raycasterMovePlayer(state, worldMap, deltaTime)
  turnDirection = double(state.turnRight) - double(state.turnLeft);
  state.angle = mod(state.angle + turnDirection * 1.85 * deltaTime, 2 * pi);
  forwardMotion = double(state.forward) - double(state.backward);
  sideMotion = double(state.strafeRight) - double(state.strafeLeft);
  motionLength = hypot(forwardMotion, sideMotion);
  if motionLength > 1
    forwardMotion = forwardMotion / motionLength;
    sideMotion = sideMotion / motionLength;
  end
  moveDistance = 2.65 * deltaTime;
  deltaX = moveDistance * (cos(state.angle) * forwardMotion - ...
    sin(state.angle) * sideMotion);
  deltaY = moveDistance * (sin(state.angle) * forwardMotion + ...
    cos(state.angle) * sideMotion);
  collisionRadius = 0.20;
  if raycasterCanOccupy(worldMap, state.x + deltaX, state.y, collisionRadius)
    state.x = state.x + deltaX;
  end
  if raycasterCanOccupy(worldMap, state.x, state.y + deltaY, collisionRadius)
    state.y = state.y + deltaY;
  end
  state.moving = abs(forwardMotion) + abs(sideMotion) > 0;
end
%=============================================================================
function canOccupy = raycasterCanOccupy(worldMap, x, y, radius)
  columns = floor([x - radius, x + radius]);
  rows = floor([y - radius, y + radius]);
  inside = min(columns) >= 1 && max(columns) <= size(worldMap, 2) && ...
    min(rows) >= 1 && max(rows) <= size(worldMap, 1);
  if ~inside
    canOccupy = false;
    return
  end
  canOccupy = all(all(worldMap(rows, columns) == 0));
end
%=============================================================================
function [textures, spriteColor, spriteAlpha, weaponColor, weaponAlpha] = ...
  raycasterLoadTextures()
  assetRoot = fullfile(modulepath('graphics'), 'examples', ...
    'retro_raycaster_assets');
  wallNames = {'brick.png', 'metal.png', 'stone.png', 'portal.png'};
  textures = zeros(256, 256, 3, numel(wallNames));
  for textureIndex = 1:numel(wallNames)
    wall = imread(fullfile(assetRoot, wallNames{textureIndex}));
    textures(:, :, :, textureIndex) = double(wall) / 255;
  end
  [spriteColor, ~, spriteAlpha] = imread(...
    fullfile(assetRoot, 'enemy_render.png'));
  [weaponColor, ~, weaponAlpha] = imread(...
    fullfile(assetRoot, 'weapon_render.png'));
  spriteColor = double(spriteColor) / 255;
  spriteAlpha = double(spriteAlpha) / 255;
  weaponColor = double(weaponColor) / 255;
  weaponAlpha = double(weaponAlpha) / 255;
end
%=============================================================================
function [frame, rayDistance] = raycasterRenderFrame(worldMap, textures, ...
  state, width, height, showMinimap)
  persistent cachedWidth cachedHeight cachedCameraX cachedVignette
  directionX = cos(state.angle);
  directionY = sin(state.angle);
  planeScale = tan(68 * pi / 360);
  planeX = -directionY * planeScale;
  planeY = directionX * planeScale;
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    cachedCameraX = 2 * ((1:width) - 0.5) / width - 1;
    cachedVignette = 1 - 0.18 * cachedCameraX .^ 2;
  end
  cameraX = cachedCameraX;
  frame = raycasterFloorAndCeiling(width, height, cachedVignette);
  rayX = directionX + planeX * cameraX;
  rayY = directionY + planeY * cameraX;
  [rayDistance, side, wallType, textureCoordinate] = ...
    raycasterCastRays(worldMap, state.x, state.y, rayX, rayY);
  frame = raycasterDrawWalls(frame, textures, rayDistance, side, ...
    wallType, textureCoordinate, rayX, rayY, cachedVignette);
  frame = raycasterDrawCrosshair(frame);
  if showMinimap
    frame = raycasterDrawMinimap(frame, worldMap, state);
  end
end
%=============================================================================
function frame = raycasterFloorAndCeiling(width, height, vignette)
  persistent cachedWidth cachedHeight cachedFrame
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    cachedFrame = zeros(height, width, 3);
    horizon = floor(height / 2);
    ceilingBlend = linspace(0, 1, max(1, horizon))';
    cachedFrame(1:horizon, :, 1) = ...
      repmat(0.018 + 0.025 * ceilingBlend, 1, width);
    cachedFrame(1:horizon, :, 2) = ...
      repmat(0.023 + 0.035 * ceilingBlend, 1, width);
    cachedFrame(1:horizon, :, 3) = ...
      repmat(0.045 + 0.075 * ceilingBlend, 1, width);
    floorBlend = linspace(0.25, 1, max(1, height - horizon))';
    grain = repmat(0.006 * sin((1:width) * 0.21), ...
      numel(floorBlend), 1);
    cachedFrame(horizon + 1:height, :, 1) = ...
      repmat(0.050 + 0.060 * floorBlend, 1, width) + grain;
    cachedFrame(horizon + 1:height, :, 2) = ...
      repmat(0.055 + 0.065 * floorBlend, 1, width) + 0.7 * grain;
    cachedFrame(horizon + 1:height, :, 3) = ...
      repmat(0.064 + 0.070 * floorBlend, 1, width) + 0.5 * grain;
    floorHeight = height - horizon;
    lineRows = horizon + unique(max(1, ...
      round(floorHeight * [0.10 0.20 0.34 0.52 0.74])));
    cachedFrame(lineRows, :, :) = 0.32 * cachedFrame(lineRows, :, :);
    cachedFrame = cachedFrame .* vignette;
  end
  frame = cachedFrame;
end
%=============================================================================
function [distance, side, wallType, textureCoordinate] = ...
  raycasterCastRays(worldMap, playerX, playerY, rayX, rayY)
  rayCount = numel(rayX);
  mapX = floor(playerX) * ones(1, rayCount);
  mapY = floor(playerY) * ones(1, rayCount);
  deltaX = abs(1 ./ rayX);
  deltaY = abs(1 ./ rayY);
  stepX = ones(1, rayCount);
  stepY = ones(1, rayCount);
  stepX(rayX < 0) = -1;
  stepY(rayY < 0) = -1;
  sideX = (mapX + 1 - playerX) .* deltaX;
  sideY = (mapY + 1 - playerY) .* deltaY;
  sideX(rayX < 0) = (playerX - mapX(rayX < 0)) .* deltaX(rayX < 0);
  sideY(rayY < 0) = (playerY - mapY(rayY < 0)) .* deltaY(rayY < 0);
  hit = false(1, rayCount);
  side = zeros(1, rayCount);
  wallType = ones(1, rayCount);
  lastSide = zeros(1, rayCount);
  for step = 1:sum(size(worldMap))
    moveX = sideX < sideY & ~hit;
    moveY = ~moveX & ~hit;
    sideX(moveX) = sideX(moveX) + deltaX(moveX);
    sideY(moveY) = sideY(moveY) + deltaY(moveY);
    mapX(moveX) = mapX(moveX) + stepX(moveX);
    mapY(moveY) = mapY(moveY) + stepY(moveY);
    lastSide(moveX) = 0;
    lastSide(moveY) = 1;
    mapIndex = mapY + (mapX - 1) * size(worldMap, 1);
    sampledWall = worldMap(mapIndex);
    newHit = ~hit & sampledWall ~= 0;
    side(newHit) = lastSide(newHit);
    wallType(newHit) = sampledWall(newHit);
    hit = hit | newHit;
    if all(hit)
      break
    end
  end
  distance = sideY - deltaY;
  distance(side == 0) = sideX(side == 0) - deltaX(side == 0);
  wallPosition = playerX + distance .* rayX;
  wallPosition(side == 0) = playerY + ...
    distance(side == 0) .* rayY(side == 0);
  distance = max(distance, 0.001);
  textureCoordinate = wallPosition - floor(wallPosition);
end
%=============================================================================
function frame = raycasterDrawWalls(frame, textures, distance, side, ...
  wallType, textureCoordinate, rayX, rayY, vignette)
  persistent cachedWidth cachedHeight cachedRowGrid
  height = size(frame, 1);
  width = size(frame, 2);
  textureSize = size(textures, 1);
  wallHeight = max(1, round(height ./ distance));
  firstRow = max(1, floor((height - wallHeight) / 2) + 1);
  lastRow = min(height, floor((height + wallHeight) / 2));
  textureX = min(textureSize, max(1, ...
    floor(textureCoordinate * textureSize) + 1));
  flipTexture = (side == 0 & rayX > 0) | (side == 1 & rayY < 0);
  textureX(flipTexture) = textureSize - textureX(flipTexture) + 1;
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    cachedRowGrid = repmat((1:height).', 1, width);
  end
  rowGrid = cachedRowGrid;
  wallMask = rowGrid >= firstRow & rowGrid <= lastRow;
  visibleIndices = reshape(find(wallMask), [], 1);
  visibleRows = mod(visibleIndices - 1, height) + 1;
  visibleColumns = floor((visibleIndices - 1) / height) + 1;
  visibleWallHeight = reshape(wallHeight(visibleColumns), [], 1);
  textureY = floor((visibleRows - (height - visibleWallHeight) / 2) .* ...
    textureSize ./ visibleWallHeight) + 1;
  textureY = min(textureSize, max(1, textureY));
  shade = max(0.30, exp(-0.050 * distance) .* ...
    (1 - 0.12 * side)) .* vignette;
  textureColumnOffset = (textureX - 1) * textureSize + ...
    (wallType - 1) * textureSize ^ 2 * 3;
  columnOffset = reshape(textureColumnOffset(visibleColumns), [], 1);
  textureIndex = textureY + columnOffset + (0:2) * textureSize ^ 2;
  textureValues = reshape(textures(textureIndex), [], 3);
  shadeValues = reshape(shade(visibleColumns), [], 1);
  frameIndices = visibleIndices + (0:2) * height * width;
  frame(frameIndices(:)) = reshape(textureValues .* shadeValues, [], 1);
end
%=============================================================================
function raycasterUpdateTextureOverlays(enemyHandles, weaponHandle, ...
  enemies, state, rayDistance, width, height, elapsedTime)
  persistent cachedEnemyHandles cachedEnemyRects cachedEnemyVisible
  persistent cachedWeaponHandle cachedWeaponRect
  if isempty(cachedEnemyHandles) || ~isequal(cachedEnemyHandles, enemyHandles)
    cachedEnemyHandles = enemyHandles;
    cachedEnemyRects = nan(numel(enemyHandles), 4);
    cachedEnemyVisible = false(1, numel(enemyHandles));
  end
  if isempty(cachedWeaponHandle) || ~isequal(cachedWeaponHandle, weaponHandle)
    cachedWeaponHandle = weaponHandle;
    cachedWeaponRect = nan(1, 4);
  end
  directionX = cos(state.angle);
  directionY = sin(state.angle);
  planeScale = tan(68 * pi / 360);
  planeX = -directionY * planeScale;
  planeY = directionX * planeScale;
  determinant = planeX * directionY - directionX * planeY;
  for enemyIndex = 1:numel(enemyHandles)
    offsetX = enemies(enemyIndex, 1) - state.x;
    offsetY = enemies(enemyIndex, 2) - state.y;
    transformX = (directionY * offsetX - directionX * offsetY) / determinant;
    transformY = (-planeY * offsetX + planeX * offsetY) / determinant;
    if transformY <= 0.15
      if cachedEnemyVisible(enemyIndex)
        set(enemyHandles{enemyIndex}, 'Visible', 'off');
        cachedEnemyVisible(enemyIndex) = false;
      end
      continue
    end
    screenX = round(width * (1 + transformX / transformY) / 2);
    spriteHeight = min(2 * height, ...
      max(1, abs(round(1.25 * height / transformY))));
    spriteWidth = max(1, round(2 * spriteHeight / 3));
    firstColumn = max(1, floor(screenX - spriteWidth / 2));
    lastColumn = min(width, floor(screenX + spriteWidth / 2));
    firstRow = max(1, floor((height - spriteHeight) / 2));
    lastRow = min(height, floor((height + spriteHeight) / 2));
    if firstColumn > lastColumn || firstRow > lastRow
      if cachedEnemyVisible(enemyIndex)
        set(enemyHandles{enemyIndex}, 'Visible', 'off');
        cachedEnemyVisible(enemyIndex) = false;
      end
      continue
    end
    columns = firstColumn:lastColumn;
    if ~any(transformY < rayDistance(columns))
      if cachedEnemyVisible(enemyIndex)
        set(enemyHandles{enemyIndex}, 'Visible', 'off');
        cachedEnemyVisible(enemyIndex) = false;
      end
      continue
    end
    enemyRect = [firstColumn lastColumn firstRow lastRow];
    if ~cachedEnemyVisible(enemyIndex) || ...
      any(cachedEnemyRects(enemyIndex, :) ~= enemyRect)
      set(enemyHandles{enemyIndex}, 'XData', enemyRect(1:2), ...
        'YData', enemyRect(3:4), 'Visible', 'on');
      cachedEnemyRects(enemyIndex, :) = enemyRect;
      cachedEnemyVisible(enemyIndex) = true;
    end
  end
  bob = double(state.moving) * round(2 * abs(sin(7 * elapsedTime)));
  weaponHeight = max(1, round(0.40 * height));
  weaponWidth = max(1, round(2 * weaponHeight / 3));
  firstRow = height - weaponHeight + 1 + bob;
  firstColumn = round((width - weaponWidth) / 2) + 1;
  weaponRect = [firstColumn firstColumn + weaponWidth - 1 ...
    firstRow firstRow + weaponHeight - 1];
  if any(cachedWeaponRect ~= weaponRect)
    set(weaponHandle, 'XData', weaponRect(1:2), ...
      'YData', weaponRect(3:4), 'Visible', 'on');
    cachedWeaponRect = weaponRect;
  end
end
%=============================================================================
function frame = raycasterDrawCrosshair(frame)
  height = size(frame, 1);
  width = size(frame, 2);
  centerRow = round(height / 2);
  centerColumn = round((width + 1) / 2);
  crosshairColor = reshape([0.82 1.00 0.86], 1, 1, 3);
  rows = max(1, centerRow - 4):min(height, centerRow + 4);
  columns = max(1, centerColumn - 1):min(width, centerColumn + 1);
  frame(rows, columns, :) = repmat(crosshairColor, ...
    numel(rows), numel(columns), 1);
  rows = max(1, centerRow - 1):min(height, centerRow + 1);
  columns = max(1, centerColumn - 4):min(width, centerColumn + 4);
  frame(rows, columns, :) = repmat(crosshairColor, ...
    numel(rows), numel(columns), 1);
end
%=============================================================================
function frame = raycasterDrawMinimap(frame, worldMap, state)
  persistent cachedHeight cachedWorldMap cachedScale cachedColor cachedMask
  scale = max(1, min(3, floor(size(frame, 1) / 70)));
  top = 5;
  left = 5;
  mapHeight = size(worldMap, 1) * scale;
  mapWidth = size(worldMap, 2) * scale;
  lastRow = min(size(frame, 1), top + mapHeight + 3);
  lastColumn = min(size(frame, 2), left + mapWidth + 3);
  if isempty(cachedHeight) || cachedHeight ~= size(frame, 1) || ...
    ~ isequal(cachedWorldMap, worldMap)
    cachedHeight = size(frame, 1);
    cachedWorldMap = worldMap;
    cachedScale = scale;
    regionHeight = lastRow - top + 1;
    regionWidth = lastColumn - left + 1;
    cachedColor = zeros(regionHeight, regionWidth, 3);
    cachedMask = false(regionHeight, regionWidth);
    wallColors = [0.72 0.20 0.13; 0.18 0.46 0.72; ...
      0.22 0.62 0.34; 0.92 0.52 0.10];
    for row = 1:size(worldMap, 1)
      for column = 1:size(worldMap, 2)
        wallType = worldMap(row, column);
        if wallType == 0
          continue
        end
        rows = (row - 1) * scale + (2:scale + 1);
        columns = (column - 1) * scale + (2:scale + 1);
        cachedMask(rows, columns) = true;
        cachedColor(rows, columns, :) = repmat(...
          reshape(wallColors(wallType, :), 1, 1, 3), scale, scale, 1);
      end
    end
  end
  scale = cachedScale;
  region = 0.30 * frame(top:lastRow, left:lastColumn, :);
  for channel = 1:3
    layer = region(:, :, channel);
    source = cachedColor(:, :, channel);
    layer(cachedMask) = source(cachedMask);
    region(:, :, channel) = layer;
  end
  frame(top:lastRow, left:lastColumn, :) = region;
  playerRow = top + floor(state.y) * scale;
  playerColumn = left + floor(state.x) * scale;
  rows = max(1, playerRow - 1):min(size(frame, 1), playerRow + 1);
  columns = max(1, playerColumn - 1):min(size(frame, 2), playerColumn + 1);
  frame(rows, columns, :) = repmat(reshape([1.00 0.94 0.34], 1, 1, 3), ...
    numel(rows), numel(columns), 1);
end
%=============================================================================
