%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A real-time, music-synchronized demoscene reel rendered by a pure Nelson
% software engine: copper bars, plasma, textured tunnel, 3D starfield,
% rotozoomer, flat-shaded 3D vector cube and a sine scroller, all paced to a
% procedurally generated synthwave soundtrack.
%=============================================================================
if ~exist('demoFigureVisible', 'var')
  demoFigureVisible = 'on';
end
if ~exist('demoStartFullscreen', 'var')
  demoStartFullscreen = true;
end
if ~exist('demoEnableAudio', 'var')
  demoEnableAudio = true;
end
if ~exist('demoWidth', 'var')
  demoWidth = 320;
end
if ~exist('demoHeight', 'var')
  demoHeight = 180;
end
if ~exist('demoTargetFps', 'var')
  demoTargetFps = 60;
end
if ~exist('demoMaxFrames', 'var')
  demoMaxFrames = Inf;
end
if ~exist('demoMaxSeconds', 'var')
  demoMaxSeconds = Inf;
end
if ~exist('demoErrorLog', 'var')
  demoErrorLog = '';
end
if ~exist('demoOutputImage', 'var')
  demoOutputImage = '';
end
if ~exist('demoSelfTest', 'var')
  demoSelfTest = false;
end
if ~exist('demoSelfTestImageDir', 'var')
  demoSelfTestImageDir = '';
end

if ~exist('demoBenchmark', 'var')
  demoBenchmark = '';
end

try
  if ~isempty(demoBenchmark)
    demoRunBenchmark(demoBenchmark, demoWidth, demoHeight, demoErrorLog, ...
      demoSelfTestImageDir);
  elseif demoSelfTest
    demoRunSelfTest(demoWidth, demoHeight, demoErrorLog, demoSelfTestImageDir);
  else
    demoRun(demoFigureVisible, demoStartFullscreen, demoEnableAudio, ...
      demoWidth, demoHeight, demoTargetFps, demoMaxFrames, demoMaxSeconds, ...
      demoOutputImage);
  end
catch demoError
  if ~isempty(demoErrorLog)
    fid = fopen(demoErrorLog, 'wt');
    if fid ~= -1
      fprintf(fid, 'ERROR: %s\n', demoError.message);
      if isfield(demoError, 'stack') && ~isempty(demoError.stack)
        for k = 1:numel(demoError.stack)
          fprintf(fid, '  at %s line %d\n', demoError.stack(k).name, ...
            demoError.stack(k).line);
        end
      end
      fclose(fid);
    end
  end
  rethrow(demoError);
end
%=============================================================================
function demoRunBenchmark(sceneName, width, height, errorLog, imageDir)
  musicTimer = tic();
  music = demoBuildMusic();
  musicElapsed = toc(musicTimer);
  frames = 120;
  timelineTimer = tic();
  scenes = demoBuildTimeline(music);
  timelineElapsed = toc(timelineTimer);
  sceneT0 = 0;
  for k = 1:numel(scenes)
    if strcmp(scenes(k).name, sceneName)
      sceneT0 = scenes(k).t0;
    end
  end
  setupTimer = tic();
  frame = zeros(height, width, 3);
  setupElapsed = toc(setupTimer);
  sceneElapsed = 0;
  postElapsed = 0;
  timer = tic();
  for i = 1:frames
    t = sceneT0 + i / 60;
    beat = demoBeatInfo(t, music);
    phaseTimer = tic();
    frame = demoRenderScene(sceneName, i / 60, t, beat, width, height);
    sceneElapsed = sceneElapsed + toc(phaseTimer);
    applyOverlay = ~any(strcmp(sceneName, {'logo', 'earth', 'another'}));
    phaseTimer = tic();
    frame = demoPostProcess(frame, t, beat, width, height, music, ...
      applyOverlay, 1.0);
    postElapsed = postElapsed + toc(phaseTimer);
  end
  elapsed = toc(timer);
  fps = frames / elapsed;
  report = sprintf(['benchmark scene=%s %dx%d: %d frames in %.3fs ', ...
    '=> %.1f FPS (%.2f ms/frame)\n'], sceneName, width, height, frames, ...
    elapsed, fps, 1000 * elapsed / frames);
  report = [report, sprintf(['benchmark prep scene=%s music %.3fs ', ...
    'timeline %.3fs setup %.3fs render %.3fs total %.3fs\n'], ...
    sceneName, musicElapsed, timelineElapsed, setupElapsed, elapsed, ...
    musicElapsed + timelineElapsed + setupElapsed + elapsed)];
  report = [report, sprintf(['benchmark render scene=%s effect %.3fs ', ...
    'post %.3fs total %.3fs\n'], sceneName, sceneElapsed, postElapsed, ...
    elapsed)];
  if isfield(music, 'profile')
    p = music.profile;
    report = [report, sprintf(['benchmark music scene=%s drums %.3fs ', ...
      'bass %.3fs arp %.3fs lead %.3fs pad %.3fs post %.3fs ', ...
      'ambient %.3fs total %.3fs\n'], sceneName, p.drums, p.bass, ...
      p.arp, p.lead, p.pad, p.post, p.ambient, p.total)];
  end
  if ~isempty(imageDir)
    imwrite(min(1, max(0, frame)), [imageDir, '/bench_', sceneName, '.png']);
  end
  if ~isempty(errorLog)
    fid = fopen(errorLog, 'wt');
    if fid ~= -1
      fprintf(fid, '%s', report);
      fclose(fid);
    end
  end
end
%=============================================================================
function demoRunSelfTest(width, height, errorLog, imageDir)
  music = demoBuildMusic();
  scenes = demoBuildTimeline(music);
  report = sprintf('music: %d samples, %.2fs, peak=%.3f\n', ...
    size(music.samples, 1), music.duration, max(abs(music.samples(:))));
  for k = 1:numel(scenes)
    scene = scenes(k);
    midT = 0.5 * (scene.t0 + scene.t1);
    beat = demoBeatInfo(midT, music);
    frame = demoRenderScene(scene.name, midT - scene.t0, midT, beat, ...
      width, height);
    isEon = any(strcmp(scene.name, {'logo', 'earth', 'another'}));
    frame = demoPostProcess(frame, midT, beat, width, height, music, ...
      ~ isEon, 1.0);
    report = [report, sprintf('scene %-7s frame %dx%dx%d range[%.3f %.3f]\n', ...
      scene.name, size(frame, 1), size(frame, 2), size(frame, 3), ...
      min(frame(:)), max(frame(:)))];
    if ~isempty(imageDir)
      imwrite(frame, [imageDir, '/scene_', scene.name, '.png']);
    end
  end
  if ~isempty(imageDir)
    endT = music.duration - 2.3;
    endScene = scenes(end);
    beat = demoBeatInfo(endT, music);
    frame = demoRenderScene(endScene.name, endT - endScene.t0, endT, beat, ...
      width, height);
    endFade = min(1, max(0, music.duration - endT) / 3);
    frame = demoPostProcess(frame, endT, beat, width, height, music, ...
      false, endFade);
    imwrite(frame, [imageDir, '/scene_endcard.png']);
  end
  report = [report, sprintf('SELFTEST OK\n')];
  if ~isempty(errorLog)
    fid = fopen(errorLog, 'wt');
    if fid ~= -1
      fprintf(fid, '%s', report);
      fclose(fid);
    end
  end
end
%=============================================================================
function demoRun(figureVisible, startFullscreen, enableAudio, width, ...
  height, targetFps, maxFrames, maxSeconds, outputImage)
  fprintf(['Preparing demoscene: generating procedural music and ', ...
    'timeline, please wait...\n']);
  drawnow();
  music = demoBuildMusic();
  fs = music.sampleRate;
  scenes = demoBuildTimeline(music);
  totalDuration = music.duration;
  fprintf('Starting demoscene.\n');
  drawnow();

  screenSize = get(0, 'ScreenSize');
  figureWidth = min(1120, max(720, screenSize(3) - 80));
  figureHeight = round(figureWidth * height / width);
  figureLeft = screenSize(1) + max(20, (screenSize(3) - figureWidth) / 2);
  figureBottom = screenSize(2) + max(35, (screenSize(4) - figureHeight) / 2);
  demoState = struct('quit', false, 'fullscreenKeyDown', false);
  figureHandle = figure('Name', 'Nelson demoscene', 'NumberTitle', 'off', ...
    'Color', [0 0 0], 'MenuBar', 'none', 'ToolBar', 'none', 'Resize', 'on', ...
    'Visible', figureVisible, ...
    'Position', [figureLeft figureBottom figureWidth figureHeight], ...
    'WindowKeyPressFcn', @(source, event) demoKeyEvent(source, event), ...
    'UserData', demoState);
  axesHandle = axes('Parent', figureHandle, 'Position', [0 0 1 1]);
  frame = zeros(height, width, 3);
  imageHandle = image('Parent', axesHandle, 'CData', frame, ...
    'XData', [1 width], 'YData', [1 height]);
  set(axesHandle, 'XLim', [0.5 width + 0.5], 'YLim', [0.5 height + 0.5], ...
    'XLimMode', 'manual', 'YLimMode', 'manual', 'YDir', 'reverse');
  axis(axesHandle, 'off');

  if startFullscreen
    set(figureHandle, 'WindowState', 'fullscreen');
  end

  player = [];
  audioActive = false;
  if enableAudio
    try
      player = audioplayer(music.samples, fs);
      play(player);
      audioActive = true;
    catch
      player = [];
      audioActive = false;
    end
  end

  targetFrameDuration = 1 / targetFps;
  wallClock = tic();
  nextFrameTime = targetFrameDuration;
  frameIndex = 0;

  while isgraphics(figureHandle) && isgraphics(imageHandle) && ...
    frameIndex < maxFrames
    state = get(figureHandle, 'UserData');
    if state.quit
      break
    end
    wallTime = toc(wallClock);
    if audioActive && isvalid(player) && strcmp(player.Running, 'on')
      musicTime = player.CurrentSample / fs;
    else
      musicTime = wallTime;
    end
    if musicTime >= totalDuration || wallTime >= maxSeconds
      break
    end

    beat = demoBeatInfo(musicTime, music);
    frame = demoRenderTimeline(scenes, musicTime, beat, width, height);
    applyOverlay = musicTime < music.eonStart;
    fadeLevel = min(1, musicTime / 2.5) * ...
      min(1, max(0, totalDuration - musicTime) / 3);
    frame = demoPostProcess(frame, musicTime, beat, width, height, music, ...
      applyOverlay, fadeLevel);
    set(imageHandle, 'CData', frame);

    frameIndex = frameIndex + 1;
    drawnow();

    remainingTime = nextFrameTime - toc(wallClock);
    if remainingTime > 0.002
      pause(remainingTime - 0.001);
    end
    while toc(wallClock) < nextFrameTime
    end
    nextFrameTime = nextFrameTime + targetFrameDuration;
    if nextFrameTime < toc(wallClock)
      nextFrameTime = toc(wallClock) + targetFrameDuration;
    end
  end

  if audioActive && isvalid(player)
    stop(player);
  end
  if ~isempty(outputImage) && isgraphics(figureHandle)
    drawnow();
    saveas(figureHandle, outputImage);
  end
  if isgraphics(figureHandle)
    close(figureHandle);
  end
end
%=============================================================================
function demoKeyEvent(source, event)
  figureSource = source;
  if ~isgraphics(figureSource, 'figure')
    figureSource = ancestor(source, 'figure');
  end
  if ~isgraphics(figureSource, 'figure')
    return
  end
  state = get(figureSource, 'UserData');
  keyName = lower(event.Key);
  character = '';
  if isfield(event, 'Character')
    character = lower(event.Character);
  end
  inputName = character;
  if isempty(inputName)
    inputName = keyName;
  end
  switch inputName
    case {'esc', 'escape', 'q'}
      state.quit = true;
    case 'f'
      if strcmp(get(figureSource, 'WindowState'), 'fullscreen')
        set(figureSource, 'WindowState', 'normal');
      else
        set(figureSource, 'WindowState', 'fullscreen');
      end
  end
  set(figureSource, 'UserData', state);
end
%=============================================================================
% ---- Timeline -------------------------------------------------------------
%=============================================================================
function scenes = demoBuildTimeline(music)
  bar = music.barDuration;
  es = music.eonStart;
  d = music.duration;
  boundaries = {...
    'intro', 0 * bar, 2 * bar; ...
    'plasma', 2 * bar, 5 * bar; ...
    'tunnel', 5 * bar, 8 * bar; ...
    'stars', 8 * bar, 11 * bar; ...
    'roto', 11 * bar, 14 * bar; ...
    'cube', 14 * bar, 18 * bar; ...
    'outro', 18 * bar, es; ...
    'logo', es, es + 10; ...
    'earth', es + 10, es + 22; ...
    'another', es + 22, d};
  scenes = struct('name', boundaries(:, 1), 't0', boundaries(:, 2), ...
    't1', boundaries(:, 3));
end
%=============================================================================
function frame = demoRenderTimeline(scenes, t, beat, width, height)
  crossfade = 0.45;
  activeIndex = numel(scenes);
  for k = 1:numel(scenes)
    if t >= scenes(k).t0 && t < scenes(k).t1
      activeIndex = k;
      break
    end
  end
  scene = scenes(activeIndex);
  localT = t - scene.t0;
  frame = demoRenderScene(scene.name, localT, t, beat, width, height);
  if activeIndex < numel(scenes)
    nextScene = scenes(activeIndex + 1);
    timeToEnd = scene.t1 - t;
    if timeToEnd < crossfade
      blend = 0.5 * (1 - cos(pi * (crossfade - timeToEnd) / crossfade));
      nextLocalT = t - nextScene.t0;
      nextFrame = demoRenderScene(nextScene.name, nextLocalT, t, beat, ...
        width, height);
      frame = (1 - blend) * frame + blend * nextFrame;
    end
  end
end
%=============================================================================
function frame = demoRenderScene(name, localT, t, beat, width, height)
  switch name
    case 'intro'
      frame = demoFxIntro(localT, t, beat, width, height);
    case 'plasma'
      frame = demoFxPlasma(localT, t, beat, width, height);
    case 'tunnel'
      frame = demoFxTunnel(localT, t, beat, width, height);
    case 'stars'
      frame = demoFxStars(localT, t, beat, width, height);
    case 'roto'
      frame = demoFxRoto(localT, t, beat, width, height);
    case 'cube'
      frame = demoFxCube(localT, t, beat, width, height);
    case 'outro'
      frame = demoFxOutro(localT, t, beat, width, height);
    case 'logo'
      frame = eonSceneLogo(localT, t, beat, width, height);
    case 'earth'
      frame = eonSceneEarth(localT, t, beat, width, height);
    case 'another'
      frame = eonSceneAnother(localT, t, beat, width, height);
    otherwise
      frame = zeros(height, width, 3);
  end
end
%=============================================================================
% ---- Effects --------------------------------------------------------------
%=============================================================================
function [X, Y] = demoGrid(width, height)
  persistent cachedWidth cachedHeight cachedX cachedY
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    aspect = width / height;
    [cachedX, cachedY] = meshgrid(...
      linspace(-aspect, aspect, width), linspace(-1, 1, height));
  end
  X = cachedX;
  Y = cachedY;
end
%=============================================================================
function rgb = demoCosPalette(phase, a, b, c, d)
  ar = reshape(a, 1, 1, 3);
  br = reshape(b, 1, 1, 3);
  cr = reshape(c, 1, 1, 3);
  dr = reshape(d, 1, 1, 3);
  rgb = ar + br .* cos(2 * pi * (cr .* phase + dr));
  rgb = min(1, max(0, rgb));
end
%=============================================================================
function frame = demoFxPlasma(localT, t, beat, width, height)
  [X, Y] = demoGrid(width, height);
  pulse = 0.20 * beat.kick;
  v = sin(X * 3.2 + t * 1.1) + ...
    sin(Y * 3.7 - t * 0.9) + ...
    sin((X + Y) * 2.6 + t * 1.3) + ...
    sin(hypot(X, Y) * (5.0 + pulse * 18) - t * 2.1);
  phase = v * 0.12 + t * 0.03;
  frame = demoCosPalette(phase, [0.52 0.42 0.58], [0.45 0.38 0.42], ...
    [1.0 1.0 1.0], [0.00 0.20 0.55]);
  frame = min(1, frame * (0.78 + 0.32 * beat.kick));
end
%=============================================================================
function frame = demoFxIntro(localT, t, beat, width, height)
  frame = demoCopperBars(t, beat, width, height);
  plasma = demoFxPlasma(localT, t, beat, width, height);
  fadeIn = min(1, localT / 1.2);
  frame = (1 - 0.5 * fadeIn) * frame + 0.5 * fadeIn * plasma;
  titleAlpha = min(1, max(0, (localT - 0.4) / 1.0)) * ...
    min(1, max(0, (3.6 - localT) / 0.8));
  frame = demoStampText(frame, 'NELSON', 5, titleAlpha, ...
    [1.0 0.95 0.55], round(height * 0.30), width, height);
  frame = demoStampText(frame, 'DEMOSCENE', 3, titleAlpha, ...
    [0.65 0.95 1.0], round(height * 0.30) + 44, width, height);
end
%=============================================================================
function frame = demoCopperBars(t, beat, width, height)
  rows = (1:height)';
  base = demoCosPalette(rows / height * 1.5 + t * 0.15, ...
    [0.20 0.16 0.28], [0.20 0.16 0.28], [1 1 1], [0.0 0.33 0.66]);
  centers = height * (0.5 + 0.42 * sin(t * [1.3 0.7 1.9] + [0 2 4]));
  bars = zeros(height, 1);
  colors = [1.0 0.35 0.55; 0.35 0.75 1.0; 0.85 0.95 0.45];
  barLayer = zeros(height, 1, 3);
  for k = 1:numel(centers)
    intensity = exp(-((rows - centers(k)) .^ 2) / (2 * 7 ^ 2));
    for ch = 1:3
      barLayer(:, 1, ch) = barLayer(:, 1, ch) + intensity * colors(k, ch);
    end
  end
  frame = repmat(min(1, base + barLayer), 1, width, 1);
end
%=============================================================================
function frame = demoFxTunnel(localT, t, beat, width, height)
  persistent cachedWidth cachedHeight angleMap depthMap shadeMap
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    [X, Y] = meshgrid(linspace(-1, 1, width) * (width / height), ...
      linspace(-1, 1, height));
    radius = hypot(X, Y) + 1e-4;
    angleMap = atan2(Y, X) / (2 * pi);
    depthMap = 0.35 ./ radius;
    shadeMap = min(1, radius * 1.15);
  end
  speed = 0.9 + 0.55 * beat.kick;
  u = angleMap * 6 + t * 0.35;
  v = depthMap + t * speed;
  checker = 0.5 + 0.5 * sign(sin(u * pi * 2) .* sin(v * pi * 2));
  glow = 0.5 + 0.5 * sin(v * 3.1 + t);
  phase = v * 0.5 + u * 0.15 + t * 0.05;
  color = demoCosPalette(phase, [0.5 0.45 0.55], [0.5 0.45 0.5], ...
    [1 1 1], [0.10 0.35 0.62]);
  texture = 0.35 + 0.65 * checker;
  frame = color .* texture .* (0.35 + 0.75 * glow);
  frame = frame .* shadeMap;
  centerGlow = (1 - shadeMap) .^ 2 * (0.6 + 0.4 * beat.kick);
  frame = frame + repmat(centerGlow, 1, 1, 3) .* ...
    reshape([0.9 0.95 1.0], 1, 1, 3);
  frame = min(1, frame * (0.9 + 0.3 * beat.kick));
end
%=============================================================================
function frame = demoFxStars(localT, t, beat, width, height)
  persistent starX starY starZ starCount lastLocalT
  count = 460;
  reset = isempty(starCount) || starCount ~= count || ...
    isempty(lastLocalT) || localT < lastLocalT;
  if reset
    starCount = count;
    starX = (demoPseudoRandom(count, 1) - 0.5) * 4;
    starY = (demoPseudoRandom(count, 2) - 0.5) * 4;
    starZ = demoPseudoRandom(count, 3) * 3 + 0.2;
  end
  dt = 1 / 60;
  if ~isempty(lastLocalT) && localT >= lastLocalT
    dt = min(0.05, localT - lastLocalT);
  end
  lastLocalT = localT;
  speed = 1.7 + 2.2 * beat.kick;
  prevZ = starZ;
  starZ = starZ - speed * dt;
  wrapped = starZ < 0.1;
  nWrap = sum(wrapped);
  if nWrap > 0
    starZ(wrapped) = 3.2;
    prevZ(wrapped) = 3.2;
    starX(wrapped) = (demoPseudoRandom(nWrap, 7 + localT) - 0.5) * 4;
    starY(wrapped) = (demoPseudoRandom(nWrap, 9 + localT) - 0.5) * 4;
  end
  focal = height * 0.9;
  cx = width / 2;
  cy = height / 2;
  brightness = min(1, 0.30 + 0.9 ./ starZ);
  tint = demoCosPalette(starZ * 0.2, [0.72 0.72 0.85], [0.28 0.28 0.18], ...
    [1 1 1], [0.0 0.2 0.4]);
  streakLen = 2.2;
  layers = zeros(height, width, 3);
  steps = 5;
  for p = 0:steps - 1
    frac = p / (steps - 1);
    zSample = starZ + (prevZ - starZ) * streakLen * frac;
    sx = round(cx + focal * starX ./ zSample);
    sy = round(cy + focal * starY ./ zSample);
    vis = sx >= 1 & sx <= width & sy >= 1 & sy <= height & zSample > 0.05;
    if ~any(vis)
      continue
    end
    weight = brightness(vis) * (1 - 0.7 * frac);
    idx = sy(vis) + (sx(vis) - 1) * height;
    tv = tint(vis, 1, :);
    for ch = 1:3
      layer = layers(:, :, ch);
      contribution = weight .* reshape(tv(:, 1, ch), [], 1);
      layer(idx) = max(layer(idx), contribution);
      layers(:, :, ch) = layer;
    end
  end
  frame = min(1, layers);
end
%=============================================================================
function frame = demoFxRoto(localT, t, beat, width, height)
  [X, Y] = demoGrid(width, height);
  angle = t * 0.6;
  zoom = 1.6 + 0.8 * sin(t * 0.7) + 0.55 * beat.kick;
  u = (cos(angle) * X - sin(angle) * Y) * zoom + t * 0.3;
  v = (sin(angle) * X + cos(angle) * Y) * zoom + t * 0.15;
  checker = sign(sin(u * pi * 3) .* sin(v * pi * 3));
  diamond = 0.5 + 0.5 * sin((abs(u) + abs(v)) * 4 - t * 2);
  phase = u * 0.1 + v * 0.1 + t * 0.04;
  color = demoCosPalette(phase, [0.5 0.4 0.5], [0.5 0.4 0.5], ...
    [1 1 1], [0.05 0.30 0.60]);
  texture = 0.45 + 0.35 * checker + 0.2 * diamond;
  frame = color .* texture;
  frame = min(1, frame * (0.9 + 0.2 * beat.kick));
end
%=============================================================================
function frame = demoFxCube(localT, t, beat, width, height)
  frame = demoStarBackdrop(width, height, beat);
  [verts, faces, faceSeed, faceNormals] = demoBuildIcosahedron();
  ax = t * 0.7;
  ay = t * 0.9;
  az = t * 0.35;
  Rx = [1 0 0; 0 cos(ax) -sin(ax); 0 sin(ax) cos(ax)];
  Ry = [cos(ay) 0 sin(ay); 0 1 0; -sin(ay) 0 cos(ay)];
  Rz = [cos(az) -sin(az) 0; sin(az) cos(az) 0; 0 0 1];
  rotation = Rz * Ry * Rx;
  scale = 1 + 0.16 * beat.kick;
  rotated = (rotation * (verts' * scale))';
  rotatedNormals = (rotation * faceNormals')';
  cameraDist = 4.6;
  focal = height * 1.35;
  invDepth = 1 ./ (cameraDist - rotated(:, 3));
  projX = width / 2 + focal * rotated(:, 1) .* invDepth;
  projY = height / 2 + focal * rotated(:, 2) .* invDepth;
  depthCam = cameraDist - rotated(:, 3);
  lightDir = [0.35 0.55 0.75];
  lightDir = lightDir / norm(lightDir);
  viewDir = [0 0 1];
  zbuf = inf(height, width);
  ambient = 0.14;
  hueBase = t * 0.15;
  hw = height * width;
  nFaces = size(faces, 1);
  for f = 1:nFaces
    normal = rotatedNormals(f, :);
    if normal(3) <= 0.02
      continue
    end
    idx = faces(f, :);
    px = projX(idx);
    py = projY(idx);
    minX = max(1, floor(min(px)));
    maxX = min(width, ceil(max(px)));
    minY = max(1, floor(min(py)));
    maxY = min(height, ceil(max(py)));
    if minX > maxX || minY > maxY
      continue
    end
    x1 = px(1); y1 = py(1);
    x2 = px(2); y2 = py(2);
    x3 = px(3); y3 = py(3);
    denom = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3);
    if abs(denom) < 1e-9
      continue
    end
    diffuse = max(0, dot(normal, lightDir));
    reflected = 2 * dot(normal, lightDir) * normal - lightDir;
    specular = max(0, dot(reflected, viewDir)) ^ 18;
    base = demoHueColor(hueBase + faceSeed(f));
    shade = ambient + 0.85 * diffuse;
    color = min(1, base * shade + 0.7 * specular);
    dep = depthCam(idx);
    [gx, gy] = meshgrid(minX:maxX, minY:maxY);
    aBary = ((y2 - y3) .* (gx - x3) + (x3 - x2) .* (gy - y3)) / denom;
    bBary = ((y3 - y1) .* (gx - x3) + (x1 - x3) .* (gy - y3)) / denom;
    cBary = 1 - aBary - bBary;
    inside = aBary >= -0.002 & bBary >= -0.002 & cBary >= -0.002;
    if ~any(inside(:))
      continue
    end
    pixelDepth = aBary * dep(1) + bBary * dep(2) + cBary * dep(3);
    lin = gy(inside) + (gx(inside) - 1) * height;
    pd = pixelDepth(inside);
    nearer = pd < zbuf(lin);
    lin = lin(nearer);
    if isempty(lin)
      continue
    end
    zbuf(lin) = pd(nearer);
    frame(lin) = color(1);
    frame(lin + hw) = color(2);
    frame(lin + 2 * hw) = color(3);
  end
end
%=============================================================================
function color = demoHueColor(h)
  h = mod(h, 1);
  color = 0.55 + 0.45 * cos(2 * pi * (h + [0.0 0.33 0.67]));
end
%=============================================================================
function [verts, faces, faceSeed, faceNormals] = demoBuildIcosahedron()
  persistent cachedVerts cachedFaces cachedSeed cachedNormals
  if ~isempty(cachedVerts)
    verts = cachedVerts;
    faces = cachedFaces;
    faceSeed = cachedSeed;
    faceNormals = cachedNormals;
    return
  end
  phi = (1 + sqrt(5)) / 2;
  verts = [...
    -1 phi 0; 1 phi 0; -1 -phi 0; 1 -phi 0; ...
    0 -1 phi; 0 1 phi; 0 -1 -phi; 0 1 -phi; ...
    phi 0 -1; phi 0 1; -phi 0 -1; -phi 0 1];
  for k = 1:size(verts, 1)
    verts(k, :) = verts(k, :) / norm(verts(k, :)) * 1.45;
  end
  faces = [...
    1 12 6; 1 6 2; 1 2 8; 1 8 11; 1 11 12; ...
    2 6 10; 6 12 5; 12 11 3; 11 8 7; 8 2 9; ...
    4 10 5; 4 5 3; 4 3 7; 4 7 9; 4 9 10; ...
    5 10 6; 3 5 12; 7 3 11; 9 7 8; 10 9 2];
  nFaces = size(faces, 1);
  faceSeed = ((0:nFaces - 1)') / nFaces;
  faceNormals = zeros(nFaces, 3);
  for f = 1:nFaces
    p = verts(faces(f, :), :);
    n = cross(p(2, :) - p(1, :), p(3, :) - p(1, :));
    center = mean(p, 1);
    if dot(n, center) < 0
      n = -n;
      faces(f, :) = faces(f, [1 3 2]);
    end
    faceNormals(f, :) = n / (norm(n) + 1e-9);
  end
  cachedVerts = verts;
  cachedFaces = faces;
  cachedSeed = faceSeed;
  cachedNormals = faceNormals;
end
%=============================================================================
function frame = demoStarBackdrop(width, height, beat)
  persistent cached cachedWidth cachedHeight
  if isempty(cached) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    [~, Y] = meshgrid(linspace(-1, 1, width), linspace(-1, 1, height));
    grad = 0.03 + 0.05 * (1 - (Y + 1) / 2);
    base = zeros(height, width, 3);
    base(:, :, 1) = grad * 0.45;
    base(:, :, 2) = grad * 0.55;
    base(:, :, 3) = grad * 1.0;
    starCount = 180;
    sx = 1 + floor(demoPseudoRandom(starCount, 11) * (width - 1));
    sy = 1 + floor(demoPseudoRandom(starCount, 13) * (height - 1));
    bright = 0.35 + 0.6 * demoPseudoRandom(starCount, 5);
    idx = sy + (sx - 1) * height;
    hw = height * width;
    base(idx) = max(base(idx), bright);
    base(idx + hw) = max(base(idx + hw), bright);
    base(idx + 2 * hw) = max(base(idx + 2 * hw), bright);
    cached = base;
  end
  frame = cached * (0.9 + 0.18 * beat.kick);
end
%=============================================================================
function frame = demoFxOutro(localT, t, beat, width, height)
  persistent spotBase cachedWidth cachedHeight
  if isempty(spotBase) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    [X, Y] = demoGrid(width, height);
    spotBase = exp(-(X .^ 2 + Y .^ 2) * 1.6);
  end
  frame = demoStarBackdrop(width, height, beat) * 0.85;
  spot = spotBase * (0.40 + 0.55 * beat.bar);
  frame(:, :, 1) = frame(:, :, 1) + spot * 0.55;
  frame(:, :, 2) = frame(:, :, 2) + spot * 0.80;
  frame(:, :, 3) = frame(:, :, 3) + spot;
  frame = min(1, frame);
  titleAlpha = min(1, localT / 1.2);
  frame = demoStampText(frame, 'PURE NELSON', 4, titleAlpha, ...
    [1.0 0.96 0.6], round(height * 0.28), width, height);
  frame = demoStampText(frame, 'MATRIX POWER', 3, titleAlpha, ...
    [0.7 0.95 1.0], round(height * 0.28) + 52, width, height);
end
%=============================================================================
% ---- Post-processing (scroller, scanlines, vignette, flash) ---------------
%=============================================================================
function frame = demoPostProcess(frame, t, beat, width, height, music, ...
  applyOverlay, fade)
  if applyOverlay
    frame = demoDrawScroller(frame, t, width, height);
  end
  frame = demoApplyScanlinesVignette(frame, width, height);
  frame = demoFilmGrain(frame, t, width, height);
  if applyOverlay
    frame = min(1, frame + 0.10 * beat.bar);
  end
  frame(:, :, 1) = min(1, frame(:, :, 1) * 1.05 + 0.008);
  frame(:, :, 2) = min(1, frame(:, :, 2) * 1.01 + 0.004);
  frame(:, :, 3) = min(1, frame(:, :, 3) * 1.00 + 0.012);
  endGap = music.duration - t;
  if endGap < 5
    endAlpha = min(1, (5 - endGap) / 2);
    frame = demoStampText(frame, 'TO BE CONTINUED', 3, endAlpha, ...
      [0.95 0.90 0.70], round(height * 0.44), width, height);
  end
  frame = frame * fade;
  barH = round(height * 0.11);
  frame(1:barH, :, :) = 0;
  frame(height - barH + 1:height, :, :) = 0;
end
%=============================================================================
function frame = demoFilmGrain(frame, t, width, height)
  persistent cachedWidth cachedHeight grainTex grainTile
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    idx = (1:height * width)';
    s = sin(idx * 12.9898 + idx * 0.017) * 43758.5453;
    grainTex = reshape((s - floor(s)) * 2 - 1, height, width);
    grainTile = [grainTex grainTex; grainTex grainTex];
  end
  rowShift = mod(round(t * 163), height);
  colShift = mod(round(t * 97), width);
  g = grainTile(rowShift + 1:rowShift + height, ...
    colShift + 1:colShift + width);
  amp = 0.022;
  frame(:, :, 1) = min(1, max(0, frame(:, :, 1) + g * amp));
  frame(:, :, 2) = min(1, max(0, frame(:, :, 2) + g * amp));
  frame(:, :, 3) = min(1, max(0, frame(:, :, 3) + g * amp));
end
%=============================================================================
function frame = demoApplyScanlinesVignette(frame, width, height)
  persistent cachedWidth cachedHeight modulation
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    scan = 0.92 + 0.08 * (mod((1:height)', 2) == 0);
    [X, Y] = meshgrid(linspace(-1, 1, width), linspace(-1, 1, height));
    vignette = 1 - 0.28 * (X .^ 2 + Y .^ 2);
    vignette = max(0.45, vignette);
    modulation = repmat(scan, 1, width) .* vignette;
  end
  frame = frame .* modulation;
end
%=============================================================================
function frame = demoDrawScroller(frame, t, width, height)
  persistent cachedWidth textBmp textWidth glyphHeight
  message = ['   GREETINGS FROM NELSON   ' ...
    'A PURE MATRIX DEMO   ' ...
    'PLASMA  TUNNEL  STARFIELD  ROTOZOOM  3D VECTORS   ' ...
    'NO GPU  JUST NELSON   ...   '];
  scale = 3;
  if isempty(cachedWidth) || cachedWidth ~= width || isempty(textBmp)
    cachedWidth = width;
    [textBmp, textWidth, glyphHeight] = demoBuildTextBitmap(message, scale);
  end
  bandHeight = glyphHeight + 2 * scale;
  bandTop = height - round(height * 0.11) - bandHeight - 2 * scale;
  if bandTop < 1
    bandTop = 1;
  end
  speed = 90;
  offset = mod(round(t * speed), textWidth);
  cols = mod((0:width - 1) + offset, textWidth) + 1;
  sampled = textBmp(:, cols);
  waveAmp = 3 * scale;
  waveShift = round(waveAmp * sin(((0:width - 1) + offset) * 0.03 + t * 2));
  hw = height * width;
  rowGrid = repmat((1:glyphHeight)', 1, width) + ...
    repmat(bandTop + waveShift, glyphHeight, 1);
  colGrid = repmat(1:width, glyphHeight, 1);
  lit = sampled > 0;
  color = [0.95 1.0 0.75];
  shadow = [0.05 0.10 0.05];
  shadowMask = lit & (rowGrid + 1 >= 1) & (rowGrid + 1 <= height);
  if any(shadowMask(:))
    lin = (rowGrid(shadowMask) + 1) + (colGrid(shadowMask) - 1) * height;
    frame(lin) = shadow(1);
    frame(lin + hw) = shadow(2);
    frame(lin + 2 * hw) = shadow(3);
  end
  mainMask = lit & (rowGrid >= 1) & (rowGrid <= height);
  if any(mainMask(:))
    lin = rowGrid(mainMask) + (colGrid(mainMask) - 1) * height;
    frame(lin) = color(1);
    frame(lin + hw) = color(2);
    frame(lin + 2 * hw) = color(3);
  end
end
%=============================================================================
function frame = demoStampText(frame, text, scale, alpha, color, topRow, ...
  width, height)
  if alpha <= 0
    return
  end
  [bmp, textWidth, glyphHeight] = demoBuildTextBitmap(text, scale);
  leftCol = round((width - textWidth) / 2);
  hw = height * width;
  rowGrid = repmat((topRow + (1:glyphHeight))', 1, textWidth);
  colGrid = repmat(leftCol + (1:textWidth), glyphHeight, 1);
  mask = bmp > 0 & rowGrid >= 1 & rowGrid <= height & ...
    colGrid >= 1 & colGrid <= width;
  if ~any(mask(:))
    return
  end
  lin = rowGrid(mask) + (colGrid(mask) - 1) * height;
  for ch = 1:3
    base = frame(lin + (ch - 1) * hw);
    frame(lin + (ch - 1) * hw) = (1 - alpha) * base + alpha * color(ch);
  end
end
%=============================================================================
function [bmp, textWidth, glyphHeight] = demoBuildTextBitmap(text, scale)
  persistent cachedKeys cachedBmps cachedWidths cachedHeights
  key = [text, '|', int2str(scale)];
  if ~isempty(cachedKeys)
    for cacheIndex = 1:numel(cachedKeys)
      if strcmp(cachedKeys{cacheIndex}, key)
        bmp = cachedBmps{cacheIndex};
        textWidth = cachedWidths(cacheIndex);
        glyphHeight = cachedHeights(cacheIndex);
        return
      end
    end
  end
  [chars, glyphs] = demoFontData();
  glyphRows = 7;
  glyphCols = 5;
  advance = glyphCols + 1;
  n = numel(text);
  smallWidth = n * advance;
  small = zeros(glyphRows, smallWidth);
  for k = 1:n
    ch = upper(text(k));
    pos = strfind(chars, ch);
    if isempty(pos)
      continue
    end
    glyph = glyphs{pos(1)};
    colStart = (k - 1) * advance + 1;
    small(:, colStart:colStart + glyphCols - 1) = glyph;
  end
  bmp = kron(small, ones(scale));
  glyphHeight = glyphRows * scale;
  textWidth = smallWidth * scale;
  if isempty(cachedKeys)
    cachedKeys = {};
    cachedBmps = {};
    cachedWidths = [];
    cachedHeights = [];
  end
  cachedKeys{end + 1} = key;
  cachedBmps{end + 1} = bmp;
  cachedWidths(end + 1) = textWidth;
  cachedHeights(end + 1) = glyphHeight;
end
%=============================================================================
% ---- Beat / timing --------------------------------------------------------
%=============================================================================
function beat = demoBeatInfo(t, music)
  beatPhase = mod(t, music.beatDuration) / music.beatDuration;
  barPhase = mod(t, music.barDuration) / music.barDuration;
  beat = struct();
  beat.kick = exp(-6 * beatPhase);
  beat.bar = exp(-5 * barPhase);
  beat.phase = beatPhase;
end
%=============================================================================
% ---- Deterministic pseudo-random -----------------------------------------
%=============================================================================
function values = demoPseudoRandom(count, seed)
  idx = (1:count)';
  s = sin(idx * 12.9898 + seed * 78.233) * 43758.5453;
  values = s - floor(s);
end
%=============================================================================
% ---- Music (procedural synthwave) -----------------------------------------
%=============================================================================
function music = demoBuildMusic()
  fs = 44100;
  bpm = 120;
  beatDuration = 60 / bpm;
  barDuration = 4 * beatDuration;
  bars = 20;
  duration = bars * barDuration;
  totalSamples = round(duration * fs) + fs;
  drumsL = zeros(totalSamples, 1);
  drumsR = zeros(totalSamples, 1);
  toneL = zeros(totalSamples, 1);
  toneR = zeros(totalSamples, 1);
  samplesPerBeat = round(beatDuration * fs);
  samplesPerBar = 4 * samplesPerBeat;
  sixteenth = round(samplesPerBeat / 4);

  progression = [45 41 48 43];
  chordTones = {...
    [57 60 64], [53 57 60], [60 64 67], [55 59 62]};
  sectionStartBars = [2 5 8 11 14 18];
  leadBars = 14:19;
  leadRiff = [69 72 71 69 67 69 64 67];
  drumsElapsed = 0;
  bassElapsed = 0;
  arpElapsed = 0;
  leadElapsed = 0;
  padElapsed = 0;

  for barIndex = 0:bars - 1
    barStart = barIndex * samplesPerBar + 1;
    chordId = mod(barIndex, 4) + 1;
    rootMidi = progression(chordId);
    tones = chordTones{chordId};

    if any(sectionStartBars == barIndex)
      phaseTimer = tic();
      crash = demoSynthCrash(fs);
      [drumsL, drumsR] = demoMix(drumsL, drumsR, crash, barStart, 0.35, 0.35);
      drumsElapsed = drumsElapsed + toc(phaseTimer);
    end

    for b = 0:3
      phaseTimer = tic();
      kickStart = barStart + b * samplesPerBeat;
      kick = demoSynthKick(fs);
      [drumsL, drumsR] = demoMix(drumsL, drumsR, kick, kickStart, 1.0, 1.0);
      if b == 1 || b == 3
        snare = demoSynthSnare(fs);
        [drumsL, drumsR] = demoMix(drumsL, drumsR, snare, kickStart, ...
          0.55, 0.55);
      end
      drumsElapsed = drumsElapsed + toc(phaseTimer);
    end

    for h = 0:7
      phaseTimer = tic();
      hatStart = barStart + h * round(samplesPerBeat / 2);
      hat = demoSynthHat(fs);
      pan = 0.4 + 0.2 * mod(h, 2);
      openHat = mod(h, 4) == 3;
      gain = 0.18;
      if openHat
        hat = demoSynthHat(fs);
        gain = 0.14;
      end
      [drumsL, drumsR] = demoMix(drumsL, drumsR, hat, hatStart, ...
        gain * (2 - pan), gain * pan);
      drumsElapsed = drumsElapsed + toc(phaseTimer);
    end

    bassPattern = [1 0 1 0 0 1 0 1] > 0;
    for s = 0:7
      if ~bassPattern(s + 1)
        continue
      end
      phaseTimer = tic();
      bassStart = barStart + s * round(samplesPerBeat / 2);
      freq = demoNoteFreq(rootMidi);
      note = demoSynthBass(freq, round(samplesPerBeat / 2), fs);
      [toneL, toneR] = demoMix(toneL, toneR, note, bassStart, 0.60, 0.60);
      bassElapsed = bassElapsed + toc(phaseTimer);
    end

    arpSequence = [1 2 3 2 1 2 3 2 1 2 3 2 1 3 2 3];
    for s = 0:15
      phaseTimer = tic();
      arpStart = barStart + s * sixteenth;
      toneMidi = tones(arpSequence(s + 1)) + 12;
      freq = demoNoteFreq(toneMidi);
      note = demoSynthArp(freq, sixteenth, fs);
      pan = 0.5 + 0.35 * sin(s * 0.6);
      [toneL, toneR] = demoMix(toneL, toneR, note, arpStart, ...
        0.20 * (1 + (1 - pan)), 0.20 * (1 + pan));
      arpElapsed = arpElapsed + toc(phaseTimer);
    end

    if any(leadBars == barIndex)
      for s = 0:7
        phaseTimer = tic();
        leadStart = barStart + s * round(samplesPerBeat / 2);
        leadMidi = leadRiff(s + 1);
        note = demoSynthLead(demoNoteFreq(leadMidi), ...
          round(samplesPerBeat / 2), fs);
        [toneL, toneR] = demoMix(toneL, toneR, note, leadStart, 0.30, 0.30);
        leadElapsed = leadElapsed + toc(phaseTimer);
      end
    end

    phaseTimer = tic();
    padFreqs = demoNoteFreq(tones);
    pad = demoSynthPad(padFreqs, samplesPerBar, fs);
    [toneL, toneR] = demoMix(toneL, toneR, pad, barStart, 0.20, 0.20);
    padElapsed = padElapsed + toc(phaseTimer);
  end

  postTimer = tic();
  duck = demoSidechain(totalSamples, samplesPerBeat, fs);
  toneL = toneL .* duck;
  toneR = toneR .* duck;
  left = drumsL + toneL;
  right = drumsR + toneR;

  left = left(1:round(duration * fs));
  right = right(1:round(duration * fs));
  left = tanh(left * 1.3);
  right = tanh(right * 1.3);
  peak = max(max(abs(left)), max(abs(right)));
  if peak > 0
    left = left / peak * 0.95;
    right = right / peak * 0.95;
  end
  fadeLen = round(0.05 * fs);
  fade = linspace(0, 1, fadeLen)';
  left(1:fadeLen) = left(1:fadeLen) .* fade;
  right(1:fadeLen) = right(1:fadeLen) .* fade;
  left(end - fadeLen + 1:end) = left(end - fadeLen + 1:end) .* flipud(fade);
  right(end - fadeLen + 1:end) = right(end - fadeLen + 1:end) .* flipud(fade);
  postElapsed = toc(postTimer);

  synthDuration = duration;
  ambientTimer = tic();
  [ambientL, ambientR] = demoBuildAmbient(fs);
  ambientElapsed = toc(ambientTimer);
  left = [left; ambientL];
  right = [right; ambientR];
  totalDuration = numel(left) / fs;

  music = struct();
  music.samples = [left right];
  music.sampleRate = fs;
  music.beatDuration = beatDuration;
  music.barDuration = barDuration;
  music.duration = totalDuration;
  music.eonStart = synthDuration;
  music.bpm = bpm;
  music.profile = struct('drums', drumsElapsed, 'bass', bassElapsed, ...
    'arp', arpElapsed, 'lead', leadElapsed, 'pad', padElapsed, ...
    'post', postElapsed, 'ambient', ambientElapsed, ...
    'total', drumsElapsed + bassElapsed + arpElapsed + leadElapsed + ...
    padElapsed + postElapsed + ambientElapsed);
end
%=============================================================================
function freq = demoNoteFreq(midi)
  freq = 440 * 2 .^ ((midi - 69) / 12);
end
%=============================================================================
function [left, right] = demoMix(left, right, signal, startSample, gainL, gainR)
  n = numel(signal);
  stop = startSample + n - 1;
  if startSample < 1 || stop > numel(left)
    stop = min(stop, numel(left));
    n = stop - startSample + 1;
    if n <= 0
      return
    end
    signal = signal(1:n);
  end
  range = startSample:stop;
  left(range) = left(range) + signal * gainL;
  right(range) = right(range) + signal * gainR;
end
%=============================================================================
function y = demoSynthKick(fs)
  dur = 0.22;
  n = round(dur * fs);
  tt = (0:n - 1)' / fs;
  freq = 150 * exp(-tt * 38) + 42;
  phase = 2 * pi * cumsum(freq) / fs;
  amp = exp(-tt * 18);
  y = sin(phase) .* amp;
  click = exp(-tt * 500) * 0.7;
  y = tanh((y + click) * 1.3);
end
%=============================================================================
function y = demoSynthCrash(fs)
  dur = 0.9;
  n = round(dur * fs);
  tt = (0:n - 1)' / fs;
  noise = demoNoiseVector(n);
  noise = noise - [0; noise(1:end - 1)];
  amp = exp(-tt * 4.5) .* (1 - exp(-tt * 200));
  y = noise .* amp;
end
%=============================================================================
function y = demoSynthLead(freq, n, fs)
  tt = (0:n - 1)' / fs;
  vibrato = 1 + 0.006 * sin(2 * pi * 5.5 * tt);
  phase = 2 * pi * freq * tt .* vibrato;
  saw = 2 * (phase / (2 * pi) - floor(0.5 + phase / (2 * pi)));
  square = sign(sin(phase));
  raw = 0.6 * saw + 0.4 * square;
  env = min(1, tt * 120) .* exp(-tt * 4.5);
  y = tanh(raw * 1.2) .* env * 0.7;
end
%=============================================================================
function duck = demoSidechain(totalSamples, samplesPerBeat, fs)
  duck = ones(totalSamples, 1);
  attack = round(0.004 * fs);
  release = round(0.26 * fs);
  shape = ones(release, 1);
  shape(1:min(release, attack)) = linspace(0.35, 0.35, min(release, attack));
  recovery = linspace(0.35, 1, release)' .^ 0.6;
  numBeats = floor(totalSamples / samplesPerBeat);
  for b = 0:numBeats - 1
    startSample = b * samplesPerBeat + 1;
    stop = min(totalSamples, startSample + release - 1);
    len = stop - startSample + 1;
    duck(startSample:stop) = min(duck(startSample:stop), recovery(1:len));
  end
end
%=============================================================================
function y = demoSynthSnare(fs)
  dur = 0.16;
  n = round(dur * fs);
  tt = (0:n - 1)' / fs;
  noise = demoNoiseVector(n);
  amp = exp(-tt * 26);
  tone = sin(2 * pi * 190 * tt) .* exp(-tt * 30) * 0.4;
  y = (noise .* amp) * 0.8 + tone;
end
%=============================================================================
function y = demoSynthHat(fs)
  dur = 0.04;
  n = round(dur * fs);
  tt = (0:n - 1)' / fs;
  noise = demoNoiseVector(n);
  noise = noise - [0; noise(1:end - 1)];
  amp = exp(-tt * 120);
  y = noise .* amp;
end
%=============================================================================
function y = demoSynthBass(freq, n, fs)
  tt = (0:n - 1)' / fs;
  phase = 2 * pi * freq * tt;
  saw = 2 * (phase / (2 * pi) - floor(0.5 + phase / (2 * pi)));
  square = sign(sin(phase));
  raw = 0.7 * saw + 0.3 * square;
  env = min(1, tt * 80) .* exp(-tt * 6);
  y = tanh(raw * 1.5) .* env;
end
%=============================================================================
function y = demoSynthArp(freq, n, fs)
  tt = (0:n - 1)' / fs;
  phase = 2 * pi * freq * tt;
  duty = 0.5 + 0.15 * sin(tt * 20);
  square = double(mod(phase / (2 * pi), 1) < duty) * 2 - 1;
  env = min(1, tt * 200) .* exp(-tt * 12);
  y = square .* env * 0.6;
end
%=============================================================================
function y = demoSynthPad(freqs, n, fs)
  tt = (0:n - 1)' / fs;
  y = zeros(n, 1);
  for k = 1:numel(freqs)
    detune = 1 + 0.004 * (k - 2);
    saw1 = 2 * (mod(freqs(k) * detune * tt, 1)) - 1;
    saw2 = 2 * (mod(freqs(k) * 1.006 * tt, 1)) - 1;
    y = y + (saw1 + saw2);
  end
  y = y / (numel(freqs) * 2);
  env = min(1, tt * 4) .* min(1, (max(tt) - tt) * 4 + 0.2);
  y = y .* env;
end
%=============================================================================
function noise = demoNoiseVector(n)
  idx = (1:n)';
  s = sin(idx * 12.9898 + idx * 0.017) * 43758.5453;
  noise = 2 * (s - floor(s)) - 1;
end
%=============================================================================
% ---- 5x7 bitmap font ------------------------------------------------------
%=============================================================================
function [chars, glyphs] = demoFontData()
  persistent cachedChars cachedGlyphs
  if ~isempty(cachedChars)
    chars = cachedChars;
    glyphs = cachedGlyphs;
    return
  end
  definitions = {...
    ' ', {'.....', '.....', '.....', '.....', '.....', '.....', '.....'}; ...
    'A', {'.###.', '#...#', '#...#', '#####', '#...#', '#...#', '#...#'}; ...
    'B', {'####.', '#...#', '#...#', '####.', '#...#', '#...#', '####.'}; ...
    'C', {'.####', '#....', '#....', '#....', '#....', '#....', '.####'}; ...
    'D', {'####.', '#...#', '#...#', '#...#', '#...#', '#...#', '####.'}; ...
    'E', {'#####', '#....', '#....', '####.', '#....', '#....', '#####'}; ...
    'F', {'#####', '#....', '#....', '####.', '#....', '#....', '#....'}; ...
    'G', {'.####', '#....', '#....', '#.###', '#...#', '#...#', '.####'}; ...
    'H', {'#...#', '#...#', '#...#', '#####', '#...#', '#...#', '#...#'}; ...
    'I', {'.###.', '..#..', '..#..', '..#..', '..#..', '..#..', '.###.'}; ...
    'J', {'..###', '...#.', '...#.', '...#.', '#..#.', '#..#.', '.##..'}; ...
    'K', {'#...#', '#..#.', '#.#..', '##...', '#.#..', '#..#.', '#...#'}; ...
    'L', {'#....', '#....', '#....', '#....', '#....', '#....', '#####'}; ...
    'M', {'#...#', '##.##', '#.#.#', '#.#.#', '#...#', '#...#', '#...#'}; ...
    'N', {'#...#', '##..#', '#.#.#', '#..##', '#...#', '#...#', '#...#'}; ...
    'O', {'.###.', '#...#', '#...#', '#...#', '#...#', '#...#', '.###.'}; ...
    'P', {'####.', '#...#', '#...#', '####.', '#....', '#....', '#....'}; ...
    'Q', {'.###.', '#...#', '#...#', '#...#', '#.#.#', '#..#.', '.##.#'}; ...
    'R', {'####.', '#...#', '#...#', '####.', '#.#..', '#..#.', '#...#'}; ...
    'S', {'.####', '#....', '#....', '.###.', '....#', '....#', '####.'}; ...
    'T', {'#####', '..#..', '..#..', '..#..', '..#..', '..#..', '..#..'}; ...
    'U', {'#...#', '#...#', '#...#', '#...#', '#...#', '#...#', '.###.'}; ...
    'V', {'#...#', '#...#', '#...#', '#...#', '#...#', '.#.#.', '..#..'}; ...
    'W', {'#...#', '#...#', '#...#', '#.#.#', '#.#.#', '##.##', '#...#'}; ...
    'X', {'#...#', '#...#', '.#.#.', '..#..', '.#.#.', '#...#', '#...#'}; ...
    'Y', {'#...#', '#...#', '.#.#.', '..#..', '..#..', '..#..', '..#..'}; ...
    'Z', {'#####', '....#', '...#.', '..#..', '.#...', '#....', '#####'}; ...
    '0', {'.###.', '#...#', '#..##', '#.#.#', '##..#', '#...#', '.###.'}; ...
    '1', {'..#..', '.##..', '..#..', '..#..', '..#..', '..#..', '.###.'}; ...
    '2', {'.###.', '#...#', '....#', '..##.', '.#...', '#....', '#####'}; ...
    '3', {'####.', '....#', '....#', '.###.', '....#', '....#', '####.'}; ...
    '4', {'...#.', '..##.', '.#.#.', '#..#.', '#####', '...#.', '...#.'}; ...
    '5', {'#####', '#....', '####.', '....#', '....#', '#...#', '.###.'}; ...
    '6', {'.###.', '#....', '#....', '####.', '#...#', '#...#', '.###.'}; ...
    '7', {'#####', '....#', '...#.', '..#..', '.#...', '.#...', '.#...'}; ...
    '8', {'.###.', '#...#', '#...#', '.###.', '#...#', '#...#', '.###.'}; ...
    '9', {'.###.', '#...#', '#...#', '.####', '....#', '....#', '.###.'}; ...
    '.', {'.....', '.....', '.....', '.....', '.....', '.##..', '.##..'}; ...
    '-', {'.....', '.....', '.....', '#####', '.....', '.....', '.....'}; ...
    '!', {'..#..', '..#..', '..#..', '..#..', '..#..', '.....', '..#..'}; ...
    ':', {'.....', '.##..', '.##..', '.....', '.##..', '.##..', '.....'}};
  chars = '';
  glyphs = cell(size(definitions, 1), 1);
  for k = 1:size(definitions, 1)
    chars(end + 1) = definitions{k, 1};
    rows = definitions{k, 2};
    glyph = zeros(7, 5);
    for r = 1:7
      line = rows{r};
      for c = 1:5
        if line(c) == '#'
          glyph(r, c) = 1;
        end
      end
    end
    glyphs{k} = glyph;
  end
  cachedChars = chars;
  cachedGlyphs = glyphs;
end
%=============================================================================
% ---- Eon tribute epilogue: dot-matrix logo, Earth, dithered forest --------
%=============================================================================
function frame = eonSceneLogo(localT, t, beat, width, height)
  spacing = 6;
  cellsX = floor(width / spacing);
  cellsY = floor(height / spacing);
  emblem = eonLotusEmblemGrid(cellsX, cellsY, t * 0.4);
  bright = 0.10 * ones(cellsY, cellsX);
  bright = max(bright, emblem * (0.75 + 0.25 * sin(t * 2)));
  message = 'THE BLACK LOTUS > EON > REVISION 2019 > PURE NELSON > ';
  textGrid = eonTextTickerGrid(message, cellsX, localT);
  textRow0 = round(cellsY * 0.72);
  textRows = textRow0 + (0:size(textGrid, 1) - 1);
  valid = textRows >= 1 & textRows <= cellsY;
  bright(textRows(valid), :) = max(bright(textRows(valid), :), ...
    textGrid(valid, :) * 0.9);
  redMask = eonLotusCenterGrid(cellsX, cellsY);
  blueMask = zeros(cellsY, cellsX);
  baccX = round(cellsX * 0.5 + cellsX * 0.30 * cos(t * 0.4));
  baccY = round(cellsY * 0.42 + cellsY * 0.30 * sin(t * 0.4));
  if baccX >= 1 && baccX <= cellsX && baccY >= 1 && baccY <= cellsY
    blueMask(baccY, baccX) = 1;
  end
  stamp = eonDotStamp(spacing);
  lum = eonCropTo(kron(bright, stamp), height, width);
  redLum = eonCropTo(kron(redMask, stamp), height, width) * 0.9;
  blueLum = eonCropTo(kron(blueMask, stamp), height, width) * 0.95;
  frame = zeros(height, width, 3);
  frame(:, :, 1) = lum + redLum - blueLum * 0.6;
  frame(:, :, 2) = lum * 0.98 + blueLum * 0.3;
  frame(:, :, 3) = lum * 0.96 - redLum * 0.6 + blueLum;
  frame = min(1, max(0, frame));
end
%=============================================================================
function grid = eonLotusEmblemGrid(cellsX, cellsY, rot)
  [gx, gy] = meshgrid(1:cellsX, 1:cellsY);
  cx = cellsX / 2;
  cy = cellsY * 0.42;
  nx = (gx - cx) / (cellsX * 0.30);
  ny = (gy - cy) / (cellsY * 0.30);
  angle = atan2(ny, nx);
  radius = hypot(nx, ny);
  petalEdge = 0.55 + 0.45 * cos(6 * (angle - rot));
  grid = double(radius < petalEdge & radius > 0.12);
  grid = max(grid, double(radius < 0.14));
end
%=============================================================================
function grid = eonLotusCenterGrid(cellsX, cellsY)
  [gx, gy] = meshgrid(1:cellsX, 1:cellsY);
  cx = cellsX / 2;
  cy = cellsY * 0.42;
  radius = hypot((gx - cx) / (cellsX * 0.30), (gy - cy) / (cellsY * 0.30));
  grid = double(radius < 0.14);
end
%=============================================================================
function grid = eonTextTickerGrid(message, cellsX, localT)
  persistent cachedMessage textBmp textWidth
  if isempty(cachedMessage) || ~strcmp(cachedMessage, message)
    cachedMessage = message;
    [textBmp, textWidth, ~] = demoBuildTextBitmap(message, 1);
  end
  speed = 9;
  offset = mod(floor(localT * speed), textWidth);
  cols = mod((0:cellsX - 1) + offset, textWidth) + 1;
  grid = double(textBmp(:, cols) > 0);
end
%=============================================================================
function stamp = eonDotStamp(spacing)
  persistent cachedSpacing cachedStamp
  if ~isempty(cachedStamp) && cachedSpacing == spacing
    stamp = cachedStamp;
    return
  end
  [sx, sy] = meshgrid(1:spacing, 1:spacing);
  c = (spacing + 1) / 2;
  r = hypot(sx - c, sy - c);
  stamp = max(0, 1 - (r / (spacing * 0.42)) .^ 2);
  cachedSpacing = spacing;
  cachedStamp = stamp;
end
%=============================================================================
function out = eonCropTo(img, height, width)
  out = zeros(height, width);
  h = min(height, size(img, 1));
  w = min(width, size(img, 2));
  out(1:h, 1:w) = img(1:h, 1:w);
end
%=============================================================================
function frame = eonSceneEarth(localT, t, beat, width, height)
  persistent cachedWidth cachedHeight starField px py
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    [px, py] = meshgrid(1:width, 1:height);
    starField = zeros(height, width);
    n = 220;
    sx = 1 + floor(demoPseudoRandom(n, 3) * (width - 1));
    sy = 1 + floor(demoPseudoRandom(n, 5) * (height - 1));
    b = 0.3 + 0.6 * demoPseudoRandom(n, 7);
    starField(sy + (sx - 1) * height) = b;
  end
  cx = width / 2;
  cy = height * 1.55;
  planetR = height * 1.15;
  dist = hypot(px - cx, py - cy);
  twinkle = 0.7 + 0.3 * sin(t * 1.5 + px * 0.2);
  frame = zeros(height, width, 3);
  space = starField .* twinkle;
  frame(:, :, 1) = space * 0.75;
  frame(:, :, 2) = space * 0.8;
  frame(:, :, 3) = space;
  atmoWidth = height * 0.10;
  atmo = exp(-((dist - planetR) / atmoWidth) .^ 2);
  glowColor = reshape([0.55 0.85 1.0], 1, 1, 3);
  frame = frame + repmat(atmo, 1, 1, 3) .* glowColor;
  inside = dist < planetR;
  terminator = min(1, max(0, (px - width * 0.15) / width + ...
    (cy - py) / height * 0.3));
  bands = 0.5 + 0.5 * sin((py - cy) * 0.08 + px * 0.02);
  surfBlue = (0.04 + 0.10 * bands) .* terminator;
  limb = exp(-((planetR - dist) / (atmoWidth * 1.2)) .^ 2) .* inside;
  surface = zeros(height, width, 3);
  surface(:, :, 1) = surfBlue * 0.35 + 0.35 * limb;
  surface(:, :, 2) = surfBlue * 0.6 + 0.02 * bands .* terminator + 0.55 * limb;
  surface(:, :, 3) = surfBlue + 0.75 * limb;
  insideRgb = repmat(inside, 1, 1, 3);
  frame(insideRgb) = surface(insideRgb);
  emblem = eonEmblemShape(width, height, localT);
  halo = exp(-(hypot(px - width / 2, py - height * 0.42) / ...
    (height * 0.42)) .^ 2) * 0.20;
  frame(:, :, 1) = frame(:, :, 1) + halo * 0.6;
  frame(:, :, 2) = frame(:, :, 2) + halo * 0.8;
  frame(:, :, 3) = frame(:, :, 3) + halo;
  emblemRgb = repmat(emblem, 1, 1, 3);
  frame = frame .* (1 - emblemRgb) + emblemRgb;
  frame = min(1, max(0, frame));
end
%=============================================================================
function mask = eonEmblemShape(width, height, localT)
  persistent cachedWidth cachedHeight baseMask
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    [gx, gy] = meshgrid(1:width, 1:height);
    cx = width / 2;
    cy = height * 0.42;
    nx = (gx - cx) / (height * 0.34);
    ny = (gy - cy) / (height * 0.34);
    angle = atan2(ny, nx);
    radius = hypot(nx, ny);
    lobe = 0.55 + 0.45 * cos(3 * (angle + pi / 2));
    petal = radius < lobe;
    center = radius < 0.24;
    baseMask = double(petal | center);
  end
  mask = baseMask * (0.85 + 0.15 * sin(localT * 1.5));
end
%=============================================================================
function frame = eonSceneAnother(localT, t, beat, width, height)
  persistent cachedWidth cachedHeight px py sR sG sB roadTop
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    [px, py] = meshgrid(1:width, 1:height);
    roadTop = height * 0.72;
    vy = max(0, min(1, py / roadTop));
    sR = 0.05 + 0.05 * vy;
    sG = 0.06 + 0.06 * vy;
    sB = 0.11 + 0.11 * vy;
    lab = px > width * 0.80 & px < width * 0.97 & ...
      py > roadTop - height * 0.32 & py < roadTop;
    sR(lab) = 0.03; sG(lab) = 0.035; sB(lab) = 0.06;
    door = px > width * 0.865 & px < width * 0.915 & ...
      py > roadTop - height * 0.15 & py < roadTop;
    sR(door) = 0.55; sG(door) = 0.42; sB(door) = 0.18;
    tower = px > width * 0.61 & px < width * 0.66 & ...
      py > roadTop - height * 0.22 & py < roadTop;
    sR(tower) = 0.03; sG(tower) = 0.035; sB(tower) = 0.065;
    road = py >= roadTop;
    sR(road) = 0.05; sG(road) = 0.05; sB(road) = 0.07;
  end
  R = sR;
  G = sG;
  B = sB;
  rain = mod(round(px * 0.5 + py - t * 165), 9) < 1 & mod(round(py), 3) < 2;
  R(rain) = R(rain) + 0.20;
  G(rain) = G(rain) + 0.24;
  B(rain) = B(rain) + 0.32;
  dashY = round(roadTop + height * 0.14);
  if dashY >= 1 && dashY <= height
    dcols = find(mod(round((1:width) + t * 220), 26) < 11);
    for rr = max(1, dashY - 1):min(height, dashY + 1)
      R(rr, dcols) = 0.16;
      G(rr, dcols) = 0.16;
      B(rr, dcols) = 0.20;
    end
  end
  startX = -width * 0.28;
  restX = width * 0.42;
  ccx = min(restX, startX + (restX - startX) * min(1, localT / 4.5));
  bob = height * 0.004 * sin(localT * 9);
  wr = height * 0.05;
  frontX = ccx + width * 0.10;
  rearX = ccx - width * 0.10;
  wheelY = roadTop - height * 0.005;
  reflMask = px > ccx - width * 0.16 & px < ccx + width * 0.16 & ...
    py > roadTop & py < roadTop + height * 0.10;
  reflFall = max(0, 1 - (py - roadTop) / (height * 0.10));
  R = R + reflMask .* reflFall * 0.07;
  G = G + reflMask .* reflFall * 0.08;
  B = B + reflMask .* reflFall * 0.12;
  [bodyIdx, windowIdx, wheelIdx, hubIdx] = eonCarAt(width, height, ccx, ...
    roadTop, bob);
  R(bodyIdx) = 0.05; G(bodyIdx) = 0.05; B(bodyIdx) = 0.07;
  R(windowIdx) = 0.24; G(windowIdx) = 0.34; B(windowIdx) = 0.48;
  R(wheelIdx) = 0.04; G(wheelIdx) = 0.04; B(wheelIdx) = 0.05;
  R(hubIdx) = 0.32; G(hubIdx) = 0.32; B(hubIdx) = 0.35;
  spin = t * 22;
  [R, G, B] = eonWheelSpokes(R, G, B, height, width, frontX, wheelY, wr, spin);
  [R, G, B] = eonWheelSpokes(R, G, B, height, width, rearX, wheelY, wr, spin);
  hlX = ccx + width * 0.165;
  hlY = roadTop - height * 0.055 + bob;
  beam = exp(-((py - hlY) / (height * 0.05)) .^ 2) .* ...
    max(0, 1 - (px - hlX) / (width * 0.38)) .* (px > hlX);
  R = R + beam * 0.35;
  G = G + beam * 0.33;
  B = B + beam * 0.15;
  hl = hypot(px - hlX, py - hlY) < height * 0.013;
  R(hl) = 1.0;
  G(hl) = 0.95;
  B(hl) = 0.6;
  tlX = ccx - width * 0.165;
  tlY = roadTop - height * 0.05 + bob;
  tlGlow = exp(-(hypot(px - tlX, py - tlY) / (height * 0.028)) .^ 2);
  R = R + tlGlow * 0.30;
  tl = hypot(px - tlX, py - tlY) < height * 0.010;
  R(tl) = 1.0; G(tl) = 0.15; B(tl) = 0.12;
  if localT < 4.8
    ns = 12;
    life = mod(demoPseudoRandom(ns, 31) + localT * 2.2, 1);
    spx = round(rearX - width * 0.02 - life * width * 0.10);
    spy = round(roadTop - life * height * 0.045 + ...
      demoPseudoRandom(ns, 33) * height * 0.012);
    sv = spx >= 1 & spx <= width & spy >= 1 & spy <= height;
    sidx = spy(sv) + (spx(sv) - 1) * height;
    sb = 0.40 * (1 - life(sv));
    R(sidx) = R(sidx) + sb;
    G(sidx) = G(sidx) + sb;
    B(sidx) = B(sidx) + sb;
  end
  doorOpen = min(1, max(0, (localT - 3.2) / 2.5));
  spill = doorOpen * exp(-((px - width * 0.89) / (width * 0.09)) .^ 2) .* ...
    double(py > roadTop) .* max(0, 1 - (py - roadTop) / (height * 0.12));
  R = R + spill * 0.35;
  G = G + spill * 0.22;
  B = B + spill * 0.08;
  boltPhase = mod(localT, 4.7);
  bolt = 0.0;
  if boltPhase < 0.14
    bolt = 0.42 * (1 - boltPhase / 0.14);
    [R, G, B] = eonBolt(R, G, B, height, width, roadTop, floor(localT / 4.7));
  end
  frame = zeros(height, width, 3);
  frame(:, :, 1) = min(1, max(0, R + bolt));
  frame(:, :, 2) = min(1, max(0, G + bolt));
  frame(:, :, 3) = min(1, max(0, B + bolt));
end
%=============================================================================
function [R, G, B] = eonWheelSpokes(R, G, B, height, width, wx, wy, wr, spin)
  for k = 0:3
    a = spin + k * pi / 2;
    steps = max(3, round(wr));
    rs = linspace(wr * 0.30, wr * 0.82, steps);
    xs = round(wx + cos(a) * rs);
    ys = round(wy + sin(a) * rs);
    valid = xs >= 1 & xs <= width & ys >= 1 & ys <= height;
    idx = ys(valid) + (xs(valid) - 1) * height;
    R(idx) = 0.30;
    G(idx) = 0.30;
    B(idx) = 0.33;
  end
end
%=============================================================================
function [bodyIdx, windowIdx, wheelIdx, hubIdx] = eonCarAt(width, height, ...
  ccx, roadTop, bob)
  x0 = max(1, floor(ccx - width * 0.22));
  x1 = min(width, ceil(ccx + width * 0.22));
  y0 = max(1, floor(roadTop - height * 0.20));
  y1 = min(height, ceil(roadTop + height * 0.02));
  [gx, gy] = meshgrid(x0:x1, y0:y1);
  carY = roadTop - height * 0.055 + bob;
  body = ((gx - ccx) / (width * 0.17)) .^ 2 + ...
    ((gy - carY) / (height * 0.05)) .^ 2 < 1;
  cabin = ((gx - (ccx - width * 0.02)) / (width * 0.085)) .^ 2 + ...
    ((gy - (roadTop - height * 0.11 + bob)) / (height * 0.05)) .^ 2 < 1 & ...
    gy < roadTop - height * 0.05 + bob;
  spoiler = gx > ccx - width * 0.17 & gx < ccx - width * 0.11 & ...
    gy > carY - height * 0.05 & gy < carY - height * 0.03;
  carBody = body | cabin | spoiler;
  window = ((gx - (ccx - width * 0.02)) / (width * 0.055)) .^ 2 + ...
    ((gy - (roadTop - height * 0.115 + bob)) / (height * 0.028)) .^ 2 < 1;
  wr = height * 0.05;
  frontX = ccx + width * 0.10;
  rearX = ccx - width * 0.10;
  wheelY = roadTop - height * 0.005;
  dFr = hypot(gx - frontX, gy - wheelY);
  dRe = hypot(gx - rearX, gy - wheelY);
  wheel = dFr < wr | dRe < wr;
  hub = dFr < wr * 0.42 | dRe < wr * 0.42;
  carBody = carBody & ~wheel;
  window = window & ~wheel;
  bodyIdx = eonSubIdx(carBody, x0, y0, height);
  windowIdx = eonSubIdx(window, x0, y0, height);
  wheelIdx = eonSubIdx(wheel, x0, y0, height);
  hubIdx = eonSubIdx(hub, x0, y0, height);
end
%=============================================================================
function idx = eonSubIdx(mask, x0, y0, height)
  [sr, sc] = find(mask);
  idx = (y0 - 1 + sr) + (x0 - 1 + sc - 1) * height;
end
%=============================================================================
function [R, G, B] = eonBolt(R, G, B, height, width, roadTop, seed)
  nb = 16;
  ys = round(linspace(1, roadTop * 0.96, nb));
  bx0 = width * (0.30 + 0.45 * mod(seed * 0.61803, 1));
  jitter = width * 0.05 * sin((1:nb) * 1.7 + seed) + ...
    width * 0.02 * (mod((1:nb) * (seed + 2), 3) - 1);
  xs = round(bx0 + jitter);
  for k = 1:nb - 1
    steps = max(2, abs(ys(k + 1) - ys(k)) + 1);
    xx = round(linspace(xs(k), xs(k + 1), steps));
    yy = round(linspace(ys(k), ys(k + 1), steps));
    valid = xx >= 1 & xx <= width & yy >= 1 & yy <= height;
    idx = yy(valid) + (xx(valid) - 1) * height;
    R(idx) = 1.0;
    G(idx) = 1.0;
    B(idx) = 1.0;
  end
end
%=============================================================================
function landMask = eonAnotherLand(width, height)
  [gx, gy] = meshgrid(1:width, 1:height);
  waterLine = height * 0.72;
  ridge = height * 0.40 - height * 0.10 * abs(sin(gx * 0.012)) - ...
    height * 0.05 * sin(gx * 0.05 + 1.0);
  landMask = gy > ridge & gy < waterLine;
end
%=============================================================================
function mask = eonLesterAt(gx, gy, width, height, lx, feet, localT)
  lh = height * 0.17;
  headR = height * 0.02;
  gait = height * 0.02 * sin(localT * 5);
  head = hypot(gx - lx, gy - (feet - lh)) < headR;
  body = abs(gx - lx) < height * 0.013 & gy > feet - lh + headR & ...
    gy < feet - lh * 0.42;
  legL = abs(gx - (lx - height * 0.012 + gait)) < height * 0.010 & ...
    gy >= feet - lh * 0.42 & gy <= feet;
  legR = abs(gx - (lx + height * 0.012 - gait)) < height * 0.010 & ...
    gy >= feet - lh * 0.42 & gy <= feet;
  arm = abs(gy - (feet - lh * 0.66)) < height * 0.010 & ...
    abs(gx - lx - gait * 0.5) < height * 0.032;
  mask = head | body | legL | legR | arm;
end
%=============================================================================
function mask = eonBeastAt(gx, gy, width, height, bx, by)
  bodyB = ((gx - bx) / (width * 0.15)) .^ 2 + ...
    ((gy - (by - height * 0.09)) / (height * 0.06)) .^ 2 < 1;
  headB = ((gx - (bx - width * 0.15)) / (width * 0.055)) .^ 2 + ...
    ((gy - (by - height * 0.13)) / (height * 0.05)) .^ 2 < 1;
  neckB = ((gx - (bx - width * 0.09)) / (width * 0.06)) .^ 2 + ...
    ((gy - (by - height * 0.11)) / (height * 0.045)) .^ 2 < 1;
  legB = (abs(gx - (bx - width * 0.05)) < width * 0.013 | ...
    abs(gx - (bx + width * 0.06)) < width * 0.013) & ...
    gy > by - height * 0.09 & gy <= by;
  tailB = ((gx - (bx + width * 0.16)) / (width * 0.07)) .^ 2 + ...
    ((gy - (by - height * 0.13)) / (height * 0.018)) .^ 2 < 1;
  earB = (abs(gx - (bx - width * 0.17)) + abs(gy - (by - height * 0.17))) < ...
    width * 0.025;
  mask = bodyB | headB | neckB | legB | tailB | earB;
end
%=============================================================================
function mask = eonWolfMask(width, height)
  [gx, gy] = meshgrid(1:width, 1:height);
  x = (gx - width * 0.5) / (width * 0.5);
  y = (gy - height * 0.62) / (height * 0.5);
  body = (x / 0.34) .^ 2 + (y / 0.12) .^ 2 < 1;
  chest = ((x - 0.22) / 0.12) .^ 2 + ((y - 0.02) / 0.16) .^ 2 < 1;
  neck = ((x - 0.26) / 0.07) .^ 2 + ((y + 0.16) / 0.14) .^ 2 < 1;
  head = ((x - 0.33) / 0.10) .^ 2 + ((y + 0.24) / 0.08) .^ 2 < 1;
  ear1 = abs(x - 0.30) + abs(y + 0.34) < 0.06;
  ear2 = abs(x - 0.37) + abs(y + 0.34) < 0.06;
  tail = ((x + 0.34) / 0.12) .^ 2 + ((y + 0.05) / 0.05) .^ 2 < 1;
  legFront = abs(x - 0.24) < 0.05 & y > 0 & y < 0.42;
  legFront2 = abs(x - 0.14) < 0.05 & y > 0 & y < 0.42;
  legBack = abs(x + 0.16) < 0.055 & y > 0 & y < 0.42;
  legBack2 = abs(x + 0.26) < 0.055 & y > 0 & y < 0.42;
  mask = double(body | chest | neck | head | ear1 | ear2 | tail | ...
    legFront | legFront2 | legBack | legBack2);
end
%=============================================================================
function frame = eonDitherDuotone(lum, colA, colB, levels, width, height)
  persistent cachedWidth cachedHeight tiled
  if isempty(cachedWidth) || cachedWidth ~= width || cachedHeight ~= height
    cachedWidth = width;
    cachedHeight = height;
    bayer = [0 8 2 10; 12 4 14 6; 3 11 1 9; 15 7 13 5] / 16 - 0.5;
    tiled = repmat(bayer, ceil(height / 4), ceil(width / 4));
    tiled = tiled(1:height, 1:width);
  end
  q = min(1, max(0, round(lum * (levels - 1) + tiled) / (levels - 1)));
  frame = zeros(height, width, 3);
  for ch = 1:3
    frame(:, :, ch) = colA(ch) + (colB(ch) - colA(ch)) * q;
  end
end
%=============================================================================
function value = eonScalarRandom(k, seed)
  s = sin(k * 12.9898 + seed * 78.233) * 43758.5453;
  value = s - floor(s);
end
%=============================================================================
function [left, right] = demoBuildAmbient(fs)
  duration = 32;
  bpm = 76;
  beatD = 60 / bpm;
  barD = 4 * beatD;
  total = round(duration * fs) + fs;
  padBus = zeros(total, 1);
  bellBus = zeros(total, 1);
  subBus = zeros(total, 1);
  spBar = round(barD * fs);
  spBeat = round(beatD * fs);
  chordProg = {...
    [45 52 57 60], [41 48 53 57], [43 50 55 59], [40 47 52 55]};
  nBars = ceil(duration / barD);
  for b = 0:nBars - 1
    bs = b * spBar + 1;
    chord = chordProg{mod(b, 4) + 1};
    pad = eonSynthPad(demoNoteFreq(chord), spBar, fs);
    padBus = eonAdd(padBus, pad, bs, 0.22);
    for k = 0:3
      sub = eonSynthSub(demoNoteFreq(chord(1) - 12), spBeat, fs);
      subBus = eonAdd(subBus, sub, bs + k * spBeat, 0.32);
    end
  end
  bellMelody = [76 79 83 81 76 79 84 83 81 79 76 74];
  bt = (0:numel(bellMelody) - 1) * (duration / numel(bellMelody));
  for k = 1:numel(bellMelody)
    bell = eonSynthBell(demoNoteFreq(bellMelody(k)), round(1.6 * fs), fs);
    bellBus = eonAdd(bellBus, bell, round(bt(k) * fs) + 1, 0.30);
  end
  bellBus = eonEcho(bellBus, round(0.38 * fs), 0.35);
  mono = padBus + subBus + bellBus;
  mono = mono(1:round(duration * fs));
  mono = tanh(mono * 1.2);
  peak = max(abs(mono));
  if peak > 0
    mono = mono / peak * 0.9;
  end
  fadeLen = min(round(1.5 * fs), floor(numel(mono) / 2));
  fade = linspace(0, 1, fadeLen)';
  mono(1:fadeLen) = mono(1:fadeLen) .* fade;
  mono(end - fadeLen + 1:end) = mono(end - fadeLen + 1:end) .* flipud(fade);
  dR = round(0.012 * fs);
  left = mono;
  right = [zeros(dR, 1); mono(1:end - dR)];
end
%=============================================================================
function bus = eonAdd(bus, signal, startSample, gain)
  n = numel(signal);
  stop = startSample + n - 1;
  if startSample < 1
    return
  end
  if stop > numel(bus)
    stop = numel(bus);
    n = stop - startSample + 1;
    if n <= 0
      return
    end
    signal = signal(1:n);
  end
  bus(startSample:stop) = bus(startSample:stop) + signal * gain;
end
%=============================================================================
function y = eonSynthPad(freqs, n, fs)
  tt = (0:n - 1)' / fs;
  y = zeros(n, 1);
  for k = 1:numel(freqs)
    saw1 = 2 * mod(freqs(k) * tt, 1) - 1;
    saw2 = 2 * mod(freqs(k) * 1.005 * tt, 1) - 1;
    y = y + saw1 + saw2;
  end
  y = y / (numel(freqs) * 2);
  env = min(1, tt * 2.5) .* min(1, (max(tt) - tt) * 3 + 0.15);
  y = y .* env;
  alpha = 0.06;
  y = filter(alpha, [1 alpha - 1], y, (1 - alpha) * y(1));
end
%=============================================================================
function y = eonSynthSub(freq, n, fs)
  tt = (0:n - 1)' / fs;
  env = min(1, tt * 40) .* exp(-tt * 3.5);
  y = sin(2 * pi * freq * tt) .* env;
end
%=============================================================================
function y = eonSynthBell(freq, n, fs)
  tt = (0:n - 1)' / fs;
  partials = [1 2 3 4.2];
  gains = [1 0.5 0.28 0.15];
  y = zeros(n, 1);
  for p = 1:numel(partials)
    y = y + gains(p) * sin(2 * pi * freq * partials(p) * tt);
  end
  y = y / sum(gains) .* exp(-tt * 3.2);
end
%=============================================================================
function y = eonEcho(x, delaySamples, feedback)
  y = x;
  gain = feedback;
  for offset = delaySamples:delaySamples:numel(x) - 1
    y(offset + 1:end) = y(offset + 1:end) + gain * x(1:end - offset);
    gain = gain * feedback;
  end
end
%=============================================================================
