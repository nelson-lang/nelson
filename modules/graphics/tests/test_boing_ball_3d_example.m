%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
% <--ADV-CLI MODE-->
%=============================================================================
exampleRoot = fullfile(modulepath('graphics'), 'examples');
examplePath = fullfile(exampleRoot, 'boing_ball_3d.m');
asserts.istrue(isfile(examplePath));
asserts.isequal(parsefile(examplePath), 'function');

source = fileread(examplePath);
asserts.istrue(contains(source, 'targetFrameRate = 63;'));
asserts.istrue(contains(source, 'targetFrameDuration = 1 / targetFrameRate;'));
asserts.istrue(contains(source, 'animationTime = animationTime + wallStep;'));
asserts.istrue(contains(source, 'boingBallDrawFrame(animationState, animationTime);'));
asserts.istrue(contains(source, 'remainingFrameTime = targetFrameDuration'));
asserts.istrue(contains(source, 'spinMargin = 0.0015;'));
asserts.istrue(contains(source, 'while toc(frameTimer) < targetFrameDuration'));
asserts.istrue(contains(source, '''Projection'', ''perspective'''));
asserts.istrue(contains(source, 'axis(axesHandle, [-6.0 6.0 -5.0 5.0 0 5.8]);'));
asserts.istrue(contains(source, 'figureHandle = uifigure'));
asserts.istrue(contains(source, 'axesHandle = uiaxes(figureHandle'));
asserts.istrue(contains(source, "'Units', 'normalized'"));
asserts.istrue(contains(source, "'Position', [0 0.08 1 0.92]"));
asserts.istrue(contains(source, 'horizontalPosition = -1 + 2 * horizontalPhase;'));
asserts.istrue(contains(source, 'spinAngle = -ballX / state.radius;'));
asserts.istrue(contains(source, 'wallShadowHandle = patch'));
asserts.istrue(contains(source, 'wallY = 5.0;'));
asserts.istrue(contains(source, 'wallHalfWidth = 6.0;'));
asserts.istrue(contains(source, "'Clipping', 'off'"));
asserts.istrue(contains(source, "'Style', 'togglebutton'"));
asserts.istrue(contains(source, "'String', 'Start / Stop'"));
asserts.istrue(contains(source, "'String', 'Click to stop or resume'"));
asserts.isfalse(contains(source, '60+ FPS target'));
asserts.istrue(contains(source, ...
  'paused = get(animationState.runToggle, ''Value'') ~= 0;'));
asserts.istrue(contains(source, '''String'', ''Stopped'''));
asserts.istrue(contains(source, 'measuredFps = renderedFrames / fpsWindowDuration;'));
asserts.istrue(contains(source, 'longitude = linspace(0, 2 * pi, 49);'));
asserts.istrue(contains(source, '''CDataMapping'', ''direct'''));
asserts.istrue(contains(source, '''FaceLighting'', ''gouraud'''));
asserts.istrue(contains(source, '''AmbientStrength'', 0.34'));
asserts.isequal(count(source, "light(axesHandle"), 2);
asserts.isfalse(contains(source, 'BOING_FPS'));
asserts.istrue(contains(source, "'XData', wallGridX"));
asserts.istrue(contains(source, "'XData', floorGridX"));
asserts.istrue(contains(source, 'wallGridPlane = wallY - 0.018;'));

manifest = jsondecode(fileread(fullfile(exampleRoot, 'index.json')));
exampleFiles = strings(1, numel(manifest.examples));
for exampleIndex = 1:numel(manifest.examples)
  exampleFiles(exampleIndex) = manifest.examples(exampleIndex).file;
end
asserts.istrue(any(exampleFiles == "boing_ball_3d.m"));
%=============================================================================
