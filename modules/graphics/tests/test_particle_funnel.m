%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
exampleRoot = fullfile(modulepath('graphics'), 'examples');
examplePath = fullfile(exampleRoot, 'particle_funnel.m');
asserts.istrue(isfile(examplePath));
asserts.isequal(parsefile(examplePath), 'script');

source = fileread(examplePath);
asserts.istrue(contains(source, 'targetFrameRate = 60;'));
asserts.istrue(contains(source, 'targetFrameDuration = 1 / targetFrameRate;'));
asserts.istrue(contains(source, 'physicsStepsPerFrame = 1;'));
asserts.istrue(contains(source, 'funnelParticleCount = 500;'));
asserts.istrue(contains(source, 'funnelParticleContactForces'));
asserts.istrue(contains(source, 'funnelWallForces'));
asserts.istrue(contains(source, 'funnelObstacleObjects'));
asserts.istrue(contains(source, 'funnelObstacleForces'));
asserts.istrue(contains(source, 'funnelFigurePosition'));
asserts.istrue(contains(source, 'funnelPauseResponsive(startStopButton'));
asserts.istrue(contains(source, '''CData'', particleColor'));
asserts.istrue(contains(source, '''String'', ''Stop'''));
asserts.istrue(contains(source, 'funnelStartStopRequest'));
asserts.istrue(contains(source, 'funnelApplyControlRequest'));
asserts.istrue(contains(source, 'funnelSetButtonState(startStopButton, ''Start'')'));
asserts.istrue(contains(source, 'funnelPauseResponsive'));
asserts.istrue(contains(source, 'funnelRunAnimation'));
forbiddenFrameText = [int2str(60), ' FPS'];
asserts.isfalse(contains(source, forbiddenFrameText));

funnelFigureVisible = 'off';
funnelBatch = true;
funnelParticleCount = 20;
funnelFrameCount = 8;
funnelMaxFrames = 4;
run(examplePath);

assert_isequal(targetFrameRate, 60);
assert_isequal(physicsStepsPerFrame, 1);
assert_isequal(size(particleXHistory), [20 8]);
assert_isequal(size(particleYHistory), [20 8]);
assert_isequal(size(speedHistory), [20 8]);
assert_isequal(size(impactHistory), [20 8]);
assert_istrue(all(isfinite(particleXHistory(:))));
assert_istrue(all(isfinite(particleYHistory(:))));
assert_istrue(all(isfinite(speedHistory(:))));
assert_istrue(all(isfinite(impactHistory(:))));
renderedColumns = 1:funnelMaxFrames;
renderedY = particleYHistory(:, renderedColumns);
assert_istrue(min(renderedY(:)) > 0);
assert_istrue(max(renderedY(:)) <= 8.0);
assert_isequal(numel(objectX), 5);
assert_isequal(numel(objectY), 5);
assert_isequal(numel(objectRadius), 5);
assert_istrue(isgraphics(figureHandle));
assert_istrue(isgraphics(particleHandle));
assert_istrue(isgraphics(shadowHandle));
assert_istrue(isgraphics(startStopButton));
assert_isequal(startStopButton.Style, 'pushbutton');
assert_isequal(startStopButton.String, 'Start');
assert_isequal(startStopButton.UserData, 'none');
feval(startStopButton.Callback, startStopButton, []);
assert_isequal(startStopButton.UserData, 'start');
set(startStopButton, 'String', 'Stop', 'UserData', 'none');
feval(startStopButton.Callback, startStopButton, []);
assert_isequal(startStopButton.UserData, 'stop');
assert_isequal(numel(particleHandle.XData), funnelParticleCount);
assert_isequal(numel(shadowHandle.XData), funnelParticleCount);
screenSize = get(0, 'ScreenSize');
if screenSize(3) >= 640 && screenSize(4) >= 520
  % ScreenSize may be reported in physical pixels while the figure Position is in
  % logical pixels (or the reverse) on high-DPI displays, so the raw numbers do
  % not share a scale. Bound the figure by the screen scaled by the device pixel
  % ratio: this still catches an absurdly oversized figure while tolerating the
  % physical/logical mismatch on scaled displays.
  screenScale = max(1, figureHandle.DevicePixelRatio);
  assert_istrue(figureHandle.Position(3) <= screenSize(3) * screenScale);
  assert_istrue(figureHandle.Position(4) <= screenSize(4) * screenScale);
end
delete(figureHandle);
drawnow();

manifest = jsondecode(fileread(fullfile(exampleRoot, 'index.json')));
exampleFiles = strings(1, numel(manifest.examples));
for exampleIndex = 1:numel(manifest.examples)
  exampleFiles(exampleIndex) = manifest.examples(exampleIndex).file;
end
asserts.istrue(any(exampleFiles == "particle_funnel.m"));
asserts.isfalse(contains(jsonencode(manifest), forbiddenFrameText));
%=============================================================================
