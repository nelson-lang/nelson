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
% <--WEIGHT 2-->
%=============================================================================
exampleRoot = fullfile(modulepath('graphics'), 'examples');
examplePath = fullfile(exampleRoot, 'retro_raycaster.m');
assert_istrue(isfile(examplePath));
assert_isequal(parsefile(examplePath), 'script');

raycasterFigureVisible = 'off';
raycasterBatch = true;
raycasterMaxFrames = 12;
raycasterWidth = 96;
raycasterHeight = 72;
raycasterAutopilot = true;
raycasterStartFullscreen = false;
run(examplePath);

assert_isequal(raycasterFrameIndex, raycasterMaxFrames);
assert_isequal(size(raycasterFrame), [raycasterHeight raycasterWidth 3]);
assert_isequal(size(raycasterRayDistance), [1 raycasterWidth]);
assert_istrue(all(isfinite(raycasterFrame(:))));
assert_istrue(all(isfinite(raycasterRayDistance)));
assert_istrue(all(raycasterRayDistance > 0));
assert_istrue(min(raycasterFrame(:)) >= 0);
assert_istrue(max(raycasterFrame(:)) <= 1);
assert_istrue(raycasterPlayerX > 3.5);
assert_isequal(raycasterWorldMap(floor(raycasterPlayerY), ...
  floor(raycasterPlayerX)), 0);
assert_isequal(size(raycasterTextures), [256 256 3 4]);
assert_isequal(size(raycasterSpriteColor), [768 512 3]);
assert_isequal(size(raycasterSpriteAlpha), [768 512]);
assert_isequal(size(raycasterWeaponColor), [768 512 3]);
assert_isequal(size(raycasterWeaponAlpha), [768 512]);
assert_istrue(any(raycasterSpriteAlpha(:) < 1));
assert_istrue(any(raycasterWeaponAlpha(:) < 1));
assert_istrue(isgraphics(figureHandle));
assert_istrue(isgraphics(imageHandle, 'image'));
assert_isequal(numel(raycasterEnemyHandles), size(raycasterEnemies, 1));
for enemyIndex = 1:numel(raycasterEnemyHandles)
  assert_istrue(isgraphics(raycasterEnemyHandles{enemyIndex}, 'image'));
end
assert_istrue(isgraphics(raycasterWeaponHandle, 'image'));
assert_isequal(axesHandle.XLimitMethod, 'tight');
assert_isequal(axesHandle.YLimitMethod, 'tight');
assert_istrue(isgraphics(fpsText, 'text'));
assert_isequal(fpsText.Tag, 'raycaster-fps');
assert_isequal(char(figureHandle.Resize), 'on');
assert_istrue(isa(figureHandle.WindowKeyPressFcn, 'function_handle'));
assert_istrue(isa(figureHandle.WindowKeyReleaseFcn, 'function_handle'));
% The demo owns the scroll wheel so the figure hosts keep their axes zoom off:
% the callback is set, does nothing, and the web display list reports it.
assert_istrue(isa(figureHandle.WindowScrollWheelFcn, 'function_handle'));
stateBeforeScroll = get(figureHandle, 'UserData');
scrollEvent = struct('VerticalScrollCount', 1, 'VerticalScrollAmount', 3);
feval(figureHandle.WindowScrollWheelFcn, figureHandle, scrollEvent);
assert_isequal(get(figureHandle, 'UserData'), stateBeforeScroll);
assert_isequal(axesHandle.XLim, [0.5 raycasterWidth + 0.5]);
assert_isequal(axesHandle.YLim, [0.5 raycasterHeight + 0.5]);
webFrame = jsondecode(__web_display_list__(figureHandle));
assert_istrue(webFrame.hasScrollWheelCallback);
keyEvent = struct('Key', 'W', 'Character', 'w');
feval(figureHandle.WindowKeyPressFcn, figureHandle, keyEvent);
callbackState = get(figureHandle, 'UserData');
assert_istrue(callbackState.forward);
feval(figureHandle.WindowKeyReleaseFcn, figureHandle, keyEvent);
callbackState = get(figureHandle, 'UserData');
assert_isfalse(callbackState.forward);
escapeEvent = struct('Key', 'Escape', 'Character', '');
feval(figureHandle.WindowKeyPressFcn, figureHandle, escapeEvent);
callbackState = get(figureHandle, 'UserData');
assert_istrue(callbackState.quit);
textKeyEvent = struct('Key', 'D', 'Character', 'd');
feval(statusText.KeyPressFcn, statusText, textKeyEvent);
callbackState = get(figureHandle, 'UserData');
assert_istrue(callbackState.strafeRight);
feval(statusText.KeyReleaseFcn, statusText, textKeyEvent);
callbackState = get(figureHandle, 'UserData');
assert_isfalse(callbackState.strafeRight);
fullscreenEvent = struct('Key', 'F', 'Character', 'f');
feval(figureHandle.WindowKeyPressFcn, figureHandle, fullscreenEvent);
assert_isequal(figureHandle.WindowState, 'fullscreen');
assert_isequal(char(statusText.Visible), 'off');
feval(figureHandle.WindowKeyPressFcn, figureHandle, fullscreenEvent);
assert_isequal(figureHandle.WindowState, 'fullscreen');
feval(figureHandle.WindowKeyReleaseFcn, figureHandle, fullscreenEvent);
feval(figureHandle.WindowKeyPressFcn, figureHandle, fullscreenEvent);
assert_isequal(figureHandle.WindowState, 'normal');
assert_isequal(char(statusText.Visible), 'on');
delete(figureHandle);
drawnow();

manifest = jsondecode(fileread(fullfile(exampleRoot, 'index.json')));
exampleFiles = strings(1, numel(manifest.examples));
for exampleIndex = 1:numel(manifest.examples)
  exampleFiles(exampleIndex) = manifest.examples(exampleIndex).file;
end
assert_istrue(any(exampleFiles == "retro_raycaster.m"));
%=============================================================================
