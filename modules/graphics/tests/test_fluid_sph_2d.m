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
examplePath = fullfile(exampleRoot, 'fluid_sph_2d.m');
assert_istrue(isfile(examplePath));
assert_isequal(parsefile(examplePath), 'script');

sphFigureVisible = 'off';
sphBatch = true;
sphFrameCount = 120;
sphParticleCount = 240;
sphAutoStart = true;
sphGravity = 'Down';
run(examplePath);

% The batch run advances every requested frame and stays finite.
assert_isequal(frameIndex, 120);
assert_isequal(sph.frame, 120);
assert_isequal(sph.count, 240);
assert_isequal(size(sph.x), [240 1]);
assert_isequal(size(sph.y), [240 1]);
assert_isequal(size(sph.vx), [240 1]);
assert_isequal(size(sph.vy), [240 1]);
assert_istrue(all(isfinite(sph.x)) && all(isfinite(sph.y)));
assert_istrue(all(isfinite(sph.vx)) && all(isfinite(sph.vy)));

% The rest density comes from the initial packing and is strictly positive.
assert_istrue(sph.restDensity > 0);

% The tank is a valid box and every particle stays inside it after bouncing.
assert_isequal(size(sph.box), [1 4]);
assert_istrue(sph.box(2) > sph.box(1) && sph.box(4) > sph.box(3));
tolerance = 1e-9;
assert_istrue(all(sph.x >= sph.box(1) - tolerance & sph.x <= sph.box(2) + tolerance));
assert_istrue(all(sph.y >= sph.box(3) - tolerance & sph.y <= sph.box(4) + tolerance));

% Gravity was set to down for this run and the particles settle low.
assert_isequal(sph.gravityName, 'Down');
assert_isequal(sph.gravityDirection, [0 -1]);
assert_istrue(mean(sph.y) < mean(sph.box(3:4)));

% The particles are drawn as separate dots tinted by their speed.
assert_istrue(isgraphics(particleHandle));
assert_isequal(size(particleHandle.CData), [240 3]);
assert_istrue(min(particleHandle.CData(:)) >= 0 && max(particleHandle.CData(:)) <= 1);
assert_isequal(particleHandle.XData(:), sph.x);
assert_isequal(particleHandle.YData(:), sph.y);
assert_istrue(isgraphics(figureHandle));

% Controls: a start/stop and a reset push-button.
assert_isequal(startStopButton.Style, 'pushbutton');
assert_isequal(resetButton.Style, 'pushbutton');

% The start/stop button toggles its request; reset asks for a rebuild.
set(startStopButton, 'String', 'Start', 'UserData', 'none');
feval(startStopButton.Callback, startStopButton, []);
assert_isequal(startStopButton.UserData, 'start');
set(startStopButton, 'String', 'Stop');
feval(startStopButton.Callback, startStopButton, []);
assert_isequal(startStopButton.UserData, 'stop');
feval(resetButton.Callback, resetButton, []);
assert_isequal(resetButton.UserData, 'reset');

% The mouse callbacks arm and disarm the stirring force on the axes.
mouse = get(axesHandle, 'UserData');
assert_isfalse(mouse.active);
feval(figureHandle.WindowButtonDownFcn, figureHandle, []);
mouse = get(axesHandle, 'UserData');
assert_istrue(mouse.active);
feval(figureHandle.WindowButtonUpFcn, figureHandle, []);
mouse = get(axesHandle, 'UserData');
assert_isfalse(mouse.active);
delete(figureHandle);

% Gravity off keeps the particles spread through the tank; gravity down
% pulls their average height well below the middle.
sphGravity = 'Off';
run(examplePath);
assert_isequal(sph.gravityDirection, [0 0]);
restingMeanY = mean(sph.y);
delete(figureHandle);
sphGravity = 'Down';
run(examplePath);
fallenMeanY = mean(sph.y);
assert_istrue(fallenMeanY < restingMeanY);
delete(figureHandle);

% Sideways gravity drifts the particles towards a side wall.
sphGravity = 'Right';
run(examplePath);
assert_isequal(sph.gravityDirection, [1 0]);
assert_istrue(mean(sph.x) > mean(sph.box(1:2)));
delete(figureHandle);
sphGravity = 'Off';

% An unknown gravity name is rejected.
sphGravity = 'Sideways';
try
  run(examplePath);
  errorId = '';
catch caughtError
  errorId = caughtError.identifier;
end
assert_isequal(errorId, 'Nelson:sph:unknownGravity');
sphGravity = 'Down';

manifest = jsondecode(fileread(fullfile(exampleRoot, 'index.json')));
exampleFiles = strings(1, numel(manifest.examples));
for exampleIndex = 1:numel(manifest.examples)
  exampleFiles(exampleIndex) = manifest.examples(exampleIndex).file;
end
assert_istrue(any(exampleFiles == "fluid_sph_2d.m"));
%=============================================================================
