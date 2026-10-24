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
examplePath = fullfile(exampleRoot, 'fluid_simulation_2d.m');
assert_istrue(isfile(examplePath));
assert_isequal(parsefile(examplePath), 'script');

fluidFigureVisible = 'off';
fluidFrameCount = 60;
fluidGridSize = 32;
fluidJacobiIterations = 20;
fluidAutoStart = true;
run(examplePath);

assert_isequal(frameIndex, 60);
assert_isequal(fluid.time, 60);
assert_isequal(size(fluid.vx), [32 64]);
assert_isequal(size(fluid.vy), [32 64]);
assert_isequal(size(fluid.dye), [32 64]);
assert_istrue(all(isfinite(fluid.vx(:))));
assert_istrue(all(isfinite(fluid.vy(:))));
% The limited MacCormack correction creates no new extrema.
assert_istrue(min(fluid.dye(:)) >= 0);
assert_istrue(max(fluid.dye(:)) <= 1);
assert_istrue(max(hypot(fluid.vx(:), fluid.vy(:))) <= 1.5 * fluid.inflowSpeed);
% The jet carries dye downstream of the nozzle.
downstream = fluid.dye(:, max(fluid.inflowCols) + 10:end);
assert_istrue(max(downstream(:)) > 0.1);
% The default obstacle is a vertical plate: no flow and no dye inside.
assert_isequal(fluid.obstacleName, 'Vertical plate');
plateHalfSize = [1, 32 / 8];
assert_isequal(nnz(fluid.solid), nnz(abs(fluid.X - 64 / 3) <= plateHalfSize(1) & ...
  abs(fluid.Y - 16) <= plateHalfSize(2)));
assert_isequal(fluid.vx(fluid.solid), zeros(nnz(fluid.solid), 1));
assert_isequal(fluid.vy(fluid.solid), zeros(nnz(fluid.solid), 1));
assert_isequal(fluid.dye(fluid.solid), zeros(nnz(fluid.solid), 1));
assert_isequal(size(obstacleHandle.XData), [4 1]);
assert_isequal(char(obstacleHandle.Visible), 'on');
assert_istrue(isgraphics(figureHandle));
% The dye is rendered as a true-colour image.
assert_isequal(size(imageHandle.CData), [32 64 3]);
assert_istrue(min(imageHandle.CData(:)) >= 0 && max(imageHandle.CData(:)) <= 1);
assert_isequal(numel(fluid.jets), 1);
assert_isequal(jetsCheckbox.Style, 'checkbox');
assert_isequal(jetsCheckbox.Value, 0);

% Obstacle list: every entry, in the order of the menu.
obstacleList = {'None', 'Square', 'Vertical plate', 'Horizontal plate', 'Disc', ...
  'Airfoil', 'Wedge', 'Cylinder array', 'Rotating propeller'};
assert_isequal(obstacleMenu.Style, 'popupmenu');
assert_isequal(obstacleMenu.String(:), obstacleList(:));
assert_isequal(obstacleMenu.Value, 3);

% Start/Reset button: the running simulation offers Reset.
assert_isequal(startResetButton.Style, 'pushbutton');
assert_isequal(startResetButton.String, 'Reset');
assert_isequal(startResetButton.UserData, 'none');
feval(startResetButton.Callback, startResetButton, []);
assert_isequal(startResetButton.UserData, 'reset');
set(startResetButton, 'String', 'Start', 'UserData', 'none');
feval(startResetButton.Callback, startResetButton, []);
assert_isequal(startResetButton.UserData, 'start');
delete(figureHandle);

% Colour modes: smoke by default, vorticity views on demand.
run(examplePath);
colorModeList = {'Smoke', 'Vorticity', 'Smoke + vorticity'};
assert_isequal(colorMenu.Style, 'popupmenu');
assert_isequal(colorMenu.String(:), colorModeList(:));
assert_isequal(colorMenu.Value, 1);
% Smoke: the background shows where there is no dye, jet 1 glows to white.
background = reshape([0.02 0.03 0.06], 1, 1, 3);
clearCells = fluid.dye(:, :, 1) == 0;
for channel = 1:3
  plane = imageHandle.CData(:, :, channel);
  assert_istrue(all(abs(plane(clearCells) - background(channel)) < 1e-12));
end
assert_isequal(fluid.solidIndex, find(fluid.solid));
delete(figureHandle);

for colorIndex = 2:numel(colorModeList)
  fluidColorMode = colorModeList{colorIndex};
  run(examplePath);
  assert_isequal(colorMenu.Value, colorIndex);
  rgb = imageHandle.CData;
  assert_isequal(size(rgb), [32 64 3]);
  assert_istrue(min(rgb(:)) >= 0 && max(rgb(:)) <= 1);
  % Both senses of rotation show up in the wake: red (counter-clockwise)
  % and blue (clockwise).
  assert_istrue(nnz(rgb(:, :, 1) > rgb(:, :, 3) + 0.2) > 0);
  assert_istrue(nnz(rgb(:, :, 3) > rgb(:, :, 1) + 0.2) > 0);
  delete(figureHandle);
end
% The open border shears the flow: it is not drawn as vorticity.
fluidColorMode = 'Vorticity';
run(examplePath);
border = [imageHandle.CData(1:2, :, 1), imageHandle.CData(end - 1:end, :, 1)];
assert_istrue(all(abs(border(:) - 0.02) < 1e-12));
delete(figureHandle);
fluidColorMode = 'Unknown colours';
try
  run(examplePath);
  errorId = '';
catch caughtError
  errorId = caughtError.identifier;
end
assert_isequal(errorId, 'Nelson:fluid:unknownColorMode');
fluidColorMode = 'Smoke';

% Fine smoke: on by default; turned off, the dye stays bounded but blurs.
run(examplePath);
assert_isequal(fineSmokeCheckbox.Style, 'checkbox');
assert_isequal(fineSmokeCheckbox.Value, 1);
assert_istrue(fluid.fineSmoke);
sharpDye = fluid.dye(:, :, 1);
delete(figureHandle);
fluidFineSmoke = false;
run(examplePath);
assert_isequal(fineSmokeCheckbox.Value, 0);
assert_isfalse(fluid.fineSmoke);
softDye = fluid.dye(:, :, 1);
assert_istrue(min(softDye(:)) >= 0 && max(softDye(:)) <= 1);
sharpness = @(d) mean(abs(reshape(diff(d, 1, 1), [], 1))) + ...
  mean(abs(reshape(diff(d, 1, 2), [], 1)));
assert_istrue(sharpness(softDye) < sharpness(sharpDye));
delete(figureHandle);
fluidFineSmoke = true;

% Two jets: one dye layer each, both carried downstream.
fluidJetCount = 2;
run(examplePath);
assert_isequal(numel(fluid.jets), 2);
assert_isequal(size(fluid.dye), [32 64 2]);
assert_isequal(jetsCheckbox.Value, 1);
assert_istrue(min(fluid.dye(:)) >= 0 && max(fluid.dye(:)) <= 1);
for jetIndex = 1:2
  layer = fluid.dye(:, max(fluid.inflowCols) + 10:end, jetIndex);
  assert_istrue(max(layer(:)) > 0.1);
end
% The jets enter on opposite sides of the axis, aimed at each other.
assert_istrue(mean(fluid.jets(1).rows) > 16 && mean(fluid.jets(2).rows) < 16);
assert_istrue(fluid.jets(1).aim < 0 && fluid.jets(2).aim > 0);
delete(figureHandle);
fluidJetCount = 1;

% No obstacle: nothing solid and no outline drawn.
fluidObstacle = 'None';
fluidFrameCount = 12;
run(examplePath);
assert_isequal(nnz(fluid.solid), 0);
assert_isequal(char(obstacleHandle.Visible), 'off');
assert_istrue(all(isfinite(fluid.vx(:))));
delete(figureHandle);

% Every other obstacle is a non-empty solid, the fluid moves with it, and
% its outline encloses it.
for obstacleIndex = 2:numel(obstacleList)
  fluidObstacle = obstacleList{obstacleIndex};
  run(examplePath);
  assert_isequal(fluid.obstacleName, obstacleList{obstacleIndex});
  assert_isequal(obstacleMenu.Value, obstacleIndex);
  assert_istrue(nnz(fluid.solid) >= 4);
  assert_istrue(all(isfinite(fluid.vx(:))) && all(isfinite(fluid.vy(:))));
  assert_isequal(fluid.vx(fluid.solid), fluid.solidVx(fluid.solid));
  assert_isequal(fluid.vy(fluid.solid), fluid.solidVy(fluid.solid));
  assert_isequal(fluid.dye(fluid.solid), zeros(nnz(fluid.solid), 1));
  assert_isequal(obstacleHandle.XData, fluid.outlineX);
  assert_isequal(obstacleHandle.YData, fluid.outlineY);
  assert_istrue(min(fluid.outlineX(:)) <= min(fluid.X(fluid.solid)));
  assert_istrue(max(fluid.outlineX(:)) >= max(fluid.X(fluid.solid)));
  assert_isequal(char(obstacleHandle.Visible), 'on');
  delete(figureHandle);
end
assert_isequal(size(fluid.outlineX, 2), 1);
% The propeller spins: rigid rotation inside, not at rest.
assert_istrue(fluid.obstacleMoving);
assert_istrue(max(abs(fluid.vx(fluid.solid))) > 0);
fluidObstacle = 'Cylinder array';
fluidFrameCount = 1;
run(examplePath);
assert_isequal(size(fluid.outlineX, 2), 3);
assert_isfalse(fluid.obstacleMoving);
delete(figureHandle);

fluidObstacle = 'Unknown shape';
try
  run(examplePath);
  errorId = '';
catch caughtError
  errorId = caughtError.identifier;
end
assert_isequal(errorId, 'Nelson:fluid:unknownObstacle');

manifest = jsondecode(fileread(fullfile(exampleRoot, 'index.json')));
exampleFiles = strings(1, numel(manifest.examples));
for exampleIndex = 1:numel(manifest.examples)
  exampleFiles(exampleIndex) = manifest.examples(exampleIndex).file;
end
assert_istrue(any(exampleFiles == "fluid_simulation_2d.m"));
%=============================================================================
