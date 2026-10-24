%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--WEB GUI REQUIRED--> run with: nelson-webview --file runtests_web.m
%=============================================================================
% drawnow-driven animations must complete headless: drawNow paces web frames
% to ~60 fps (and streams them when a browser is attached), so a short comet
% takes a bounded, non-zero time and a drawnow loop terminates cleanly.
f = figure();
t = 0:0.2:6.28; % ~32 points -> ~1s paced
tic;
comet(t, sin(t));
elapsed = toc;
assert_istrue(elapsed < 30);
close(f);
%=============================================================================
g = figure();
tic;
for k = 1:10
  plot(1:k);
  drawnow();
end
elapsed = toc;
assert_istrue(elapsed < 15);
close(g);
%=============================================================================
% Repeated image updates must publish the latest encoded CData frame.
imageFigure = figure();
imageAxes = axes('Parent', imageFigure, 'Position', [0 0 1 1]);
imageData = zeros(18, 24, 3);
imageHandle = image('Parent', imageAxes, 'CData', imageData);
axis(imageAxes, 'image');
axis(imageAxes, 'off');
imageData(:, :, 1) = 1;
set(imageHandle, 'CData', imageData);
drawnow();
% A stateless display list carries the raw RGBA payload (not an inline PNG
% data URI): each full render ships the pixels so a client that never saw the
% earlier frame still paints the latest CData.
redFrame = jsondecode(__web_display_list__(imageFigure));
redPrimitive = last_primitive(redFrame);
assert_isequal(redPrimitive.kind, 'image');
assert_istrue(isfield(redPrimitive, 'rgba'));
assert_isequal(redPrimitive.imageWidth, 24);
assert_isequal(redPrimitive.imageHeight, 18);
redImage = redPrimitive.rgba;
imageData(:, :, 1) = 0;
imageData(:, :, 3) = 1;
set(imageHandle, 'CData', imageData);
drawnow();
blueFrame = jsondecode(__web_display_list__(imageFigure));
bluePrimitive = last_primitive(blueFrame);
assert_isequal(bluePrimitive.kind, 'image');
assert_istrue(isfield(bluePrimitive, 'rgba'));
blueImage = bluePrimitive.rgba;
assert_istrue(~isempty(redImage) && ~isempty(blueImage));
assert_isfalse(strcmp(redImage, blueImage));
close(imageFigure);
%=============================================================================
% View changes stream frames directly in web mode. User scripts do not need to
% insert explicit refresh calls into rotation loops.
h = figure();
vertices = [-1 -1 0; 1 -1 0; 1 1 0; -1 1 0];
patch('Faces', [1 2 3 4], 'Vertices', vertices, 'FaceColor', 'none');
axis('square');
tic;
for k = 1:8
  view(k * 15, 30);
end
elapsed = toc;
assert_istrue(elapsed < 10);
close(h);
%=============================================================================
% A dense retained patch must survive a full rotation without an explicit
% refresh call or a backend disconnect.
demoDirectory = [modulepath('graphics', 'root'), '/examples/utah-teapot/'];
load([demoDirectory, 'teapot.nh5']);
h = figure('DrawLater', 'on');
axis off;
axis([-3 3 -3 3 -3 5]);
axis('square');
patch('Faces', teapotFaces, 'Vertices', teapotVertices, 'FaceColor', 'none');
view(0, 360);
h.DrawLater = 'off';
tic;
for k = 0:9:720
  view(k, 360);
end
elapsed = toc;
assert_istrue(elapsed < 8);
dl = jsondecode(__web_display_list__(h));
assert_istrue(isfield(dl, 'webglMeshes'));
assert_istrue(dl.webglMeshes(1).positionsLength > 1000);
assert_istrue(length(dl.webglMeshes(1).positionsBase64) > 1000);
close(h);
%=============================================================================
% sleep must really delay (haveEventsLoop pumping)
tic;
sleep(0.5);
elapsed = toc;
assert_istrue(elapsed >= 0.4);
%=============================================================================
function primitive = last_primitive(dl)
  primitives = dl.primitives;
  if iscell(primitives)
    primitive = primitives{end};
  else
    primitive = primitives(end);
  end
end
%=============================================================================
