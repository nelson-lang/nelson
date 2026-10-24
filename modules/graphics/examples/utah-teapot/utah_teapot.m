%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
teapot_directory = [modulepath('graphics', 'root'), '/examples/utah-teapot/'];
load([teapot_directory, 'teapot.nh5']);
f = figure();
f.DrawLater = 'on';
axis off
axis([-3 3 -3 3 -3 5]);
axis('square');
p = patch('Faces', teapotFaces, 'Vertices', teapotVertices, 'FaceColor', 'none');
view(0, 360);
f.DrawLater = 'off';
for k = 0:9:720
  view(k, 360);
  % Pace the loop and flush the repaint: without it the Qt backend
  % coalesces the 81 view() updates into one or two paints and the
  % teapot appears to jump straight to the final angle.
  drawnow('limitrate');
end
set(gca(), ...
  'XLim', [-3 3], ...
  'YLim', [-3 3], ...
  'ZLim', [-3 5], ...
  'XTick', -3:1:3, ...
  'YTick', [-2 0 2], ...
  'ZTick', -3:1:5, ...
  'YTickLabel', {'0'; '0.1'; '0.2'; '0.3'; '0.4'; '0.5'; '0.6'; '0.7'; '0.8'; '0.9'; '1'});
%=============================================================================
