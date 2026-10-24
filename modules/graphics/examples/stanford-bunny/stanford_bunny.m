%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tic();
bunny_directory = [modulepath('graphics', 'root'), '/examples/stanford-bunny/'];
load([bunny_directory, 'stanford-bunny.nh5']);
f = figure('Visible', 'off', 'DrawLater', 'on', 'Color', [1, 1, 1]);
patch('Faces', Faces, 'Vertices', Vertices, 'FaceVertexCData', Colors, ...
  'EdgeColor', 'white', ...
  'FaceColor', 'interp', 'FaceAlpha', 1);
axis equal
axis off
view([0, 0, 1]);
set(gca(), ...
  'XLim', [-0.130946429447853 0.0972744294478528], ...
  'YLim', [0.020116 0.200116], ...
  'ZLim', [-0.061852 0.058783], ...
  'XTick', -0.12:0.02:0.08, ...
  'YTick', 0.04:0.02:0.2, ...
  'ZTick', [-0.05 0 0.05]);
f.DrawLater = 'off';
f.Visible = 'on';
toc();
%=============================================================================
