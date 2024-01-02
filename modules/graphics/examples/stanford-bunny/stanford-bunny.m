%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
'EdgeColor', 'none', ...
'FaceColor', 'interp', 'FaceAlpha', 1);
axis equal
axis off 
view([0, 0, 1]);
f.DrawLater = 'off';
f.Visible = 'on';
toc();
%=============================================================================