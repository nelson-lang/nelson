%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
p = patch('Faces', teapotFaces, 'Vertices', teapotVertices, 'FaceColor','none');
view(0, 360);
f.DrawLater = 'off';
for k = 0:9:720
  view(k, 360);
end
%=============================================================================
