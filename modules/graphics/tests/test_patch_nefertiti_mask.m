%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
nefertiti_directory = [modulepath('graphics', 'root'), '/examples/nefertiti-mask/'];
load([nefertiti_directory, 'nefertiti-mask.nh5']);
f = figure('Visible', 'off', 'Color', [1 1 1], 'Position', [100 100 240 240]);
patch('Faces', Faces, 'Vertices', Vertices, 'FaceVertexCData', Colors, ...
  'EdgeColor', 'none', 'FaceColor', 'interp', 'FaceAlpha', 1);
axis equal
axis off
view([0, 0, 1]);
drawnow();
frameNoEdges = getframe(f).cdata;
objectMask = frameNoEdges(:, :, 1) < 245 | frameNoEdges(:, :, 2) < 245 | frameNoEdges(:, :, 3) < 245;
objectPixels = nnz(objectMask);
asserts.istrue(objectPixels > numel(objectMask) * 0.02);
%=============================================================================
