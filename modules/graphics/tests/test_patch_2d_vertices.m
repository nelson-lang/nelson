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
% 2-D patches keep their data as given: no Z coordinate is invented (ZData empty,
% Vertices N-by-2), triangles are not closed by repeating the first point, and
% without color data the color properties keep their defaults and auto modes.
%=============================================================================
gray = [33 33 33] / 255;
f = figure('Visible', 'off');
ax = axes(f);
%=============================================================================
p = patch(ax, [0 1 1], [0 0 1], 'r');
assert_isequal(p.XData, [0; 1; 1]);
assert_isequal(p.YData, [0; 0; 1]);
assert_istrue(isempty(p.ZData));
assert_isequal(p.Vertices, [0 0; 1 0; 1 1]);
assert_isequal(p.Faces, [1 2 3]);
assert_isequal(p.FaceColor, [1 0 0]);
assert_istrue(max(abs(p.FaceVertexCData - gray)) < 1e-4);
assert_isequal(p.CDataMode, 'auto');
assert_isequal(p.FaceVertexCDataMode, 'auto');
%=============================================================================
p = patch([0 1 1], [0 0 1], 'r');
assert_isequal(p.XData, [0; 1; 1]);
assert_istrue(isempty(p.ZData));
assert_isequal(p.Vertices, [0 0; 1 0; 1 1]);
%=============================================================================
p = patch();
assert_isequal(p.XData, [0; 1; 0]);
assert_isequal(p.YData, [1; 1; 0]);
assert_istrue(isempty(p.ZData));
assert_isequal(p.Vertices, [0 1; 1 1; 0 0]);
%=============================================================================
p = patch([0 1 0], [0 0 1], 'red');
p.XData = [1 2 1];
p.YData = [1 1 2];
drawnow();
assert_isequal(p.Vertices, [1 1; 2 1; 1 2]);
%=============================================================================
p = patch('Faces', [1 2 3], 'Vertices', [0 0; 1 0; 1 1]);
assert_isequal(p.Vertices, [0 0; 1 0; 1 1]);
%=============================================================================
p = patch([0 1 1], [0 0 1], [2 2 2], 'g');
assert_isequal(p.ZData, [2; 2; 2]);
assert_isequal(p.Vertices, [0 0 2; 1 0 2; 1 1 2]);
%=============================================================================
p = patch([0 1 1 0; 0 1 1 0]', [0 0 1 1; 1 1 2 2]', 'b');
assert_isequal(p.Vertices, [0 0; 1 0; 1 1; 0 1; 0 1; 1 1; 1 2; 0 2]);
assert_isequal(p.Faces, [1 2 3 4; 5 6 7 8]);
assert_istrue(isempty(p.ZData));
%=============================================================================
delete(f);
%=============================================================================
