%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% A patch parented to an hgtransform (or hggroup) resolves its colormap, CLim
% and default colour on the ancestor axes: the wrapper used to read
% Parent.Color and raised "Unrecognized property 'Color' for class
% 'hgtransform'".
f = figure('Visible', 'off');
ax = axes('Parent', f);
t = hgtransform('Parent', ax);
p = patch('Parent', t, 'XData', [0 1 1 0], 'YData', [0 0 1 1], 'FaceColor', 'r', 'EdgeColor', 'none');
asserts.isequal(p.Parent, t);
asserts.istrue(any(t.Children == p));
asserts.isequal(ancestor(p, 'axes'), ax);
% no CData at all: the default colour comes from the ancestor axes.
p2 = patch('Parent', t, 'XData', [0 1 1 0], 'YData', [1 1 2 2]);
asserts.isequal(p2.Parent, t);
% scalar CData per vertex (four values: three would be an RGB triple): mapped
% through the axes colormap and CLim.
colormap(ax, jet(16));
p3 = patch([2 3 3 2], [0 0 1 1], [0.1 0.5 0.9 0.3], 'Parent', t);
asserts.isequal(p3.Parent, t);
asserts.isequal(size(p3.FaceVertexCData, 2), 3);
asserts.isequal(size(p3.FaceVertexCData, 1), 4);
close(f);
%=============================================================================
% hggroup parent takes the same path.
f = figure('Visible', 'off');
ax = axes('Parent', f);
g = hggroup('Parent', ax);
p = patch('Parent', g, 'XData', [0 1 1 0], 'YData', [0 0 1 1]);
asserts.isequal(p.Parent, g);
p = patch([0 1 1 0], [0 0 1 1], [0.2 0.4 0.6 0.8], 'Parent', g);
asserts.isequal(p.Parent, g);
close(f);
%=============================================================================
% Rendering: the transform Matrix moves the patch. A red square at x in [-2 0]
% translated by +5 must move its pixel centroid to the right.
f = figure('Visible', 'off');
ax = axes('Parent', f);
axis(ax, [-10 10 -10 10]);
axis(ax, 'manual');
t = hgtransform('Parent', ax);
patch('Parent', t, 'XData', [-2 0 0 -2], 'YData', [-2 -2 2 2], 'FaceColor', 'r', 'EdgeColor', 'none');
cx0 = localRedCentroidX(getframe(f));
set(t, 'Matrix', makehgtform('translate', [5 0 0]));
cx1 = localRedCentroidX(getframe(f));
asserts.istrue(~isnan(cx0));
asserts.istrue(~isnan(cx1));
asserts.istrue((cx1 - cx0) > 0.125 * size(getframe(f).cdata, 2));
close(f);
%=============================================================================
function cx = localRedCentroidX(frame)
  c = frame.cdata;
  mask = (c(:, :, 1) > 150) & (c(:, :, 2) < 100) & (c(:, :, 3) < 100);
  [~, cols] = find(mask);
  if isempty(cols)
    cx = NaN;
  else
    cx = mean(cols);
  end
end
%=============================================================================
