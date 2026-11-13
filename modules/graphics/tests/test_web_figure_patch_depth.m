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
% web figure: a translucent 2D patch sent as a GPU mesh lies strictly inside the
% clip depth range, so float32 rounding cannot clip it on the near plane
f = figure('Visible', 'off', 'Position', [100 100 800 500]);
t0 = 738450.6;
x = t0 + linspace(0, 0.8, 300);
ax = axes(f, 'Position', [0.07 0.1 0.9 0.3]);
plot(ax, x, sin(40 * x));
hold(ax, 'on');
patch(ax, t0 + [0.2 0.6 0.6 0.2], [-1 -1 1 1], [0.2 0.45 1], 'FaceAlpha', 0.25, ...
  'EdgeColor', [0.1 0.3 0.9]);
set(ax, 'XLim', t0 + [0 0.8], 'YLim', [-1 1]);
drawnow();
dl = jsondecode(__web_display_list__(f));
assert_istrue(isfield(dl, 'webglMeshes'));
meshes = dl.webglMeshes;
if iscell(meshes)
  meshes = [meshes{:}];
end
assert_isequal(numel(meshes), 1);
m = meshes(1);
transform = reshape(m.projection, 4, 4) * reshape(m.model, 4, 4);
box = m.positionBBox(:)';
for corner = [box(1:3); box(4:6)]'
  clip = transform * [corner; 1];
  ndc = clip(1:3) / clip(4);
  assert_istrue(abs(ndc(3)) < 1 - 1e-6);
  assert_istrue(all(abs(ndc(1:2)) <= 1 + 1e-9));
end
%=============================================================================
% a 17 s wide band on a date axis keeps its place and width: float32 positions
% are sent relative to the mesh origin
ax.XLim = t0 + [0.4 0.4 + 60 / 86400];
band = t0 + 0.4 + [20 37] / 86400;
p = findobj(ax, 'Type', 'patch');
set(p, 'XData', band([1 2 2 1]));
drawnow();
dl = jsondecode(__web_display_list__(f));
meshes = dl.webglMeshes;
if iscell(meshes)
  meshes = [meshes{:}];
end
m = meshes(1);
transform = reshape(m.projection, 4, 4) * reshape(m.model, 4, 4);
box = m.positionBBox(:)';
left = transform * [box(1:3)'; 1];
right = transform * [box(4:6)'; 1];
expected = 2 * (band - ax.XLim(1)) / diff(ax.XLim) - 1;
assert_isapprox([left(1) / left(4), right(1) / right(4)], expected, 1e-3);
close(f);
%=============================================================================
