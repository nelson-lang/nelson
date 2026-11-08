%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A manual camera angle that zooms in makes the data box overflow the axes
% rectangle: the overflow is drawn (clip and mesh viewport grow), limits stay the clip.
%=============================================================================
% auto camera angle: the clip is the axes rectangle
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
ax = axes(f);
surf(ax, peaks(20));
drawnow;
% the canvas size comes from the display list (a headless runner may not honor Position)
[clips, W, H] = axesClips(f);
box = ax.Position .* [W H W H];
assert_isapprox(clips(1, 3), box(3), 1);
assert_isapprox(clips(1, 4), box(4), 1);
%=============================================================================
% zooming in with a manual angle: the clip grows beyond the axes rectangle
camva(ax, camva(ax) * 0.6);
drawnow;
clips = axesClips(f);
% the peaks box is height-bound: the clip grows to the frame height and never shrinks in width
assert_istrue(clips(1, 3) >= box(3) - 1);
assert_istrue(clips(1, 4) > box(4) + 50);
assert_istrue(clips(1, 1) >= 0 && clips(1, 2) >= 0);
assert_istrue(clips(1, 1) + clips(1, 3) <= W + 1 && clips(1, 2) + clips(1, 4) <= H + 1);
dl = jsondecode(__web_display_list__(f, false));
m = dl.webglMeshes;
if iscell(m)
  m = m{1};
else
  m = m(1);
end
assert_istrue(m.viewport(4) > box(4) + 50);
% zooming out never shrinks the clip below the axes rectangle
camva(ax, camva(ax) * 3);
drawnow;
clips = axesClips(f);
assert_isapprox(clips(1, 3), box(3), 1);
assert_isapprox(clips(1, 4), box(4), 1);
close(f);
%=============================================================================
% the angle frozen by vis3d on an 'axis equal' box spans the width: the knot stays inside
% the axes box after 'axis tight' and the title is visible, at any figure size
for figSize = {[859 517], [1074 646], [700 450]}
  f = figure('Visible', 'off', 'Position', [100 100 figSize{1}]);
  knot();
  axis equal
  title('Trefoil knot');
  drawnow;
  ax = gca;
  assert_isapprox(ax.CameraViewAngle, 8.52, 0.2);
  [clips, W, H] = axesClips(f);
  box = ax.Position .* [W H W H];
  assert_isapprox(clips(1, 3), box(3), 1);
  assert_isapprox(clips(1, 4), box(4), 1);
  fr = getframe(f);
  img = fr.cdata;
  bg = double(reshape(img(1, 1, :), 1, 3));
  mask = any(abs(double(img) - reshape(bg, 1, 1, 3)) > 12, 3);
  rows = find(any(mask, 2));
  cols = find(any(mask, 1));
  % drawn square (equal data units) and no taller than the axes box (plus the title above)
  scale = size(img, 1) / H;
  assert_istrue(rows(end) - rows(1) <= (box(4) + 40) * scale);
  assert_istrue(cols(end) - cols(1) <= (box(3) + 2) * scale);
  close(f);
end
%=============================================================================
function [clips, W, H] = axesClips(f)
  dl = jsondecode(__web_display_list__(f, false));
  W = dl.width;
  H = dl.height;
  prims = dl.primitives;
  if ~iscell(prims)
    prims = num2cell(prims);
  end
  clips = [];
  for k = 1:numel(prims)
    p = prims{k};
    if strcmp(p.kind, 'clip') && ~(p.x == 0 && p.y == 0 && p.w == dl.width && p.h == dl.height)
      clips(end + 1, :) = [p.x, p.y, p.w, p.h];
    end
  end
  % the children clip is the largest one below the frame (an earlier pass emits the bare axes rect)
  [~, order] = sort(clips(:, 3) .* clips(:, 4), 'descend');
  clips = clips(order, :);
end
%=============================================================================
