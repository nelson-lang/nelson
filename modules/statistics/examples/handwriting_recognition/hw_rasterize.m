%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Rasterize a set of pen strokes into an R-by-R grayscale image (ink near 1).
% strokes is a cell array; each cell is a K-by-2 list of [x y] points, both in
% [0 1] with y pointing down. The same rasterizer feeds training and inference,
% so the pixel representation always matches.
function img = hw_rasterize(strokes, R, radius)
  if nargin < 2 || isempty(R)
    R = 48;
  end
  if nargin < 3 || isempty(radius)
    radius = R / 22;
  end
  pts = collectSamples(strokes, R);
  if isempty(pts)
    img = zeros(R, R);
    return;
  end
  cx = pts(:, 1) * (R - 1) + 1;
  cy = pts(:, 2) * (R - 1) + 1;
  img = splatDisk(cx, cy, radius, R);
end
%=============================================================================
% Sample every stroke densely so segments become continuous pixel runs.
function pts = collectSamples(strokes, R)
  pts = zeros(0, 2);
  for s = 1:numel(strokes)
    P = strokes{s};
    if isempty(P)
      continue;
    end
    if size(P, 1) == 1
      pts = [pts; P];
      continue;
    end
    for k = 1:size(P, 1) - 1
      a = P(k, :);
      b = P(k + 1, :);
      n = max(2, ceil(hypot(b(1) - a(1), b(2) - a(2)) * R * 2));
      t = linspace(0, 1, n)';
      pts = [pts; a + t * (b - a)];
    end
  end
end
%=============================================================================
% Paint each sample with a round soft brush and keep the brightest hit.
function img = splatDisk(cx, cy, radius, R)
  rr = ceil(radius) + 1;
  [ox, oy] = meshgrid(-rr:rr, -rr:rr);
  ox = ox(:);
  oy = oy(:);
  keep = (ox .^ 2 + oy .^ 2) <= (radius + 0.8) ^ 2;
  ox = ox(keep);
  oy = oy(keep);
  acc = zeros(R * R, 1);
  for m = 1:numel(ox)
    w = max(0, 1 - hypot(ox(m), oy(m)) / (radius + 0.6));
    if w <= 0
      continue;
    end
    pr = round(cy + oy(m));
    pc = round(cx + ox(m));
    inb = pr >= 1 & pr <= R & pc >= 1 & pc <= R;
    idx = (pc(inb) - 1) * R + pr(inb);
    acc(idx) = max(acc(idx), w);
  end
  img = reshape(acc, R, R);
end
%=============================================================================
