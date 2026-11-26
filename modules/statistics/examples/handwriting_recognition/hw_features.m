%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Turn a rasterized glyph into a size-, slant- and position-normalized feature
% vector. The ink is cropped, slant-corrected (moment deskew), scaled to a fixed
% box keeping its aspect ratio, recentred on its centre of mass, lightly blurred
% and L2-normalized, so drawings and training glyphs land in the same space and
% cosine distance is meaningful.
function f = hw_features(img, C)
  if nargin < 2 || isempty(C)
    C = 20;
  end
  f = zeros(C * C, 1);
  patch = cropInk(img);
  if isempty(patch)
    return;
  end
  patch = deskew(patch);
  patch = cropInk(patch);
  if isempty(patch)
    return;
  end
  [h, w] = size(patch);
  scale = (C - 4) / max(h, w);
  patch = imresize(patch, [max(1, round(h * scale)), max(1, round(w * scale))]);
  patch(patch < 0) = 0;
  pm = max(patch(:));
  if pm > 0
    patch = patch / pm;
  end
  canvas = placeByMass(patch, C);
  g = [1 2 1] / 4;
  canvas = conv2(conv2(canvas, g, 'same'), g', 'same');
  v = canvas(:);
  nrm = norm(v);
  if nrm > 0
    v = v / nrm;
  end
  f = v;
end
%=============================================================================
% Crop to the ink bounding box; returns [] when the image holds no ink.
function patch = cropInk(img)
  patch = [];
  peak = max(img(:));
  if peak <= 0
    return;
  end
  [rows, cols] = find(img > 0.15 * peak);
  if isempty(rows)
    return;
  end
  patch = img(min(rows):max(rows), min(cols):max(cols));
end
%=============================================================================
% Moment-based deskew: shear horizontally so the ink stands upright.
function out = deskew(patch)
  out = patch;
  [h, w] = size(patch);
  if h < 3 || w < 3
    return;
  end
  [X, Y] = meshgrid(1:w, 1:h);
  total = sum(patch(:));
  if total <= 0
    return;
  end
  mx = sum(X(:) .* patch(:)) / total;
  my = sum(Y(:) .* patch(:)) / total;
  mu02 = sum(((Y(:) - my) .^ 2) .* patch(:)) / total;
  mu11 = sum((X(:) - mx) .* (Y(:) - my) .* patch(:)) / total;
  if mu02 < 1
    return;
  end
  alpha = max(-0.5, min(0.5, mu11 / mu02));
  % Pad sideways so the sheared ink is not clipped, then resample.
  pad = ceil(abs(alpha) * h) + 1;
  wide = [zeros(h, pad), patch, zeros(h, pad)];
  [Xw, Yw] = meshgrid(1:(w + 2 * pad), 1:h);
  out = interp2(Xw, Yw, wide, Xw + alpha * (Yw - my), Yw, 'linear', 0);
end
%=============================================================================
% Centre the patch on its centre of mass inside a C-by-C canvas.
function canvas = placeByMass(patch, C)
  [th, tw] = size(patch);
  [gx, gy] = meshgrid(1:tw, 1:th);
  total = sum(patch(:));
  comR = sum(gy(:) .* patch(:)) / total;
  comC = sum(gx(:) .* patch(:)) / total;
  canvas = zeros(C, C);
  rIdx = (1:th) + round(C / 2 - comR);
  cIdx = (1:tw) + round(C / 2 - comC);
  vr = rIdx >= 1 & rIdx <= C;
  vc = cIdx >= 1 & cIdx <= C;
  canvas(rIdx(vr), cIdx(vc)) = patch(vr, vc);
end
%=============================================================================
