%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Apply a random affine warp plus point jitter to a glyph skeleton, so one
% hand-authored shape yields many plausible handwritten variants. p carries the
% random parameters (rotation, scale, shear, translation, jitter) so the caller
% controls the spread.
function out = hw_augment(strokes, p)
  c = [0.5, 0.5];
  rot = [cos(p.theta), -sin(p.theta); sin(p.theta), cos(p.theta)];
  shear = [p.sx, p.shx; 0, p.sy];
  M = rot * shear;
  out = cell(size(strokes));
  for i = 1:numel(strokes)
    P = strokes{i};
    Q = (P - c) * M' + c + [p.tx, p.ty];
    Q = Q + p.jit * randn(size(Q));
    out{i} = Q;
  end
end
%=============================================================================
