%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build the handwriting template bank and save it next to this script. Run it
% once (headless, no graphics needed) to regenerate handwriting_model.mat after
% editing the glyph skeletons or the feature pipeline. The demo only loads the
% saved file. Everything is pure Nelson, so there is nothing to download.
function build_handwriting_model()
  thisDir = fileparts(mfilename('fullpath'));
  addpath(thisDir);

  R = 48;            % rasterization resolution
  C = 20;            % normalized feature canvas (C*C features)
  nPerVariant = 48;  % augmented samples per hand shape
  rng(20261126);

  [labels, variants] = hw_glyph_skeletons();
  nClass = numel(labels);
  total = 0;
  for ci = 1:nClass
    total = total + numel(variants{ci}) * nPerVariant;
  end
  X = zeros(C * C, total);
  y = zeros(1, total);
  col = 0;
  for ci = 1:nClass
    shapes = variants{ci};
    for vi = 1:numel(shapes)
      base = shapes{vi};
      for a = 1:nPerVariant
        warped = hw_augment(base, randomWarp());
        col = col + 1;
        X(:, col) = hw_features(hw_rasterize(warped, R, randomRadius()), C);
        y(col) = ci;
      end
    end
    fprintf(' %s', labels(ci));
  end
  fprintf('\n');

  model.X = X;
  model.y = y;
  model.labels = labels;
  model.C = C;
  model.R = R;
  outFile = fullfile(thisDir, 'handwriting_model.mat');
  save(outFile, 'model');
  fprintf('Saved %d templates (%d classes) to %s\n', size(X, 2), nClass, outFile);
end
%=============================================================================
% Random affine warp parameters for one augmented sample. Slant stays moderate
% because hw_features already deskews; the rest spreads scale, shift and jitter.
function p = randomWarp()
  p.theta = (rand * 2 - 1) * 0.30;
  p.sx = 0.80 + rand * 0.38;
  p.sy = 0.80 + rand * 0.38;
  p.shx = (rand * 2 - 1) * 0.20;
  p.tx = (rand * 2 - 1) * 0.035;
  p.ty = (rand * 2 - 1) * 0.035;
  p.jit = 0.006 + rand * 0.014;
end
%=============================================================================
function r = randomRadius()
  r = 1.3 + rand * 1.9;
end
%=============================================================================
