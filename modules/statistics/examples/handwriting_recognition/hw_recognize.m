%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Classify drawn strokes against the template bank with a weighted k-nearest
% neighbour vote. Features are L2-normalized, so the dot product is the cosine
% similarity. Returns the recognized character, a 0..1 confidence, the three
% best candidates with their neighbourhood shares, the normalized C-by-C image
% the classifier actually sees (featImg), and the single closest template with
% its character (protoImg, protoChar) so the UI can show "yours vs nearest".
function [ch, conf, topChars, topScores, featImg, protoImg, protoChar] = ...
  hw_recognize(strokes, model)
  ch = '?';
  conf = 0;
  topChars = '?  ';
  topScores = [0, 0, 0];
  protoChar = '?';
  f = hw_features(hw_rasterize(strokes, model.R), model.C);
  featImg = reshape(f, model.C, model.C);
  protoImg = zeros(model.C, model.C);
  if ~any(f)
    return;
  end
  sims = model.X' * f;
  [sorted, order] = sort(sims, 'descend');
  protoImg = reshape(model.X(:, order(1)), model.C, model.C);
  protoChar = model.labels(model.y(order(1)));
  K = min(20, numel(sorted));
  nn = model.y(order(1:K));
  sc = sorted(1:K);
  classes = unique(nn);
  agg = zeros(numel(classes), 1);
  for i = 1:numel(classes)
    agg(i) = sum(sc(nn == classes(i)));
  end
  [aggSorted, ai] = sort(agg, 'descend');
  denom = max(sum(sc), eps);
  nTop = min(3, numel(classes));
  topChars = repmat(' ', 1, 3);
  topScores = zeros(1, 3);
  for i = 1:nTop
    topChars(i) = model.labels(classes(ai(i)));
    topScores(i) = aggSorted(i) / denom;
  end
  ch = topChars(1);
  % Headline confidence is the winner's neighbourhood share, so it matches the
  % top bar exactly instead of being a second, slightly different number.
  conf = max(0, min(1, topScores(1)));
end
%=============================================================================
