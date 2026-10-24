%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function keys = synth_piano_keys_at(keyBox, keyBlack, from, to)
  % Keys crossed by the pointer going from `from` to `to` ([x y] in key
  % units), in order and without repeats; a single point when from == to.
  % keyBox holds one [x y width height] row per key. Black keys are on top of
  % their white neighbours. Between two motion events a fast drag can pass
  % over several keys: the segment is sampled every 4 units, the narrowest
  % key being 32 wide, so none is missed.
  t = linspace(0, 1, max(1, ceil(max(abs(to - from)) / 4) + 1));
  xs = from(1) + t(:) * (to(1) - from(1));
  ys = from(2) + t(:) * (to(2) - from(2));
  inside = xs >= keyBox(:, 1)' & xs < (keyBox(:, 1) + keyBox(:, 3))' ...
    & ys >= keyBox(:, 2)' & ys < (keyBox(:, 2) + keyBox(:, 4))';
  [rank, keys] = max(inside .* (1 + keyBlack(:)'), [], 2);
  keys = keys(rank > 0)';
  if ~isempty(keys)
    keys = keys([true, diff(keys) ~= 0]);
  end
end
%=============================================================================
