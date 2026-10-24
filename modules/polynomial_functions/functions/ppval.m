%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function v = ppval(pp, xq)
  narginchk(2, 2);
  nargoutchk(0, 1);
  if ~isstruct(pp) || ~isfield(pp, 'form') || ~strcmp(pp.form, 'pp')
    error('Nelson:ppval:invalidStructure', 'Piecewise polynomial structure expected.');
  end
  breaks = pp.breaks;
  coefs = pp.coefs;
  pieces = pp.pieces;
  if isfield(pp, 'dim')
    d = prod(pp.dim);
  else
    d = 1;
  end
  order = size(coefs, 2);
  xqSize = size(xq);
  q = xq(:);
  nq = numel(q);
  % Locate every query's piece in one vectorized pass instead of scanning the
  % breaks for each query, which made ppval O(nq * pieces).
  pieceIdx = discretize(q, breaks(:).');
  pieceIdx(q <= breaks(1)) = 1;
  pieceIdx(q >= breaks(end)) = pieces;
  pieceIdx(isnan(pieceIdx)) = pieces;
  pieceIdx = min(max(pieceIdx, 1), pieces);
  if d == 1
    dx = q - reshape(breaks(pieceIdx), nq, 1);
    C = coefs(pieceIdx, :);
    val = C(:, 1);
    for j = 2:order
      val = val .* dx + C(:, j);
    end
    v = reshape(val, xqSize);
    return
  end
  v = zeros(d, nq) + 0 * coefs(1);
  for k = 1:nq
    piece = pieceIdx(k);
    dx = q(k) - breaks(piece);
    baseRow = (piece - 1) * d;
    for r = 1:d
      c = coefs(baseRow + r, :);
      val = 0;
      for j = 1:order
        val = val * dx + c(j);
      end
      v(r, k) = val;
    end
  end
  if d == 1
    v = reshape(v, xqSize);
  end
end
%=============================================================================
