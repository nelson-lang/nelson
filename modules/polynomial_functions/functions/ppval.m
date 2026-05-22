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
    error(_('Piecewise polynomial structure expected.'));
  end
  breaks = pp.breaks;
  coefs = pp.coefs;
  pieces = pp.pieces;
  xqSize = size(xq);
  q = xq(:);
  v = zeros(size(q)) + 0 * coefs(1);
  for k = 1:numel(q)
    x = q(k);
    if x <= breaks(1)
      piece = 1;
    elseif x >= breaks(end)
      piece = pieces;
    else
      piece = find(breaks <= x, 1, 'last');
      if piece > pieces
        piece = pieces;
      end
    end
    dx = x - breaks(piece);
    c = coefs(piece, :);
    v(k) = ((c(1) * dx + c(2)) * dx + c(3)) * dx + c(4);
  end
  v = reshape(v, xqSize);
end
%=============================================================================
