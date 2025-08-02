%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function H = hankel(c, r)
  narginchk(1, 2);
  nargoutchk(0, 1);
  c = c(:);
  nc = length(c);
  if nargin < 2
    r = zeros(size(c), 'like', c);
  elseif (~isempty(c) && ~isempty(r) && c(nc) ~= r(1))
    warning('Nelson:hankel:AntiDiagonalConflict', _('Last element of input column does not match first element of input row.'));
  end
  r = r(:);
  nr = length(r);
  x = [ c; r(2:nr, 1)];
  ij = (1:nc)' + (0:(nr-1));
  H = x(ij);
  if isrow(ij) && ~isempty(H)
    H = H.';
  end
end
%=============================================================================
