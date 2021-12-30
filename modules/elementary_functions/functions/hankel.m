%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
