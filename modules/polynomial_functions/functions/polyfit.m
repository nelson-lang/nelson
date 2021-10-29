%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
function p = polyfit(x, y, n)
  if numel(x) ~= numel(y)
    error('Nelson:polyfit:XYSizeMismatch', _('The first two inputs must have the same number of elements.'));
  end
  x = x(:);
  y = y(:);
  A = zeros(size(x, 1), n+1);
  xp = x*0+1;
  for i = (n+1):-1:1
    A(:, i) = xp;
    xp = xp.*x;
  end
  p = (pinv(A)*y)';
end
%=============================================================================
