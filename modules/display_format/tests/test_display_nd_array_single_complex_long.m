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
rng('default')
format('long')
%=============================================================================
A = complex(rand(3,3,2), rand(3,3,2));
A(1) = 0;
R = evalc('single(A)');
REF =  '
  3×3×2 single array

ans(:,:,1) =

  0.0000000 + 0.0000000i  0.9133759 + 0.0357117i  0.2784982 + 0.6787351i
  0.9057919 + 0.9594924i  0.6323593 + 0.8491293i  0.5468815 + 0.7577401i
  0.1269868 + 0.6557407i  0.0975404 + 0.9339932i  0.9575068 + 0.7431325i


ans(:,:,2) =

  0.9648885 + 0.3922270i  0.9571670 + 0.7060461i  0.1418863 + 0.0461714i
  0.1576131 + 0.6554779i  0.4853756 + 0.0318328i  0.4217613 + 0.0971318i
  0.9705928 + 0.1711867i  0.8002805 + 0.2769230i  0.9157355 + 0.8234578i

';
assert_isequal(R, REF)
%=============================================================================
