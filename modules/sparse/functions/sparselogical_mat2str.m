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
function r = sparselogical_mat2str(varargin)
  r = '';
  if nargin > 0 && nargin < 4

    M = varargin{1};

    if nargin > 1
      NorC = varargin{2};
    else
      NorC = 15;
    end

    if nargin > 2
       C = varargin{3};
    end

    [IV, JV, VV, M, N, NZMAX] = IJV(M);
    SIV = mat2str(IV);
    SJV = mat2str(JV);
    SM = mat2str(M);
    SN = mat2str(N);
    SNZMAX = mat2str(NZMAX);

    if nargin == 1
      SVV = mat2str(VV);
    else
      if nargin == 2
        SVV = mat2str(VV, NorC);
      else
        SVV = mat2str(VV, NorC, C);
      end
    end

    if M * N == NZMAX
      r = ['sparse(', SIV, ', ', SJV, ', ', SVV, ', ', SM, ', ', SN, ')'];
    else
      r = ['sparse(', SIV, ', ', SJV, ', ', SVV, ', ', SM, ', ', SN, ', ', SNZMAX, ')'];
    end

  else
    error(_('Wrong number of input arguments.'));
  end

end
