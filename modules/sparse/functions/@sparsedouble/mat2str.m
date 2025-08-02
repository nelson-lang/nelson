%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = mat2str(varargin)
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
