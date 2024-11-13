%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = csvwrite(varargin)
  nargoutchk(0, 0);
  narginchk(1, 4);
  filename = varargin{1};
  mustBeTextScalar(filename, 1);
  
  M = varargin{2};
  if (nargin < 3)
    R = 0;
  else 
    R = varargin{3};
  end
  if (nargin < 4)
    C = 0;
  else
    C = varargin{4};
  end
  try
    dlmwrite(filename, M, ',', R, C); 
  catch e
    throw(e);
  end
end
%=============================================================================
