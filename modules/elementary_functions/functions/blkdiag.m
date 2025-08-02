%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = blkdiag(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  
  isSupported = all(cellfun('ismatrix', varargin)) && all(cellfun ('isnumeric', varargin));
  if ~isSupported
    error(_('all arguments must numeric matrix.'));
  end
  
  haveSparse = any(cellfun('issparse', varargin));
  
  % need to manage string type
  
  mat = cell2mat(cellfun(@size, varargin', 'UniformOutput', false));
  cs = cumsum([0 0; mat], 1);
  
  % preallocate
  M = zeros(cs(end, :));
  
  for k = 1:nargin
    arg = varargin{k};
    if (~isempty(arg))
      X = (cs(k, 1) + 1):cs(k + 1, 1);
      Y = (cs(k, 2) + 1):cs(k + 1, 2);
      if issparse(arg)
        M(X, Y) = full(arg);
      else
        M(X, Y) = arg;
      end
    end
  end
  if haveSparse
    varargout{1} = sparse(M);
  else
    varargout{1} = M;
  end
end
%=============================================================================
