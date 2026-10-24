%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = size(varargin)
  narginchk(1, 2);
  d = __gpuArray_size__(varargin{1});
  if nargin == 2
    dim = varargin{2};
    if dim <= length(d)
      varargout{1} = d(dim);
    else
      varargout{1} = 1;
    end
    return
  end
  if nargout <= 1
    varargout{1} = d;
  else
    for k = 1:nargout
      if k < nargout
        if k <= length(d)
          varargout{k} = d(k);
        else
          varargout{k} = 1;
        end
      else
        varargout{k} = prod(d(k:end));
      end
    end
  end
end
%=============================================================================
