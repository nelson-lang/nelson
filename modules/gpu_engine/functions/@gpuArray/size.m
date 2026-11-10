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
  d = __gpuArray_size__(varargin{1});
  if nargin > 1
    % the host checks the dimension arguments on a scalar, the values come from d
    size(1, varargin{2:end});
    if nargin == 2 && (ischar(varargin{2}) || isstring(varargin{2}))
      dims = 1 + strcmp(char(varargin{2}), 'c');
    else
      dims = double([varargin{2:end}]);
    end
    d(end + 1:max(dims)) = 1;
    out = d(dims);
    if nargout <= 1
      varargout{1} = out;
    else
      varargout = num2cell(out);
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
