%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = intersect(varargin)
  % sorted values of both sets on the device; other forms run on the host
  ok = false;
  if nargin == 2 && nargout <= 1
    [ok, a, b] = __gpuArray_set_operands__(varargin{1}, varargin{2}, false);
  end
  if ok
    u = unique(a(:));
    c = u(ismember(u, b));
    if isrow(a) && isrow(b)
      c = c.';
    end
    varargout{1} = c;
    return
  end
  [varargout{1:max(1, nargout)}] = __gpuArray_host__('intersect', varargin{:});
end
%=============================================================================
