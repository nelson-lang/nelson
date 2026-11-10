%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = unique(varargin)
  % sorted distinct values on the device; other forms run on the host
  a = varargin{1};
  if nargin == 1 && nargout <= 1 && isgpuarray(a) && ~isempty(a) && isreal(a)
    v = sort(a(:));
    if numel(v) > 1
      v = v([true; v(2:end) ~= v(1:end - 1)]);
    end
    if isrow(a)
      v = v.';
    end
    varargout{1} = v;
    return
  end
  [varargout{1:max(1, nargout)}] = __gpuArray_host__('unique', varargin{:});
end
%=============================================================================
