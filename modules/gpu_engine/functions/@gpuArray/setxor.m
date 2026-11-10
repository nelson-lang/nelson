%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = setxor(varargin)
  % sorted values in exactly one set on the device; other forms run on the host
  ok = false;
  if nargin == 2 && nargout <= 1
    [ok, a, b] = __gpuArray_set_operands__(varargin{1}, varargin{2}, false);
  end
  if ok
    ua = unique(a(:));
    ub = unique(b(:));
    c = sort([ua(~ismember(ua, b)); ub(~ismember(ub, a))]);
    if isrow(a) && isrow(b)
      c = c.';
    end
    varargout{1} = c;
    return
  end
  [varargout{1:max(1, nargout)}] = __gpuArray_host__('setxor', varargin{:});
end
%=============================================================================
