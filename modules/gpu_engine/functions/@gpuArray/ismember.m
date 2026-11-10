%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ismember(varargin)
  % membership by binary search in the sorted set on the device; other forms run on the host
  ok = false;
  if nargin == 2 && nargout <= 1
    [ok, a, b] = __gpuArray_set_operands__(varargin{1}, varargin{2}, true);
  end
  if ok
    u = unique(b(:));
    u = u(~isnan(u));
    varargout{1} = sortedMember(a, u);
    return
  end
  [varargout{1:max(1, nargout)}] = __gpuArray_host__('ismember', varargin{:});
end
%=============================================================================
function tf = sortedMember(a, u)
  tf = isnan(a) & false;
  if isempty(u)
    return
  end
  hasInf = u(end) == Inf;
  if hasInf
    u = u(1:end - 1);
  end
  if ~isempty(u)
    bin = discretize(a, [u; Inf]);
    found = ~isnan(bin);
    bin(~found) = 1;
    tf = found & (reshape(u(bin), size(a)) == a);
  end
  if hasInf
    tf = tf | (a == Inf);
  end
end
%=============================================================================
