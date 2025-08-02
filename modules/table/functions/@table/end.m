%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = end(varargin)
  narginchk(3, 3);
  nargoutchk(0, 1);
  T = varargin{1};
  k = varargin{2};
  n = varargin{3};
  sz = size(T);
  if k < n
    ind = sz(k);
  else
    ind = prod(sz(k:end));
  end
  varargout{1} = ind; 
end
%=============================================================================
