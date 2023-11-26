%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = size(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  [m, n, p] = size(varargin{1});
  varargout{1} = m * n;  
end
%=============================================================================
