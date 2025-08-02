%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = length(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  [m, n] =  size(varargin{1}.Numerator);
  varargout{1} = m*n;
end
%=============================================================================

