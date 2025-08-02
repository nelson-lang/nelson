%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ipermute(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  A = varargin{1};
  dimorder = varargin{2};
  dimreordered(dimorder) = 1:numel(dimorder);
  varargout{1} = permute(A, dimreordered);
end
%=============================================================================