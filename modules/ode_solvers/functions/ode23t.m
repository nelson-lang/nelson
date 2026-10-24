%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ode23t(varargin)
  n = max(nargout, 1);
  varargout = cell(1, n);
  [varargout{:}] = odeSolveFunction('ode23t', n, varargin{:});
end
%=============================================================================
