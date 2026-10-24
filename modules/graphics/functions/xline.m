%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xline(varargin)
  nargoutchk(0, 1);
  narginchk(1, inf);
  handle = constantLineImpl('x', varargin);
  if nargout > 0
    varargout{1} = handle;
  end
end
%=============================================================================
