%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bar3h(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  h = bar3Base(true, varargin{:});
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================