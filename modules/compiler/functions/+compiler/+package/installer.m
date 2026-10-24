%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function installer(input, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 0);
  __nelsonc_installer__(input, varargin{:});
end
