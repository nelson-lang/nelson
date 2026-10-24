%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = standaloneWindowsApplication(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 1);
  if ~ispc()
    error('Nelson:nelsonc:platform', 'No-console applications require Windows.');
  end
  result = __nelsonc_standalone__(true, varargin{:});
end
