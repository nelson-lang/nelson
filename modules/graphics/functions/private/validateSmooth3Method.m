%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function method = validateSmooth3Method(method)
  if ~ischar(method)
    error('Nelson:validateSmooth3Method:invalidSmoothingMethod', 'Smoothing method must be ''box'' or ''gaussian''.');
  end
  method = lower(method);
  if ~any(strcmp(method, {'box', 'gaussian'}))
    error('Nelson:validateSmooth3Method:invalidSmoothingMethod', 'Smoothing method must be ''box'' or ''gaussian''.');
  end
end
%=============================================================================
