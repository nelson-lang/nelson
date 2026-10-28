%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = validateSampleTime(value)
  if isempty(value)
    return
  end
  if ~isnumeric(value) || ~isscalar(value) || ~(value == -1 || value >= 0)
    error('Nelson:control_system:invalidSampleTimeProperty', 'Ts property should be either a positive scalar, 0, or -1 to indicate that it is unspecified.');
  end
end
%=============================================================================
