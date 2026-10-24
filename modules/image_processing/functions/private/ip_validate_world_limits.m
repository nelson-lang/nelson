%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function limits = ip_validate_world_limits(limits, name)
  if ~isnumeric(limits) || ~isreal(limits) || numel(limits) ~= 2
    error('Nelson:image_processing:mustBeRealVector', '%s must be a real numeric 2-element vector.', name);
  end
  limits = double(reshape(limits, 1, 2));
  if any(~isfinite(limits)) || limits(2) <= limits(1)
    error('Nelson:image_processing:mustBeIncreasing', '%s must contain increasing finite values.', name);
  end
end
%=============================================================================
