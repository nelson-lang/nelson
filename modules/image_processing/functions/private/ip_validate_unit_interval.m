%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = ip_validate_unit_interval(value, name)
  if ~isnumeric(value) || ~isreal(value) || numel(value) ~= 1 || ~isfinite(value) ...
      || value < 0 || value > 1
    error('Nelson:image_processing:mustBeInRange', '%s must be a finite scalar in the range [%s].', name, '0, 1');
  end
  value = double(value);
end
%=============================================================================
