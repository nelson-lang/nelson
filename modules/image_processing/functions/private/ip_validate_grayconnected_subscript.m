%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = ip_validate_grayconnected_subscript(value, maxValue, name)
  if ~isnumeric(value) || ~isreal(value) || numel(value) ~= 1 || ~isfinite(value)
    error('Nelson:image_processing:mustBeFiniteScalar', '%s subscript must be a finite numeric scalar.', name);
  end
  value = round(double(value));
  if value < 1 || value > maxValue
    error('Nelson:image_processing:outOfBounds', '%s subscript is outside image bounds.', name);
  end
end
%=============================================================================
