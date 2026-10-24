%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = ip_validate_corner_filter_size(value)
  if ~isnumeric(value) || ~isreal(value) || numel(value) ~= 1 || value < 3 ...
      || fix(value) ~= value || mod(value, 2) == 0
    error('Nelson:image_processing:filtersizeOddIntegerScalarGreaterEqual', 'FilterSize must be an odd integer scalar greater than or equal to 3.');
  end
  value = double(value);
end
%=============================================================================
