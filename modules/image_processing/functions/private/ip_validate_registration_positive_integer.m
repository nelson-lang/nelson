%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_validate_registration_positive_integer(value, name)
  if ~isnumeric(value) || ~isreal(value) || numel(value) ~= 1 || value < 1 ...
      || fix(value) ~= value
    error('Nelson:image_processing:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
end
%=============================================================================
