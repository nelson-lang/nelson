%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function method = ip_registration_interpolation(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:image_processing:interpolationMethodString', '%s must be a string.', 'Interpolation method');
  end
  method = lower(convertStringsToChars(value));
  if ~(strcmp(method, 'nearest') || strcmp(method, 'linear') || strcmp(method, 'bilinear') ...
      || strcmp(method, 'cubic'))
    error('Nelson:image_processing:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
  end
end
%=============================================================================
