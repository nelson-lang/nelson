%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function transformType = ip_registration_transform_type(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:image_processing:registrationTransformationTypeString', '%s must be a string.', 'Registration transformation type');
  end
  transformType = lower(convertStringsToChars(value));
  switch transformType
    case {'translation', 'rigid', 'similarity', 'affine'}
    otherwise
      error('Nelson:image_processing:unsupportedRegistrationTransformationType', 'Unsupported "%s" option.', 'registration transformation type');
  end
end
%=============================================================================
