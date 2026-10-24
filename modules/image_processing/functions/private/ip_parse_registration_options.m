%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = ip_parse_registration_options(args, optimizer)
  options = struct('InitialTransformation', eye(3), 'Interpolation', 'linear', ...
    'MaximumIterations', double(optimizer.MaximumIterations), ...
    'AngleSearch', double(optimizer.AngleSearch), 'ScaleSearch', double(optimizer.ScaleSearch), ...
    'ShearSearch', double(optimizer.ShearSearch));
  if mod(numel(args), 2) ~= 0
    error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
  end
  for k = 1:2:numel(args)
    if ~(ischar(args{k}) || isStringScalar(args{k}))
      error('Nelson:image_processing:registrationOptionNameString', '%s option name must be a string.', 'Registration');
    end
    name = lower(convertStringsToChars(args{k}));
    value = args{k + 1};
    switch name
      case 'initialtransformation'
        options.InitialTransformation = ip_tform_matrix(value);
      case 'interpolation'
        options.Interpolation = ip_registration_interpolation(value);
      case 'pyramidlevels'
        ip_validate_registration_positive_integer(value, 'PyramidLevels');
      case 'displayoptimization'
      otherwise
        error('Nelson:image_processing:unsupportedRegistrationOption', 'Unsupported "%s" option.', 'registration');
    end
  end
end
%=============================================================================
