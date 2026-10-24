%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = ip_parse_registration_output_options(args)
  options = struct('Interpolation', 'linear');
  if mod(numel(args), 2) ~= 0
    error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
  end
  for k = 1:2:numel(args)
    if ~(ischar(args{k}) || isStringScalar(args{k}))
      error('Nelson:image_processing:registrationOptionNameString', '%s option name must be a string.', 'Registration');
    end
    name = lower(convertStringsToChars(args{k}));
    if strcmp(name, 'interpolation')
      options.Interpolation = ip_registration_interpolation(args{k + 1});
    elseif ~strcmp(name, 'initialtransformation') && ~strcmp(name, 'pyramidlevels') ...
        && ~strcmp(name, 'displayoptimization')
      error('Nelson:image_processing:unsupportedRegistrationOption', 'Unsupported "%s" option.', 'registration');
    end
  end
end
%=============================================================================
