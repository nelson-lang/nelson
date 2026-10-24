%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = ip_parse_corner_options(args)
  options = struct('FilterSize', 5, 'SensitivityFactor', 0.04, ...
    'MinQuality', 0.01, 'MinContrast', 0.2, 'ROI', []);
  if mod(numel(args), 2) ~= 0
    error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
  end
  for k = 1:2:numel(args)
    if ~(ischar(args{k}) || isStringScalar(args{k}))
      error('Nelson:image_processing:featureOptionNameString', '%s option name must be a string.', 'Feature');
    end
    name = lower(convertStringsToChars(args{k}));
    value = args{k + 1};
    switch name
      case 'filtersize'
        options.FilterSize = ip_validate_corner_filter_size(value);
      case 'sensitivityfactor'
        options.SensitivityFactor = ip_validate_unit_interval(value, 'SensitivityFactor');
      case 'minquality'
        options.MinQuality = ip_validate_unit_interval(value, 'MinQuality');
      case 'mincontrast'
        options.MinContrast = ip_validate_unit_interval(value, 'MinContrast');
      case 'roi'
        options.ROI = ip_validate_corner_roi(value);
      otherwise
        error('Nelson:image_processing:unsupportedFeatureOption', 'Unsupported "%s" option.', 'feature');
    end
  end
end
%=============================================================================
