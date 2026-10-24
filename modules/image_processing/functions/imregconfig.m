%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [optimizer, metric] = imregconfig(modality)
  narginchk(1, 1);
  nargoutchk(0, 2);
  if ~(ischar(modality) || isStringScalar(modality))
    error('Nelson:image_processing:registrationModalityString', '%s must be a string.', 'Registration modality');
  end
  modality = lower(convertStringsToChars(modality));
  switch modality
    case 'monomodal'
      optimizer = struct('Type', 'regularstepgradientdescent', ...
        'MaximumIterations', 50, 'AngleSearch', 15, 'ScaleSearch', 0.05, ...
        'ShearSearch', 0.05);
      metric = struct('Type', 'MeanSquares');
    case 'multimodal'
      optimizer = struct('Type', 'oneplusoneevolutionary', ...
        'MaximumIterations', 50, 'AngleSearch', 15, 'ScaleSearch', 0.05, ...
        'ShearSearch', 0.05);
      metric = struct('Type', 'Correlation');
    otherwise
      error('Nelson:image_processing:unsupportedRegistrationModality', 'Unsupported "%s" option.', 'registration modality');
  end
end
%=============================================================================
