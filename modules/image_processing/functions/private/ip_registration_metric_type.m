%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function metricType = ip_registration_metric_type(metric)
  if isstruct(metric)
    if ~isfield(metric, 'Type')
      error('Nelson:image_processing:registrationMetricContainTypeField', 'Registration metric must contain a Type field.');
    end
    metric = metric.Type;
  end
  if ~(ischar(metric) || isStringScalar(metric))
    error('Nelson:image_processing:registrationMetricTypeString', '%s must be a string.', 'Registration metric type');
  end
  metricType = lower(convertStringsToChars(metric));
  switch metricType
    case {'meansquares', 'mean-squares', 'mean_squares'}
      metricType = 'meansquares';
    case {'correlation', 'mattesmutualinformation'}
      metricType = 'correlation';
    otherwise
      error('Nelson:image_processing:unsupportedRegistrationMetric', 'Unsupported "%s" option.', 'registration metric');
  end
end
%=============================================================================
