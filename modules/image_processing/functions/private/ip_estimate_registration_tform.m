%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = ip_estimate_registration_tform(moving, fixed, transformType, metricType, options)
  if strcmp(transformType, 'translation')
    T = ip_translation_registration(moving, fixed, options);
    return
  end
  span = min(max(abs(options.AngleSearch), 0), 45);
  step = max(1, ceil(span / 6));
  angles = -span:step:span;
  if strcmp(transformType, 'affine')
    [T, angle, scaleX, scaleY, shear] = ip_registration_affine_search(...
      moving, fixed, angles, options, metricType);
    fineAngles = (angle - step):max(step / 4, 1):(angle + step);
    fineX = unique([scaleX - options.ScaleSearch / 2, scaleX, ...
      scaleX + options.ScaleSearch / 2]);
    fineY = unique([scaleY - options.ScaleSearch / 2, scaleY, ...
      scaleY + options.ScaleSearch / 2]);
    fineShear = unique([shear - options.ShearSearch / 2, shear, ...
      shear + options.ShearSearch / 2]);
    Tfine = ip_registration_affine_search(moving, fixed, fineAngles, ...
      options, metricType, fineX, fineY, fineShear);
    if ip_registration_score(moving, fixed, Tfine, metricType, options.Interpolation) ...
        > ip_registration_score(moving, fixed, T, metricType, options.Interpolation)
      T = Tfine;
    end
    return
  end
  scales = 1;
  if strcmp(transformType, 'similarity')
    scales = unique([1 - options.ScaleSearch, 1, 1 + options.ScaleSearch]);
  end
  [T, bestAngle, bestScale] = ip_registration_search(moving, fixed, angles, scales, metricType, options);
  fineAngles = (bestAngle - step):max(step / 4, 1):(bestAngle + step);
  if numel(scales) == 1
    fineScales = bestScale;
  else
    fineScales = unique([bestScale - options.ScaleSearch / 2, bestScale, ...
      bestScale + options.ScaleSearch / 2]);
  end
  Tfine = ip_registration_search(moving, fixed, fineAngles, fineScales, metricType, options);
  if ip_registration_score(moving, fixed, Tfine, metricType, options.Interpolation) ...
      > ip_registration_score(moving, fixed, T, metricType, options.Interpolation)
    T = Tfine;
  end
end
%=============================================================================
