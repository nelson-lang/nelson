%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, maxGap, window] = fillmissingSamplePoints(samplePoints, n, maxGap, window, isGapWindow)
  % Validates the sample points (n of them) and returns them as a numeric
  % column x ([] when absent), datetime and duration values in seconds.
  % maxGap and window (a moving window, or the gap window of a function
  % handle when isGapWindow; [] when unused) must have a compatible type
  % and are converted to the same unit.
  x = [];
  timeBased = false;
  if ~isempty(samplePoints)
    [x, timeBased] = samplePointsSeconds(samplePoints, n);
  end
  if ~isempty(maxGap)
    isDuration = isduration(maxGap);
    if ~(isscalar(maxGap) && (isDuration || (isnumeric(maxGap) && isreal(maxGap))) && toSeconds(maxGap) > 0)
      error('Nelson:data_analysis:fillmissingInvalidMaxGap', '''MaxGap'' must be a positive numeric or duration scalar.');
    end
    if isDuration ~= timeBased
      error('Nelson:data_analysis:fillmissingMaxGapType', '''MaxGap'' data type must be compatible with ''SamplePoints'' data type.');
    end
    maxGap = toSeconds(maxGap);
  end
  if ~isempty(window)
    if isduration(window) ~= timeBased
      windowTypeError(timeBased, isGapWindow);
    end
    window = toSeconds(window);
  end
end
%=============================================================================
function [x, timeBased] = samplePointsSeconds(samplePoints, n)
  timeBased = isdatetime(samplePoints) || isduration(samplePoints);
  if ~(isvector(samplePoints) && (timeBased || (isfloat(samplePoints) && isreal(samplePoints))))
    error('Nelson:data_analysis:fillmissingSamplePointsType', '''SamplePoints'' value must be a vector of type double, single, duration, or datetime.');
  end
  if numel(samplePoints) ~= n
    error('Nelson:data_analysis:fillmissingSamplePointsLength', 'The sample points vector length must be %d, the same as the size of the input data in the operating dimension.', n);
  end
  if isdatetime(samplePoints)
    x = seconds(samplePoints(:) - samplePoints(1));
  else
    x = double(toSeconds(samplePoints(:)));
  end
  if ~all(isfinite(x))
    error('Nelson:data_analysis:fillmissingSamplePointsNonFinite', '''SamplePoints'' value must not contain Inf or NaN.');
  end
  steps = diff(x);
  if any(steps < 0)
    error('Nelson:data_analysis:fillmissingSamplePointsSorted', '''SamplePoints'' value must be sorted in ascending order.');
  end
  if any(steps == 0)
    error('Nelson:data_analysis:fillmissingSamplePointsDuplicate', '''SamplePoints'' value contains duplicates.');
  end
end
%=============================================================================
function windowTypeError(timeBased, isGapWindow)
  if isGapWindow && timeBased
    error('Nelson:data_analysis:fillmissingInvalidGapWindowDuration', 'Window length must be a finite positive scalar or 2-element vector of finite nonnegative scalars of type duration.');
  elseif isGapWindow
    error('Nelson:data_analysis:fillmissingInvalidGapWindow', 'Window length must be a finite positive scalar or 2-element vector of finite nonnegative scalars.');
  elseif timeBased
    error('Nelson:data_analysis:fillmissingWindowDuration', 'Invalid data type. Window size must be a duration if ''SamplePoints'' value is a duration or a datetime.');
  end
  error('Nelson:data_analysis:fillmissingWindowType', 'Window size data type must be compatible with ''SamplePoints'' data type.');
end
%=============================================================================
function value = toSeconds(value)
  if isduration(value)
    value = seconds(value);
  end
end
%=============================================================================
