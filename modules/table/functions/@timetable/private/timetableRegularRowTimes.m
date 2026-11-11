%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rowTimes = timetableRegularRowTimes(nRows, opts)
  % Row times of a regular timetable: nRows times from StartTime (0 sec by
  % default) spaced by TimeStep or 1/SampleRate.
  if ~isempty(opts.SampleRate) && ~isempty(opts.TimeStep)
    error('Nelson:timetable:invalidSyntax:specifySampleRateTimeStepNotBoth', ...
      'Specify SampleRate or TimeStep, not both.');
  end
  % a numeric StartTime or TimeStep gives numeric row times
  numericTimes = isNumericTime(opts.StartTime) || isNumericTime(opts.TimeStep);
  startTime = defaultStart(opts.StartTime, numericTimes);
  step = regularStep(opts, numericTimes);
  values = 0:(nRows - 1);
  rowTimes = startTime + values(:) .* step;
end
%=============================================================================
function step = regularStep(opts, numericTimes)
  % TimeStep, or 1/SampleRate (seconds unless the row times are numbers)
  if ~isempty(opts.SampleRate)
    if ~isnumeric(opts.SampleRate) || ~isscalar(opts.SampleRate) || opts.SampleRate <= 0
      error('Nelson:timetable:invalidValue:sampleRateAPositiveNumericScalar', ...
        'SampleRate must be a positive numeric scalar.');
    end
    step = 1 / double(opts.SampleRate);
    if ~numericTimes
      step = seconds(step);
    end
    return
  end
  step = opts.TimeStep;
  if ~(isa(step, 'duration') || isNumericTime(step)) || ~isscalar(step)
    error('Nelson:timetable:timeStepNotDuration', 'TimeStep must be a duration, double or single scalar.');
  end
end
%=============================================================================
function tf = isNumericTime(value)
  tf = (isa(value, 'double') || isa(value, 'single')) && ~isempty(value);
end
%=============================================================================
function startTime = defaultStart(startTime, numericTimes)
  % 0 (numeric times) or 0 sec when no StartTime is given
  if isempty(startTime) && numericTimes
    startTime = 0;
  elseif isempty(startTime)
    startTime = seconds(0);
  end
end
%=============================================================================
