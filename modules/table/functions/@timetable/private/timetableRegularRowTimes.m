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
    error('Nelson:timetable:invalidSyntax:specifySampleRateTimeStepNotBoth', 'Specify SampleRate or TimeStep, not both.');
  end
  if isempty(opts.StartTime)
    startTime = seconds(0);
  else
    startTime = opts.StartTime;
  end
  if ~isempty(opts.SampleRate)
    if ~isnumeric(opts.SampleRate) || ~isscalar(opts.SampleRate) || opts.SampleRate <= 0
      error('Nelson:timetable:invalidValue:sampleRateAPositiveNumericScalar', 'SampleRate must be a positive numeric scalar.');
    end
    step = seconds(1 / double(opts.SampleRate));
  else
    step = opts.TimeStep;
    if ~isa(step, 'duration') || ~isscalar(step)
      error('Nelson:timetable:timeStepNotDuration', 'TimeStep must be a duration scalar.');
    end
  end
  values = 0:(nRows - 1);
  rowTimes = startTime + values(:) .* step;
end
%=============================================================================
