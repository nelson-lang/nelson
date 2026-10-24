%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function parsed = timeseriesParseConstructor(varargin)
  parsed = struct();
  parsed.Data = [];
  parsed.Time = [];
  parsed.Quality = [];
  parsed.Name = '';
  parsed.UserData = [];
  parsed.DataInfo = tsdata.datametadata();
  parsed.TimeInfo = tsdata.timemetadata();
  parsed.QualityInfo = tsdata.qualmetadata();
  parsed.TreatNaNasMissing = true;
  parsed.IsTimeFirst = true;

  [args, opts] = splitNameValues(varargin);
  if length(args) >= 1
    parsed.Data = args{1};
  end
  if length(args) >= 2
    parsed.Time = args{2};
  end
  if length(args) >= 3
    parsed.Quality = args{3};
  end
  if length(args) > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end

  isTimeFirstSpecified = false;
  if ~isempty(parsed.Data) && ndims(parsed.Data) > 2
    parsed.IsTimeFirst = false;
  end
  for k = 1:2:length(opts)
    name = lower(timeseriesText(opts{k}, 'Property name'));
    value = opts{k + 1};
    switch name
      case 'name'
        parsed.Name = timeseriesText(value, 'Name');
      case 'userdata'
        parsed.UserData = value;
      case 'datainfo'
        parsed.DataInfo = value;
      case 'timeinfo'
        parsed.TimeInfo = value;
      case 'qualityinfo'
        parsed.QualityInfo = value;
      case 'treatnanasmissing'
        parsed.TreatNaNasMissing = logical(value);
      case 'istimefirst'
        parsed.IsTimeFirst = logical(value);
        isTimeFirstSpecified = true;
      otherwise
        error('Nelson:time:unsupportedTimeseriesOption', 'Unsupported %s.', 'timeseries option');
    end
  end
  if ~isTimeFirstSpecified && ~isempty(parsed.Data) && ndims(parsed.Data) > 2
    parsed.IsTimeFirst = false;
  end
  n = timeseriesLengthFromData(parsed.Data, parsed.IsTimeFirst);
  if isempty(parsed.Time) && n > 0
    parsed.Time = (0:n - 1).';
  end
end
%=============================================================================
function [args, opts] = splitNameValues(values)
  firstOption = length(values) + 1;
  k = 1;
  while k <= length(values)
    if (ischar(values{k}) || isstring(values{k})) && k < length(values)
      name = lower(char(values{k}));
      if any(strcmp(name, {'name', 'userdata', 'datainfo', 'timeinfo', 'qualityinfo', 'treatnanasmissing', 'istimefirst'}))
        firstOption = k;
        break
      end
    end
    k = k + 1;
  end
  args = values(1:firstOption - 1);
  opts = values(firstOption:end);
  if rem(length(opts), 2) ~= 0
    error('Nelson:time:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
  end
end
%=============================================================================
