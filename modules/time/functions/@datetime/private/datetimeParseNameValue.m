%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [args, options] = datetimeParseNameValue(values)
  % Split datetime constructor inputs into positional arguments and options.
  % HasTimeZone tells a given TimeZone (even '') from the default one.
  options = defaultOptions();
  args = {};
  k = 1;
  while k <= numel(values)
    if isOptionName(values{k})
      name = char(values{k});
      if any(strcmpi(name, {'Format', 'TimeZone', 'InputFormat', 'ConvertFrom', 'Epoch', ...
          'TicksPerSecond', 'Locale', 'PivotYear'}))
        if k == numel(values)
          error('Nelson:time:missingValueNameValueArgument', 'Missing value for name-value argument.');
        end
        options = setOption(options, lower(name), values{k + 1});
        k = k + 2;
        continue
      end
    end
    args{end + 1} = values{k};
    k = k + 1;
  end
end
%=============================================================================
function tf = isOptionName(value)
  % a char row or a non-missing string scalar may name an option
  tf = (ischar(value) && isrow(value)) || (isstring(value) && isscalar(value) && ~ismissing(value));
end
%=============================================================================
function options = defaultOptions()
  options.Format = 'default';
  options.TimeZone = '';
  options.HasTimeZone = false;
  options.InputFormat = '';
  options.ConvertFrom = '';
  options.Epoch = datetime.fromSerial(datenum(1970, 1, 1), 'default', '');
  options.TicksPerSecond = 1;
  options.Locale = 'en_US';
  % two-digit years ('y', 'yy') fall in the 100 years from PivotYear
  options.PivotYear = [];
end
%=============================================================================
function options = setOption(options, name, value)
  % text options are stored as given
  textOptions = {'Format', 'InputFormat', 'ConvertFrom', 'Locale'};
  match = strcmpi(textOptions, name);
  if any(match)
    field = textOptions{match};
    options.(field) = datetime.validateTextScalar(value, field);
    return
  end
  switch name
    case 'timezone'
      options.TimeZone = datetimeTimeZoneName(value);
      options.HasTimeZone = true;
    case 'epoch'
      if isa(value, 'datetime')
        options.Epoch = value;
      else
        options.Epoch = datetime(value);
      end
    case 'tickspersecond'
      options.TicksPerSecond = double(value);
    case 'pivotyear'
      options.PivotYear = pivotYearValue(value);
  end
end
%=============================================================================
function value = pivotYearValue(value)
  if ~(isnumeric(value) && isscalar(value) && isreal(value) && value == fix(value))
    error('Nelson:datetime:InvalidPivotYear', 'The pivot year must be an integer scalar.');
  end
  value = double(value);
end
%=============================================================================
