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
    if datetime.isTextScalar(values{k})
      name = char(values{k});
      if any(strcmpi(name, {'Format', 'TimeZone', 'InputFormat', 'ConvertFrom', 'Epoch', ...
          'TicksPerSecond', 'Locale'}))
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
function options = defaultOptions()
  options.Format = 'default';
  options.TimeZone = '';
  options.HasTimeZone = false;
  options.InputFormat = '';
  options.ConvertFrom = '';
  options.Epoch = datetime.fromSerial(datenum(1970, 1, 1), 'default', '');
  options.TicksPerSecond = 1;
  options.Locale = 'en_US';
end
%=============================================================================
function options = setOption(options, name, value)
  switch name
    case 'format'
      options.Format = datetime.validateTextScalar(value, 'Format');
    case 'timezone'
      options.TimeZone = datetimeTimeZoneName(value);
      options.HasTimeZone = true;
    case 'inputformat'
      options.InputFormat = datetime.validateTextScalar(value, 'InputFormat');
    case 'convertfrom'
      options.ConvertFrom = datetime.validateTextScalar(value, 'ConvertFrom');
    case 'epoch'
      if isa(value, 'datetime')
        options.Epoch = value;
      else
        options.Epoch = datetime(value);
      end
    case 'tickspersecond'
      options.TicksPerSecond = double(value);
    case 'locale'
      options.Locale = datetime.validateTextScalar(value, 'Locale');
  end
end
%=============================================================================
