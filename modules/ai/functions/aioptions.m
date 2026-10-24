%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = aioptions(varargin)
  if mod(nargin, 2) ~= 0
    error('Nelson:aioptions:nameValueArgumentsExpected2', 'Name-value arguments expected.');
  end
  opts = struct();
  opts.Provider = 'ollama';
  opts.Model = 'llama3';
  opts.Endpoint = 'http://127.0.0.1:11434/api/generate';
  opts.TokenEnvVar = '';
  opts.Timeout = 120;
  opts.SystemPrompt = '';
  opts.Think = [];
  opts.NumPredict = [];
  opts.Temperature = [];
  for k = 1:2:nargin
    name = varargin{k};
    if isstring(name)
      name = char(name);
    end
    if ~ischar(name) || size(name, 1) ~= 1
      error('Nelson:aioptions:optionNameACharacterVector2', 'Option name must be a character vector or string scalar.');
    end
    value = varargin{k + 1};
    switch lower(name)
      case 'provider'
        opts.Provider = getTextScalar(value, 'Provider');
      case 'model'
        opts.Model = getTextScalar(value, 'Model');
      case 'endpoint'
        opts.Endpoint = getTextScalar(value, 'Endpoint');
      case 'tokenenvvar'
        opts.TokenEnvVar = getTextScalar(value, 'TokenEnvVar');
      case 'timeout'
        if ~isnumeric(value) || ~isscalar(value) || value <= 0
          error('Nelson:aioptions:timeoutAPositiveNumericScalar2', 'Timeout must be a positive numeric scalar.');
        end
        opts.Timeout = double(value);
      case 'systemprompt'
        opts.SystemPrompt = getTextScalar(value, 'SystemPrompt');
      case 'think'
        if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
          error('Nelson:aioptions:thinkALogicalScalar2', 'Think must be a logical scalar.');
        end
        opts.Think = logical(value);
      case 'numpredict'
        if ~isnumeric(value) || ~isscalar(value) || value <= 0
          error('Nelson:aioptions:numPredictAPositiveNumericScalar2', 'NumPredict must be a positive numeric scalar.');
        end
        opts.NumPredict = double(value);
      case 'temperature'
        if ~isnumeric(value) || ~isscalar(value) || value < 0
          error('Nelson:aioptions:temperatureANonnegativeNumericScalar2', 'Temperature must be a nonnegative numeric scalar.');
        end
        opts.Temperature = double(value);
      otherwise
        error('Nelson:aioptions:unsupportedOptionName2', 'Unsupported option name.');
    end
  end
end
%=============================================================================
function txt = getTextScalar(value, name)
  if isstring(value)
    if ~isscalar(value)
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
    end
    txt = char(value);
    return
  end
  if ~ischar(value) || size(value, 1) ~= 1
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  txt = value;
end
%=============================================================================
