%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = options(varargin)
  if mod(nargin, 2) ~= 0
    error('Nelson:aioptions:nameValueArgumentsExpected2', 'Name-value arguments expected.');
  end
  opts = defaultOptions();
  for k = 1:2:nargin
    opts = applyOption(opts, varargin{k}, varargin{k + 1});
  end
end
%=============================================================================
function opts = defaultOptions()
  opts = struct();
  opts.Provider = environmentOr('NELSON_AI_PROVIDER', 'ollama');
  opts.Model = environmentOr('NELSON_AI_MODEL', 'llama3');
  opts.Endpoint = environmentOr('NELSON_AI_ENDPOINT', 'http://127.0.0.1:11434/api/generate');
  opts.TokenEnvVar = environmentOr('NELSON_AI_TOKEN_ENVVAR', '');
  opts.Timeout = 120;
  opts.SystemPrompt = '';
  opts.Think = [];
  opts.NumPredict = [];
  opts.Temperature = [];
  opts.MaxSteps = [];
  opts.Stream = false;
end
%=============================================================================
function value = environmentOr(name, fallback)
  value = getenv(name);
  if isempty(value)
    value = fallback;
  end
end
%=============================================================================
function opts = applyOption(opts, name, value)
  if isstring(name)
    name = char(name);
  end
  if ~ischar(name) || size(name, 1) ~= 1
    error('Nelson:aioptions:optionNameACharacterVector2', 'Option name must be a character vector or string scalar.');
  end
  [opts, handled] = applyTextOption(opts, lower(name), value);
  if handled
    return
  end
  switch lower(name)
    case 'timeout'
      opts.Timeout = requirePositive(value, 'Nelson:aioptions:timeoutAPositiveNumericScalar2', ...
        'Timeout must be a positive numeric scalar.');
    case 'numpredict'
      opts.NumPredict = requirePositive(value, 'Nelson:aioptions:numPredictAPositiveNumericScalar2', ...
        'NumPredict must be a positive numeric scalar.');
    case 'temperature'
      opts.Temperature = requireNonnegative(value);
    case 'maxsteps'
      opts.MaxSteps = requirePositive(value, 'Nelson:aioptions:maxStepsAPositiveNumericScalar2', ...
        'MaxSteps must be a positive numeric scalar.');
    case 'think'
      opts.Think = requireLogical(value);
    case 'stream'
      opts.Stream = requireLogical(value);
    otherwise
      error('Nelson:aioptions:unsupportedOptionName2', 'Unsupported option name.');
  end
end
%=============================================================================
% The character-valued options share one validator; routing them here keeps the
% main switch small.
function [opts, handled] = applyTextOption(opts, name, value)
  fields = {'provider', 'Provider'; 'model', 'Model'; 'endpoint', 'Endpoint'; ...
    'tokenenvvar', 'TokenEnvVar'; 'systemprompt', 'SystemPrompt'};
  handled = false;
  for k = 1:size(fields, 1)
    if strcmp(name, fields{k, 1})
      opts.(fields{k, 2}) = getTextScalar(value, fields{k, 2});
      handled = true;
      return
    end
  end
end
%=============================================================================
function out = requirePositive(value, id, message)
  if ~isnumeric(value) || ~isscalar(value) || value <= 0
    error(id, message);
  end
  out = double(value);
end
%=============================================================================
function out = requireNonnegative(value)
  if ~isnumeric(value) || ~isscalar(value) || value < 0
    error('Nelson:aioptions:temperatureANonnegativeNumericScalar2', ...
      'Temperature must be a nonnegative numeric scalar.');
  end
  out = double(value);
end
%=============================================================================
function out = requireLogical(value)
  if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
    error('Nelson:aioptions:thinkALogicalScalar2', 'Think must be a logical scalar.');
  end
  out = logical(value);
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
