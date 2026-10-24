%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [options, runArguments] = tuneReuse_parseOptions(args)
  options = struct('Apply', false, 'AllowAdd', false, 'Trials', 3, 'MinGain', 0.1);
  runArguments = {};
  allowedRunOptions = {'Name', 'Module', 'File', 'Kind', 'Tags', ...
    'ExcludeTags', 'Match', 'Exclude', 'Timeout'};
  if mod(length(args), 2) ~= 0
    error('Nelson:tests_manager:optionValueExpected', 'Option value expected.');
  end
  for k = 1:2:length(args)
    name = args{k};
    value = args{k + 1};
    if isfield(options, name)
      options.(name) = value;
    elseif any(strcmp(allowedRunOptions, name))
      runArguments = [runArguments, {name, value}];
    else
      error('Nelson:tests_manager:unsupportedOption', 'Unsupported option.');
    end
  end
  tuneReuse_validateLogical(options.Apply, 'Apply');
  tuneReuse_validateLogical(options.AllowAdd, 'AllowAdd');
  tuneReuse_validateTrials(options.Trials);
  tuneReuse_validateMinGain(options.MinGain);
  options.Trials = double(options.Trials);
  options.MinGain = double(options.MinGain);
end
%=============================================================================
function tuneReuse_validateLogical(value, name)
  if ~(islogical(value) && isscalar(value))
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
end
%=============================================================================
function tuneReuse_validateTrials(value)
  if ~(isnumeric(value) && isscalar(value) && isfinite(value) ...
    && value >= 1 && fix(value) == value)
    error('Nelson:tests_manager:invalidTrials', 'Trials must be a positive integer.');
  end
end
%=============================================================================
function tuneReuse_validateMinGain(value)
  if ~(isnumeric(value) && isscalar(value) && isfinite(value) ...
    && value >= -1 && value <= 1)
    error('Nelson:tests_manager:invalidMinGain', 'MinGain must be between -1 and 1.');
  end
end
%=============================================================================
