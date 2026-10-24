%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [options, runArguments] = tuneWeights_parseOptions(args)
  options = struct('Apply', false, 'MaxWeight', 64);
  runArguments = {};
  if mod(length(args), 2) ~= 0
    error('Nelson:tests_manager:optionValueExpected', 'Option value expected.');
  end
  for k = 1:2:length(args)
    name = args{k};
    value = args{k + 1};
    if isfield(options, name)
      options.(name) = value;
    else
      runArguments = [runArguments, {name, value}];
    end
  end
  tuneWeights_validateApply(options.Apply);
  tuneWeights_validateMaxWeight(options.MaxWeight);
  options.MaxWeight = double(options.MaxWeight);
end
%=============================================================================
function tuneWeights_validateApply(value)
  if ~(islogical(value) && isscalar(value))
    error('Nelson:tests_manager:invalidApply', 'Apply must be a logical scalar.');
  end
end
%=============================================================================
function tuneWeights_validateMaxWeight(value)
  if ~(isnumeric(value) && isscalar(value) && isfinite(value) ...
    && value >= 1 && fix(value) == value)
    error('Nelson:tests_manager:invalidMaxWeight', 'MaxWeight must be a positive integer.');
  end
end
%=============================================================================
