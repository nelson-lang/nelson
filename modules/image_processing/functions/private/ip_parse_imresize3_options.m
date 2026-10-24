%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [outputSize, options] = ip_parse_imresize3_options(args, inputSize)
  options = struct('Method', 'linear', 'Antialiasing', true);
  outputSize = ip_parse_imresize3_size(args{1}, inputSize);
  index = 2;
  if index <= numel(args) && (ischar(args{index}) || isStringScalar(args{index}))
    first = lower(convertStringsToChars(args{index}));
    if ip_is_imresize3_method(first)
      options.Method = first;
      index = index + 1;
    elseif numel(args) == index
      error('Nelson:image_processing:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
    end
  end
  while index <= numel(args)
    if index + 1 > numel(args)
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    name = args{index};
    if ~(ischar(name) || isStringScalar(name))
      error('Nelson:image_processing:imresize3OptionNameString', '%s option name must be a string.', 'imresize3');
    end
    name = lower(convertStringsToChars(name));
    value = args{index + 1};
    if strcmp(name, 'method')
      options.Method = ip_parse_imresize3_method(value);
    elseif strcmp(name, 'antialiasing')
      options.Antialiasing = ip_parse_imresize3_antialiasing(value);
    else
      error('Nelson:image_processing:unsupportedImresize3Option', 'Unsupported "%s" option.', 'imresize3');
    end
    index = index + 2;
  end
end
%=============================================================================
function outputSize = ip_parse_imresize3_size(value, inputSize)
  if ~isnumeric(value) || ~isreal(value) || issparse(value)
    error('Nelson:image_processing:scaleFactorOutputSizeNumericReal', 'Scale factor or output size must be numeric and real.');
  end
  value = double(value(:).');
  if numel(value) == 1
    if ~isfinite(value) || value <= 0
      error('Nelson:image_processing:scaleFactorPositiveFinite', 'Scale factor must be positive and finite.');
    end
    outputSize = max(round(inputSize * value), 1);
  elseif numel(value) == 3
    if any(~isfinite(value)) || any(value <= 0)
      error('Nelson:image_processing:outputSizeContainPositiveFiniteValues', '%s must contain positive finite values.', 'Output size');
    end
    outputSize = max(round(value), 1);
  else
    error('Nelson:image_processing:scaleFactorScalarOutputSizeElement', 'Scale factor must be a scalar or output size a 3-element vector.');
  end
end
%=============================================================================
function method = ip_parse_imresize3_method(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:image_processing:interpolationMethodString', '%s must be a string.', 'Interpolation method');
  end
  method = lower(convertStringsToChars(value));
  if ~ip_is_imresize3_method(method)
    error('Nelson:image_processing:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
  end
end
%=============================================================================
function tf = ip_is_imresize3_method(method)
  tf = strcmp(method, 'nearest') || strcmp(method, 'linear');
end
%=============================================================================
function antialiasing = ip_parse_imresize3_antialiasing(value)
  if ischar(value) || isStringScalar(value)
    value = lower(convertStringsToChars(value));
    if strcmp(value, 'on') || strcmp(value, 'true')
      antialiasing = true;
    elseif strcmp(value, 'off') || strcmp(value, 'false')
      antialiasing = false;
    else
      error('Nelson:image_processing:antialiasingLogicalOff', 'Antialiasing must be logical or on/off.');
    end
  elseif (islogical(value) || isnumeric(value)) && isscalar(value) && isreal(value)
    value = double(value);
    if ~isfinite(value)
      error('Nelson:image_processing:antialiasingLogicalOff', 'Antialiasing must be logical or on/off.');
    end
    antialiasing = value ~= 0;
  else
    error('Nelson:image_processing:antialiasingLogicalOff', 'Antialiasing must be logical or on/off.');
  end
end
%=============================================================================
