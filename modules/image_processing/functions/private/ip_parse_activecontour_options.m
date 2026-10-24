%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [iterations, method, smoothFactor, contractionBias] = ip_parse_activecontour_options(args)
  iterations = 100;
  method = 'chan-vese';
  smoothFactor = [];
  contractionBias = [];
  pos = 3;
  if pos <= numel(args) && ~(ischar(args{pos}) || isstring(args{pos}))
    iterations = double(args{pos});
    pos = pos + 1;
  end
  if pos <= numel(args) && (ischar(args{pos}) || isstring(args{pos}))
    text = lower(convertStringsToChars(args{pos}));
    if strcmp(text, 'chan-vese') || strcmp(text, 'edge')
      method = text;
      pos = pos + 1;
    elseif pos == numel(args) && ~ip_is_activecontour_option(text)
      method = text;
      pos = pos + 1;
    end
  elseif pos <= numel(args)
    error('Nelson:image_processing:activeContourMethodString', '%s must be a string.', 'Active contour method');
  end
  [iterations, method, smoothFactor, contractionBias] = ip_parse_activecontour_name_values(...
    args, pos, iterations, method, smoothFactor, contractionBias);
  ip_validate_activecontour_iterations(iterations);
  [smoothFactor, contractionBias] = ip_activecontour_method_defaults(...
    method, smoothFactor, contractionBias);
end
%=============================================================================
function tf = ip_is_activecontour_option(name)
  tf = strcmp(name, 'smoothfactor') || strcmp(name, 'contractionbias') ...
    || strcmp(name, 'iterations') || strcmp(name, 'method');
end
%=============================================================================
function [iterations, method, smoothFactor, contractionBias] = ip_parse_activecontour_name_values(...
  args, pos, iterations, method, smoothFactor, contractionBias)
  while pos <= numel(args)
    if pos == numel(args)
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    if ~(ischar(args{pos}) || isstring(args{pos}))
      error('Nelson:image_processing:activecontourOptionNameString', '%s option name must be a string.', 'activecontour');
    end
    name = lower(convertStringsToChars(args{pos}));
    value = args{pos + 1};
    switch name
      case 'iterations'
        iterations = double(value);
        ip_validate_activecontour_iterations(iterations);
      case 'method'
        method = ip_validate_activecontour_method(value);
      case 'smoothfactor'
        smoothFactor = ip_validate_activecontour_scalar(value, ...
          sprintf(_('%s must be a nonnegative finite scalar.'), 'Smooth factor'), 0, Inf);
      case 'contractionbias'
        contractionBias = ip_validate_activecontour_scalar(value, ...
          sprintf(_('%s must be a finite scalar in the range [%s].'), 'Contraction bias', '-1, 1'), -1, 1);
      otherwise
        error('Nelson:image_processing:unsupportedActiveContourOption', 'Unsupported "%s" option.', 'active contour');
    end
    pos = pos + 2;
  end
end
%=============================================================================
function method = ip_validate_activecontour_method(method)
  if ~(ischar(method) || isstring(method))
    error('Nelson:image_processing:activeContourMethodString', '%s must be a string.', 'Active contour method');
  end
  method = lower(convertStringsToChars(method));
  if ~(strcmp(method, 'chan-vese') || strcmp(method, 'edge'))
    error('Nelson:image_processing:unsupportedActiveContourMethod', 'Unsupported "%s" option.', 'active contour method');
  end
end
%=============================================================================
function value = ip_validate_activecontour_scalar(value, msg, minValue, maxValue)
  if ~(isnumeric(value) || islogical(value)) || ~isreal(value) || ~isscalar(value)
    error('Nelson:image_processing:invalidType', msg);
  end
  value = double(value);
  if ~(isfinite(value) && value >= minValue && value <= maxValue)
    error('Nelson:image_processing:mustBeInRange', msg);
  end
end
%=============================================================================
function ip_validate_activecontour_iterations(iterations)
  if ~(isscalar(iterations) && isfinite(iterations) && iterations >= 0 && iterations == floor(iterations))
    error('Nelson:image_processing:numberIterationsNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'Number of iterations');
  end
end
%=============================================================================
function [smoothFactor, contractionBias] = ip_activecontour_method_defaults(...
  method, smoothFactor, contractionBias)
  if isempty(smoothFactor)
    if strcmp(method, 'edge')
      smoothFactor = 1;
    else
      smoothFactor = 0;
    end
  end
  if isempty(contractionBias)
    if strcmp(method, 'edge')
      contractionBias = 0.3;
    else
      contractionBias = 0;
    end
  end
end
%=============================================================================
