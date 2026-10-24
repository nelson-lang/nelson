%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = ip_parse_imgaussfilt3_options(args)
  options = struct('Sigma', [0.5 0.5 0.5], 'FilterSize', [3 3 3], ...
    'Padding', 'replicate');
  index = 1;
  if numel(args) >= 1 && ~(ischar(args{1}) || isstring(args{1}))
    options.Sigma = ip_parse_imgaussfilt3_sigma(args{1});
    options.FilterSize = 2 * ceil(2 * options.Sigma) + 1;
    index = 2;
  end
  while index <= numel(args)
    if index + 1 > numel(args)
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    name = args{index};
    if ~(ischar(name) || isstring(name))
      error('Nelson:image_processing:unsupportedImgaussfilt3Option', 'Unsupported "%s" option.', 'imgaussfilt3');
    end
    name = lower(convertStringsToChars(name));
    value = args{index + 1};
    if strcmp(name, 'filtersize')
      options.FilterSize = ip_parse_imgaussfilt3_filter_size(value);
    elseif strcmp(name, 'padding')
      options.Padding = ip_parse_imgaussfilt3_padding(value);
    elseif strcmp(name, 'filterdomain')
      ip_validate_imgaussfilt3_domain(value);
    else
      error('Nelson:image_processing:unsupportedImgaussfilt3Option', 'Unsupported "%s" option.', 'imgaussfilt3');
    end
    index = index + 2;
  end
end
%=============================================================================
function sigma = ip_parse_imgaussfilt3_sigma(value)
  if ~isnumeric(value) || ~isreal(value) || issparse(value)
    error('Nelson:image_processing:sigmaContainPositiveFiniteValues', '%s must contain positive finite values.', 'Sigma');
  end
  sigma = double(value(:).');
  if ~(numel(sigma) == 1 || numel(sigma) == 3)
    error('Nelson:image_processing:sigmaScalarElementVector', 'Sigma must be a scalar or %d-element vector.', 3);
  end
  if any(~isfinite(sigma)) || any(sigma <= 0)
    error('Nelson:image_processing:sigmaContainPositiveFiniteValues', '%s must contain positive finite values.', 'Sigma');
  end
  if numel(sigma) == 1
    sigma = [sigma sigma sigma];
  end
end
%=============================================================================
function filterSize = ip_parse_imgaussfilt3_filter_size(value)
  if ~isnumeric(value) || ~isreal(value) || issparse(value)
    error('Nelson:image_processing:filterSizeContainPositiveOddIntegers', '%s size must contain positive odd integers.', 'Filter');
  end
  filterSize = double(value(:).');
  if numel(filterSize) == 1
    filterSize = [filterSize filterSize filterSize];
  end
  if numel(filterSize) ~= 3 || any(~isfinite(filterSize)) ...
      || any(filterSize ~= floor(filterSize)) || any(filterSize < 1) ...
      || any(mod(filterSize, 2) == 0)
    error('Nelson:image_processing:filterSizeContainPositiveOddIntegers', '%s size must contain positive odd integers.', 'Filter');
  end
end
%=============================================================================
function padding = ip_parse_imgaussfilt3_padding(value)
  if ischar(value) || isstring(value)
    padding = lower(convertStringsToChars(value));
    if ~any(strcmp(padding, {'replicate', 'symmetric', 'circular'}))
      error('Nelson:image_processing:unsupportedPaddingOption', 'Unsupported "%s" option.', 'padding');
    end
  elseif (isnumeric(value) || islogical(value)) && isscalar(value) && isreal(value) ...
      && ~issparse(value)
    padding = double(value);
    if ~isfinite(padding)
      error('Nelson:validators:mustBeFinite', '%s must be finite.', 'Padding value');
    end
  else
    error('Nelson:image_processing:unsupportedPaddingOption', 'Unsupported "%s" option.', 'padding');
  end
end
%=============================================================================
function ip_validate_imgaussfilt3_domain(value)
  if ~(ischar(value) || isstring(value))
    error('Nelson:image_processing:unsupportedImgaussfilt3Option', 'Unsupported "%s" option.', 'imgaussfilt3');
  end
  domain = lower(convertStringsToChars(value));
  if ~any(strcmp(domain, {'auto', 'spatial', 'frequency'}))
    error('Nelson:image_processing:unsupportedImgaussfilt3Option', 'Unsupported "%s" option.', 'imgaussfilt3');
  end
end
%=============================================================================
