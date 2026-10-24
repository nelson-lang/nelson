%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imboxfilt(varargin)
  narginchk(1, 6);
  nargoutchk(0, 1);
  img = varargin{1};
  siz = [3 3];
  argIndex = 2;
  if nargin >= 2 && ~(ischar(varargin{2}) || isstring(varargin{2}))
    siz = varargin{2};
    argIndex = 3;
  end
  if ~isnumeric(siz) || ~isreal(siz)
    error('Nelson:image_processing:filterSizeContainPositiveIntegers', '%s size must contain positive integers.', 'Filter');
  end
  siz = double(siz);
  if numel(siz) == 1
    siz = [siz siz];
  end
  if numel(siz) ~= 2 || any(~isfinite(siz)) || any(siz ~= floor(siz)) || any(siz < 1)
    error('Nelson:image_processing:filterSizeContainPositiveIntegers', '%s size must contain positive integers.', 'Filter');
  end
  padMethod = 'replicate';
  normFactor = 1 / prod(double(siz));
  while argIndex <= nargin
    if argIndex == nargin
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    name = lower(convertStringsToChars(varargin{argIndex}));
    value = varargin{argIndex + 1};
    switch name
      case 'padding'
        padMethod = value;
      case 'normalizationfactor'
        if ~isnumeric(value) || ~isreal(value) || ~isscalar(value) || ~isfinite(double(value))
          error('Nelson:image_processing:normalizationFactorFiniteNumericScalar', '%s must be a finite numeric scalar.', 'Normalization factor');
        end
        normFactor = double(value);
      otherwise
        error('Nelson:image_processing:unsupportedImboxfiltOption', 'Unsupported "%s" option.', 'imboxfilt');
    end
    argIndex = argIndex + 2;
  end
  h = ones(siz(1), siz(2)) * normFactor;
  out = imfilter(img, h, padMethod);
end
%=============================================================================
