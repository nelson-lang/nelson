%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imtranslate(varargin)
  narginchk(2, 6);
  nargoutchk(0, 1);
  img = varargin{1};
  ip_assert_full_image(img, true);
  if ~isnumeric(varargin{2}) || ~isreal(varargin{2}) || numel(varargin{2}) ~= 2
    error('Nelson:image_processing:translationVectorTwoElements', 'Translation vector must have two elements.');
  end
  t = double(varargin{2});
  if any(~isfinite(t(:)))
    error('Nelson:image_processing:translationVectorContainFiniteValues', '%s must contain finite values.', 'Translation vector');
  end
  t = t(:).';
  method = 'bilinear';
  fillValue = 0;
  outputView = 'same';
  startIndex = 3;
  if nargin >= 3
    if ~(ischar(varargin{3}) || isStringScalar(varargin{3}))
      error('Nelson:image_processing:imtranslateOptionNameString', '%s option name must be a string.', 'imtranslate');
    end
    firstOption = lower(convertStringsToChars(varargin{3}));
    if strcmp(firstOption, 'nearest') || strcmp(firstOption, 'linear') ...
        || strcmp(firstOption, 'bilinear') || strcmp(firstOption, 'cubic')
      method = firstOption;
      startIndex = 4;
    end
  end
  for k = startIndex:2:nargin
    if k == nargin
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    if ~(ischar(varargin{k}) || isStringScalar(varargin{k}))
      error('Nelson:image_processing:imtranslateOptionNameString', '%s option name must be a string.', 'imtranslate');
    end
    name = lower(convertStringsToChars(varargin{k}));
    value = varargin{k + 1};
    if strcmp(name, 'fillvalues')
      fillValue = value;
    elseif strcmp(name, 'interpolation')
      if ~(ischar(value) || isStringScalar(value))
        error('Nelson:image_processing:interpolationMethodString', '%s must be a string.', 'Interpolation method');
      end
      method = lower(convertStringsToChars(value));
    elseif strcmp(name, 'outputview')
      if ~(ischar(value) || isStringScalar(value))
        error('Nelson:image_processing:outputviewString', '%s must be a string.', 'OutputView');
      end
      outputView = lower(convertStringsToChars(value));
    else
      error('Nelson:image_processing:unsupportedImtranslateOption', 'Unsupported "%s" option.', 'imtranslate');
    end
  end
  if ~(strcmp(method, 'nearest') || strcmp(method, 'linear') || strcmp(method, 'bilinear') ...
      || strcmp(method, 'cubic'))
    error('Nelson:image_processing:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
  end
  if ~(strcmp(outputView, 'same') || strcmp(outputView, 'full'))
    error('Nelson:image_processing:unsupportedOutputView', 'Unsupported "%s" option.', 'output view');
  end
  origin = [0 0];
  outSize = size(img);
  if strcmp(outputView, 'full')
    origin = floor(min([0 0; t], [], 1));
    extra = ceil(max([0 0; t], [], 1)) - origin;
    outSize(1) = size(img, 1) + extra(2);
    outSize(2) = size(img, 2) + extra(1);
  end
  shiftedT = t - origin;
  T = [1 0 0; 0 1 0; shiftedT(1) shiftedT(2) 1];
  out = imwarp(img, T, 'Interpolation', method, 'FillValues', fillValue, 'OutputView', outSize);
end
%=============================================================================
