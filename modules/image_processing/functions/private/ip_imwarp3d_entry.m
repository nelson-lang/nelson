%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [out, refOut] = ip_imwarp3d_entry(args)
  volume = args{1};
  ip_validate_imwarp3d_volume(volume);
  if numel(args) >= 3 && isstruct(args{2}) && isfield(args{2}, 'ZWorldLimits')
    srcRef = ip_validate_imref3d(args{2});
    if ~isequal(srcRef.ImageSize, [size(volume, 1), size(volume, 2), size(volume, 3)])
      error('Nelson:image_processing:spatialReferenceImageSizeMatchImage', 'Spatial reference image size must match image size.');
    end
    tform = ip_tform_matrix3d(args{3});
    startIndex = 4;
  else
    srcRef = imref3d([size(volume, 1), size(volume, 2), size(volume, 3)]);
    tform = ip_tform_matrix3d(args{2});
    startIndex = 3;
  end
  [method, fillValue, outputView] = ip_parse_imwarp3d_options(args, startIndex, srcRef);
  refOut = ip_resolve_output_ref3d(outputView, srcRef, tform);
  out = ip_imwarp3d(volume, srcRef, tform, refOut, method, fillValue);
end
%=============================================================================
function ip_validate_imwarp3d_volume(volume)
  if ~(isnumeric(volume) || islogical(volume))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Image');
  end
  if issparse(volume)
    error('Nelson:image_processing:sparseImagesNotSupported', '%s are not supported.', 'Sparse images');
  end
  if length(size(volume)) ~= 3
    error('Nelson:image_processing:volumeDArray', '%s must be a 3-D array.', 'Volume');
  end
end
%=============================================================================
function [method, fillValue, outputView] = ip_parse_imwarp3d_options(args, startIndex, srcRef)
  method = 'linear';
  fillValue = 0;
  outputView = 'full';
  if numel(args) >= startIndex
    if ~(ischar(args{startIndex}) || isStringScalar(args{startIndex}))
      error('Nelson:image_processing:imwarpOptionNameString', '%s option name must be a string.', 'imwarp');
    end
    firstOption = lower(convertStringsToChars(args{startIndex}));
    if strcmp(firstOption, 'nearest') || strcmp(firstOption, 'linear') ...
        || strcmp(firstOption, 'bilinear')
      method = firstOption;
      startIndex = startIndex + 1;
    elseif strcmp(firstOption, 'cubic')
      error('Nelson:image_processing:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
    end
  end
  for k = startIndex:2:numel(args)
    if k == numel(args)
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    if ~(ischar(args{k}) || isStringScalar(args{k}))
      error('Nelson:image_processing:imwarpOptionNameString', '%s option name must be a string.', 'imwarp');
    end
    name = lower(convertStringsToChars(args{k}));
    value = args{k + 1};
    if strcmp(name, 'interpolation')
      if ~(ischar(value) || isStringScalar(value))
        error('Nelson:image_processing:interpolationMethodString', '%s must be a string.', 'Interpolation method');
      end
      method = lower(convertStringsToChars(value));
      if strcmp(method, 'bilinear')
        method = 'linear';
      end
    elseif strcmp(name, 'fillvalues')
      fillValue = value;
    elseif strcmp(name, 'outputview')
      outputView = value;
    else
      error('Nelson:image_processing:unsupportedImwarpOption', 'Unsupported "%s" option.', 'imwarp');
    end
  end
  if ~(strcmp(method, 'nearest') || strcmp(method, 'linear'))
    error('Nelson:image_processing:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
  end
end
%=============================================================================
