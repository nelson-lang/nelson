%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [out, refOut] = imwarp(varargin)
  narginchk(2, 10);
  nargoutchk(0, 2);
  img = varargin{1};
  if ip_imwarp_is_3d_call(varargin)
    [out, refOut] = ip_imwarp3d_entry(varargin);
    return
  end
  ip_assert_full_image(img, true, true);
  srcRef = imref2d([size(img, 1) size(img, 2)]);
  if nargin >= 3 && isstruct(varargin{2}) && isfield(varargin{2}, 'ImageSize') ...
      && isfield(varargin{2}, 'XWorldLimits') && isfield(varargin{2}, 'YWorldLimits')
    srcRef = ip_validate_imref2d(varargin{2});
    if ~isequal(srcRef.ImageSize, [size(img, 1) size(img, 2)])
      error('Nelson:image_processing:spatialReferenceImageSizeMatchImage', 'Spatial reference image size must match image size.');
    end
    tform = ip_tform_matrix(varargin{3});
    startIndex = 4;
  else
    tform = ip_tform_matrix(varargin{2});
    startIndex = 3;
  end
  method = 'bilinear';
  fillValue = 0;
  outputView = 'full';
  if nargin >= startIndex
    if ~(ischar(varargin{startIndex}) || isStringScalar(varargin{startIndex}))
      error('Nelson:image_processing:imwarpOptionNameString', '%s option name must be a string.', 'imwarp');
    end
    firstOption = lower(convertStringsToChars(varargin{startIndex}));
    if strcmp(firstOption, 'nearest') || strcmp(firstOption, 'linear') ...
        || strcmp(firstOption, 'bilinear') || strcmp(firstOption, 'cubic')
      method = firstOption;
      startIndex = startIndex + 1;
    end
  end
  for k = startIndex:2:nargin
    if k == nargin
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    if ~(ischar(varargin{k}) || isStringScalar(varargin{k}))
      error('Nelson:image_processing:imwarpOptionNameString', '%s option name must be a string.', 'imwarp');
    end
    name = lower(convertStringsToChars(varargin{k}));
    value = varargin{k + 1};
    if strcmp(name, 'interpolation')
      if ~(ischar(value) || isStringScalar(value))
        error('Nelson:image_processing:interpolationMethodString', '%s must be a string.', 'Interpolation method');
      end
      method = lower(convertStringsToChars(value));
    elseif strcmp(name, 'fillvalues')
      fillValue = value;
    elseif strcmp(name, 'outputview')
      outputView = value;
    else
      error('Nelson:image_processing:unsupportedImwarpOption', 'Unsupported "%s" option.', 'imwarp');
    end
  end
  if ~(strcmp(method, 'nearest') || strcmp(method, 'linear') || strcmp(method, 'bilinear') ...
      || strcmp(method, 'cubic'))
    error('Nelson:image_processing:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
  end
  refOut = ip_resolve_output_ref(outputView, srcRef, tform);
  outSize = refOut.ImageSize;
  imgSize = size(img);
  channels = 1;
  if length(imgSize) == 3
    channels = imgSize(3);
  end
  if ~(isnumeric(fillValue) || islogical(fillValue)) || ~isreal(fillValue)
    error('Nelson:image_processing:fillValuesNumericLogicalReal', 'Fill values must be numeric or logical and real.');
  end
  fillValue = double(fillValue);
  if any(~isfinite(fillValue(:)))
    error('Nelson:image_processing:fillValuesFinite', '%s values must be finite.', 'Fill');
  end
  if numel(fillValue) == 1
    fillValues = repmat(fillValue, 1, channels);
  elseif numel(fillValue) == channels
    fillValues = reshape(fillValue, 1, channels);
  else
    error('Nelson:image_processing:fillValuesScalarMatchNumberChannels', 'Fill values must be scalar or match the number of channels.');
  end
  srcDx = srcRef.PixelExtentInWorldX;
  srcDy = srcRef.PixelExtentInWorldY;
  outDx = refOut.PixelExtentInWorldX;
  outDy = refOut.PixelExtentInWorldY;
  out = builtin('__imwarp_kernel__', img, inv(tform), ...
    [srcRef.XWorldLimits(1), srcRef.YWorldLimits(1)], [srcDx srcDy], ...
    [refOut.XWorldLimits(1), refOut.YWorldLimits(1)], [outDx outDy], ...
    outSize, method, fillValues);
end
%=============================================================================
function tf = ip_imwarp_is_3d_call(args)
  tf = false;
  if numel(args) < 2
    return
  end
  if isstruct(args{2}) && isfield(args{2}, 'ZWorldLimits')
    tf = true;
    return
  end
  value = args{2};
  if isstruct(value) && isfield(value, 'Dimensionality') && value.Dimensionality == 3
    tf = true;
  elseif isstruct(value) && isfield(value, 'T') && isequal(size(value.T), [4 4])
    tf = true;
  elseif isnumeric(value) && (isequal(size(value), [4 4]) || isequal(size(value), [3 4]))
    tf = true;
  end
end
%=============================================================================
