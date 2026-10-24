%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [components, imageSize, intensity, propertyArgs] = ip_regionprops3_components(args)
  input = args{1};
  [components, imageSize] = ip_regionprops3_components_from_input(input);
  intensity = [];
  propertyArgs = args(2:end);
  if numel(args) >= 2 && ip_regionprops3_is_intensity_argument(args{2})
    intensity = args{2};
    ip_regionprops3_validate_intensity(intensity, imageSize);
    propertyArgs = args(3:end);
  end
end
%=============================================================================
function tf = ip_regionprops3_is_intensity_argument(value)
  tf = (isnumeric(value) || islogical(value)) && ~issparse(value) && ~ischar(value) ...
    && ~isStringScalar(value) && length(size(value)) == 3;
end
%=============================================================================
function [components, imageSize] = ip_regionprops3_components_from_input(input)
  if isstruct(input)
    [components, imageSize] = ip_regionprops3_components_from_cc(input);
  elseif islogical(input)
    ip_regionprops3_validate_volume(input, false);
    cc = bwconncomp(input);
    [components, imageSize] = ip_regionprops3_components_from_cc(cc);
  else
    [components, imageSize] = ip_regionprops3_components_from_labels(input);
  end
end
%=============================================================================
function [components, imageSize] = ip_regionprops3_components_from_cc(cc)
  if ~isfield(cc, 'ImageSize') || ~isfield(cc, 'PixelIdxList')
    error('Nelson:image_processing:connectedComponentStructureContainImagesizePixelidxlist', 'Connected-component structure must contain ImageSize and PixelIdxList fields.');
  end
  imageSize = double(cc.ImageSize(:).');
  if numel(imageSize) ~= 3 || any(imageSize <= 0) || any(imageSize ~= floor(imageSize))
    error('Nelson:image_processing:connectedComponentImagesizeDescribeDVolume', 'Connected-component ImageSize must describe a 3-D volume.');
  end
  pixelIdxList = cc.PixelIdxList;
  if ~iscell(pixelIdxList)
    error('Nelson:image_processing:connectedComponentPixelidxlistCellArray', 'Connected-component PixelIdxList must be a cell array.');
  end
  components = cell(1, numel(pixelIdxList));
  maxIndex = prod(imageSize);
  for k = 1:numel(pixelIdxList)
    idx = double(pixelIdxList{k}(:));
    if any(~isfinite(idx)) || any(idx < 1) || any(idx > maxIndex) || any(idx ~= floor(idx))
      error('Nelson:image_processing:connectedComponentPixelIndicesInvalid', 'Connected-component pixel indices are invalid.');
    end
    components{k} = sort(idx);
  end
end
%=============================================================================
function [components, imageSize] = ip_regionprops3_components_from_labels(labels)
  ip_regionprops3_validate_volume(labels, true);
  values = double(labels);
  if any(~isfinite(values(:))) || any(values(:) < 0) || any(values(:) ~= floor(values(:)))
    error('Nelson:image_processing:labelVolumeValuesNonnegativeFiniteIntegers', '%s values must be nonnegative finite integers.', 'Label volume');
  end
  labelValues = unique(values(:));
  labelValues = labelValues(labelValues > 0);
  components = cell(1, numel(labelValues));
  for k = 1:numel(labelValues)
    components{k} = find(values == labelValues(k));
  end
  imageSize = [size(labels, 1), size(labels, 2), size(labels, 3)];
end
%=============================================================================
function ip_regionprops3_validate_volume(value, allowNumeric)
  if ~(islogical(value) || (allowNumeric && isnumeric(value)))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Volume');
  end
  if issparse(value)
    error('Nelson:image_processing:sparseVolumesNotSupported', '%s are not supported.', 'Sparse volumes');
  end
  if ~isreal(value)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Volume');
  end
  if length(size(value)) ~= 3
    error('Nelson:image_processing:volumeDArray', '%s must be a 3-D array.', 'Volume');
  end
end
%=============================================================================
function ip_regionprops3_validate_intensity(intensity, imageSize)
  ip_regionprops3_validate_volume(intensity, true);
  if ~isequal([size(intensity, 1), size(intensity, 2), size(intensity, 3)], imageSize)
    error('Nelson:image_processing:intensityVolumeSameSizeRegionVolume', '%s must have the same size as %s.', 'Intensity volume', 'the region volume');
  end
end
%=============================================================================
