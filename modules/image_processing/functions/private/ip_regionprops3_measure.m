%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stats = ip_regionprops3_measure(components, imageSize, intensity, properties)
  template = struct();
  for p = 1:numel(properties)
    template.(properties{p}) = [];
  end
  stats = repmat(template, numel(components), 1);
  intensityValues = [];
  if ~isempty(intensity)
    intensityValues = double(intensity);
  end
  for k = 1:numel(components)
    idx = components{k}(:);
    [r, c, z] = ind2sub(imageSize, idx);
    geom = ip_regionprops3_geometry(r, c, z);
    for p = 1:numel(properties)
      prop = properties{p};
      stats(k).(prop) = ip_regionprops3_value(prop, idx, r, c, z, geom, ...
        imageSize, intensityValues);
    end
  end
end
%=============================================================================
function geom = ip_regionprops3_geometry(r, c, z)
  minR = min(r);
  maxR = max(r);
  minC = min(c);
  maxC = max(c);
  minZ = min(z);
  maxZ = max(z);
  geom = struct('MinR', minR, 'MaxR', maxR, 'MinC', minC, 'MaxC', maxC, ...
    'MinZ', minZ, 'MaxZ', maxZ, 'Height', maxR - minR + 1, ...
    'Width', maxC - minC + 1, 'Depth', maxZ - minZ + 1);
end
%=============================================================================
function value = ip_regionprops3_value(prop, idx, r, c, z, geom, imageSize, intensityValues)
  switch prop
    case 'Volume'
      value = numel(idx);
    case 'Centroid'
      value = [mean(c), mean(r), mean(z)];
    case 'BoundingBox'
      value = [geom.MinC - 0.5, geom.MinR - 0.5, geom.MinZ - 0.5, ...
        geom.Width, geom.Height, geom.Depth];
    case 'VoxelIdxList'
      value = idx(:);
    case 'VoxelList'
      value = [c(:), r(:), z(:)];
    case 'Image'
      value = ip_regionprops3_local_image(r, c, z, geom);
    case 'SubarrayIdx'
      value = {geom.MinR:geom.MaxR, geom.MinC:geom.MaxC, geom.MinZ:geom.MaxZ};
    case 'Extent'
      value = numel(idx) / (geom.Width * geom.Height * geom.Depth);
    case 'EquivDiameter'
      value = 2 * ((3 * numel(idx)) / (4 * pi)) ^ (1 / 3);
    case 'MeanIntensity'
      value = mean(intensityValues(idx));
    case 'MinIntensity'
      value = min(intensityValues(idx));
    case 'MaxIntensity'
      value = max(intensityValues(idx));
    case 'VoxelValues'
      value = intensityValues(idx);
      value = value(:);
    case 'WeightedCentroid'
      value = ip_regionprops3_weighted_centroid(r, c, z, intensityValues(idx));
    otherwise
      error('Nelson:image_processing:unsupportedRegionProperty', 'Unsupported "%s" option.', 'region property');
  end
end
%=============================================================================
function image = ip_regionprops3_local_image(r, c, z, geom)
  image = false(geom.Height, geom.Width, geom.Depth);
  local = sub2ind(size(image), r - geom.MinR + 1, c - geom.MinC + 1, z - geom.MinZ + 1);
  image(local) = true;
end
%=============================================================================
function centroid = ip_regionprops3_weighted_centroid(r, c, z, values)
  total = sum(values);
  if total == 0
    centroid = [NaN NaN NaN];
  else
    centroid = [sum(c .* values) / total, sum(r .* values) / total, sum(z .* values) / total];
  end
end
%=============================================================================
