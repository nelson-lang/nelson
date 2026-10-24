%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function properties = ip_regionprops3_properties(args)
  basic = {'Volume', 'Centroid', 'BoundingBox'};
  allProps = {'Volume', 'Centroid', 'BoundingBox', 'VoxelIdxList', 'VoxelList', 'Image', ...
    'SubarrayIdx', 'Extent', 'EquivDiameter', 'MeanIntensity', 'MinIntensity', ...
    'MaxIntensity', 'VoxelValues', 'WeightedCentroid'};
  if isempty(args)
    properties = basic;
    return
  end
  properties = {};
  for k = 1:numel(args)
    item = args{k};
    if iscell(item)
      for j = 1:numel(item)
        properties = ip_regionprops3_append(properties, item{j}, basic, allProps);
      end
    else
      properties = ip_regionprops3_append(properties, item, basic, allProps);
    end
  end
end
%=============================================================================
function properties = ip_regionprops3_append(properties, value, basic, allProps)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:image_processing:unsupportedRegionProperty', 'Unsupported "%s" option.', 'region property');
  end
  name = lower(convertStringsToChars(value));
  if strcmp(name, 'basic')
    properties = ip_regionprops3_append_many(properties, basic);
  elseif strcmp(name, 'all')
    properties = ip_regionprops3_append_many(properties, allProps);
  else
    properties{end + 1} = ip_regionprops3_canonical(name, allProps);
  end
end
%=============================================================================
function properties = ip_regionprops3_append_many(properties, names)
  for k = 1:numel(names)
    if ~any(strcmp(properties, names{k}))
      properties{end + 1} = names{k};
    end
  end
end
%=============================================================================
function prop = ip_regionprops3_canonical(name, allProps)
  aliases = {'VoxelIndices', 'VoxelIdxList'; 'VoxelValues', 'VoxelValues'};
  for k = 1:size(aliases, 1)
    if strcmp(name, lower(aliases{k, 1}))
      prop = aliases{k, 2};
      return
    end
  end
  for k = 1:numel(allProps)
    if strcmp(name, lower(allProps{k}))
      prop = allProps{k};
      return
    end
  end
  error('Nelson:image_processing:unsupportedRegionProperty', 'Unsupported "%s" option.', 'region property');
end
%=============================================================================
