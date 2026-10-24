%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = ip_regionprops3_needs_intensity(properties)
  needs = {'MeanIntensity', 'MinIntensity', 'MaxIntensity', 'VoxelValues', 'WeightedCentroid'};
  tf = false;
  for k = 1:numel(properties)
    tf = tf || any(strcmp(needs, properties{k}));
  end
end
%=============================================================================
