%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [g, gN, gL] = grp2idx(s)
  narginchk(1, 1);
  nargoutchk(0, 3);
  if isnumeric(s)
    [g, gN, gL] = numericGroupsLocal(s);
  elseif islogical(s)
    [g, gN, gL] = logicalGroupsLocal(s);
  elseif iscategorical(s)
    [g, gN, gL] = categoricalGroupsLocal(s);
  elseif isstring(s)
    [g, gN, gL] = textGroupsLocal(cellstr(s), size(s), true);
  elseif ischar(s)
    labels = cellstr(s);
    [g, gN, gL] = textGroupsLocal(labels, [size(s, 1), 1], false);
    gL = char(gL);
  elseif iscellstr(s)
    [g, gN, gL] = textGroupsLocal(s, size(s), false);
  else
    error('Nelson:statistics:sGroupingVariable', '%s must be a grouping variable.', 'S');
  end
end
%=============================================================================
function [g, gN, gL] = numericGroupsLocal(s)
  if ~isvector(s)
    error('Nelson:statistics:sVectorTextArray', '%s must be a vector or text array.', 'S');
  end
  values = double(s(:));
  missing = isnan(values);
  [levels, ~, ic] = unique(values(~missing));
  g = NaN(size(values));
  g(~missing) = ic;
  g = reshape(g, size(s));
  gL = cast(levels(:), class(s));
  gN = cell(length(levels), 1);
  for k = 1:length(levels)
    gN{k} = mat2str(gL(k));
  end
end
%=============================================================================
function [g, gN, gL] = logicalGroupsLocal(s)
  if ~isvector(s)
    error('Nelson:statistics:sVectorTextArray', '%s must be a vector or text array.', 'S');
  end
  levels = [false; true];
  counts = [any(s(:) == false); any(s(:) == true)];
  levels = levels(counts);
  g = NaN(size(s));
  for k = 1:length(levels)
    g(s == levels(k)) = k;
  end
  gL = levels(:);
  gN = cell(length(levels), 1);
  for k = 1:length(levels)
    gN{k} = mat2str(gL(k));
  end
end
%=============================================================================
function [g, gN, gL] = categoricalGroupsLocal(s)
  labels = cellstr(s);
  groupNames = categories(s);
  g = labelIndicesLocal(labels, groupNames);
  g = reshape(g, size(s));
  gN = groupNames(:);
  gL = categorical(groupNames, groupNames, groupNames);
end
%=============================================================================
function [g, gN, gL] = textGroupsLocal(labels, outSize, asString)
  if ~isvector(labels)
    error('Nelson:statistics:sVectorTextArray', '%s must be a vector or text array.', 'S');
  end
  labels = labels(:);
  missing = strcmp(labels, '');
  [groupNames, ~, ic] = unique(labels(~missing), 'stable');
  g = NaN(length(labels), 1);
  g(~missing) = ic;
  g = reshape(g, outSize);
  gN = groupNames(:);
  if asString
    gL = string(groupNames(:));
  else
    gL = groupNames(:);
  end
end
%=============================================================================
function g = labelIndicesLocal(labels, groupNames)
  labels = labels(:);
  g = NaN(length(labels), 1);
  for k = 1:length(groupNames)
    g(strcmp(labels, groupNames{k})) = k;
  end
end
%=============================================================================
