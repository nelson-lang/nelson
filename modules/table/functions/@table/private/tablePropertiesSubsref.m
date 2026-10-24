%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = tablePropertiesSubsref(obj, properties, sref)
  if isempty(sref)
    value = properties;
    return
  end
  try
    value = subsref(properties, sref);
  catch ex
    if length(sref) ~= 2 || ~strcmp(sref(1).type, '.') ...
        || ~(strcmp(sref(2).type, '()') || strcmp(sref(2).type, '{}')) ...
        || length(sref(2).subs) ~= 1
      rethrow(ex);
    end
    propertyName = sref(1).subs;
    if ~ischar(propertyName) || ~isfield(properties, propertyName)
      rethrow(ex);
    end
    index = sref(2).subs{1};
    if isempty(index) || issparse(index) || ~isreal(index) ...
        || ~((isnumeric(index) && all(isfinite(index(:))) ...
        && all(index(:) >= 1) && all(index(:) == fix(index(:)))) || islogical(index))
      rethrow(ex);
    end
    if islogical(index)
      exceedsBound = any(find(index(:)) > propertyDimensionBound(obj, propertyName));
    else
      exceedsBound = any(index(:) > propertyDimensionBound(obj, propertyName));
    end
    if ~exceedsBound
      rethrow(ex);
    end
    if startsWith(propertyName, 'Variable')
      error('Nelson:table:varIndexOutOfRange', _('Variable index exceeds table dimensions.'));
    elseif strcmp(propertyName, 'RowNames') || strcmp(propertyName, 'RowTimes')
      error('Nelson:table:rowIndexOutOfRange', _('Row index exceeds table dimensions.'));
    elseif strcmp(propertyName, 'DimensionNames')
      error('Nelson:table:dimIndexOutOfRange', _('Dimension index exceeds number of table dimensions.'));
    end
    rethrow(ex);
  end
end
%=============================================================================
function bound = propertyDimensionBound(obj, propertyName)
  if startsWith(propertyName, 'Variable')
    bound = size(obj, 2);
  elseif strcmp(propertyName, 'RowNames') || strcmp(propertyName, 'RowTimes')
    bound = size(obj, 1);
  elseif strcmp(propertyName, 'DimensionNames')
    bound = 2;
  else
    bound = Inf;
  end
end
%=============================================================================
