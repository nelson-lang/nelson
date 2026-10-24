%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = __class_names__(value, observed)
  if ~isvector(value)
    error('Nelson:statistics:classNamesVectorClassLabels', 'ClassNames must be a vector of class labels.');
  end
  if isnumeric(observed)
    if ~(isnumeric(value) && isreal(value))
      error('Nelson:statistics:classNamesMatchClassLabelType', 'ClassNames must match the class label type.');
    end
    value = value(:);
  elseif islogical(observed)
    if ~islogical(value)
      error('Nelson:statistics:classNamesMatchClassLabelType', 'ClassNames must match the class label type.');
    end
    value = value(:);
  elseif isstring(observed)
    value = string(value(:));
  elseif iscategorical(observed)
    value = categorical(value);
    value = value(:);
  elseif iscellstr(observed)
    if ~(iscellstr(value) || isstring(value))
      error('Nelson:statistics:classNamesMatchClassLabelType', 'ClassNames must match the class label type.');
    end
    value = cellstr(value(:));
  else
    error('Nelson:statistics:classNamesMatchClassLabelType', 'ClassNames must match the class label type.');
  end
  if length(unique(value)) ~= length(value)
    error('Nelson:statistics:classNamesContainUniqueLabels', 'ClassNames must contain unique labels.');
  end
end
%=============================================================================
