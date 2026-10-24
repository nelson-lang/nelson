%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [yIndex, classNames] = __reorder_class_names__(yIndex, classNames, requested)
  if isequal(classNames, requested)
    return
  end
  loc = __class_locations__(classNames, requested);
  if any(loc == 0)
    error('Nelson:statistics:classNamesIncludeAllObservedClasses', 'ClassNames must include all observed classes.');
  end
  remapped = yIndex;
  for k = 1:length(loc)
    remapped(yIndex == k) = loc(k);
  end
  yIndex = remapped;
  classNames = requested;
end
%=============================================================================
