%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __check_class_names__(observed, requested, requireSameLength)
  if isequal(observed, requested)
    return
  end
  if requireSameLength && length(requested) ~= length(observed)
    error('Nelson:statistics:classNamesIncludeAllObservedClasses', 'ClassNames must include all observed classes.');
  end
  loc = __class_locations__(observed, requested);
  if any(loc == 0)
    error('Nelson:statistics:classNamesIncludeAllObservedClasses', 'ClassNames must include all observed classes.');
  end
end
%=============================================================================
