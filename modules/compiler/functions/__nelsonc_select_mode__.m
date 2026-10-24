%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function gui = __nelsonc_select_mode__(plan, requested)
  if islogical(requested) && isscalar(requested)
    gui = requested;
  elseif ischar(requested) && any(strcmp(requested, {'auto', 'cli', 'gui'}))
    gui = strcmp(requested, 'gui') || (strcmp(requested, 'auto') && plan.requiresGraphics);
  else
    error('Nelson:nelsonc:mode', 'Application mode must be auto, cli or gui.');
  end
  if ~gui && plan.requiresGraphics
    error('Nelson:nelsonc:mode:graphicalDependenciesCannotPackagedCLI', 'Graphical dependencies cannot be packaged in CLI mode.');
  end
end
