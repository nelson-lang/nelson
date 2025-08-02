%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ( (strcmp(getnelsonmode(), 'GUI') == 1) || (strcmp(getnelsonmode(), 'ADVANCED_TERMINAL') == 1) )
  if ~any(strcmp(argv(),'--nouserstartup'))
    history('save');
  end
  removegateway(modulepath('history_manager', 'builtin'));
end
