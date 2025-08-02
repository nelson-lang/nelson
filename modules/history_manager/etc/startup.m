%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ( (strcmp(getnelsonmode(), 'GUI') == true) || (strcmp(getnelsonmode(), 'ADVANCED_TERMINAL') == true) )
  addgateway(modulepath('history_manager', 'builtin'), 'history_manager');
  if ~any(strcmp(argv(),'--nouserstartup'))
    history('load');
  end
end
%=============================================================================