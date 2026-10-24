%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
setenv('NFLOW_GUI_DEBUGGING_PORT', '1973');
nflow();
cd([modulepath('nflow_gui', 'root')]);
[a, b] = system('npm run test:e2e')
