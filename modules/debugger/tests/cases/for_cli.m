%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%<--INTERACTIVE TEST-->
%=============================================================================
for k = 1:2
    disp(['Iteration k = ', num2str(k)]);
    disp('next line');
end
disp('end of file')
%=============================================================================
% addpath([modulepath('debugger', 'tests'), '/cases/']);
% dbstop in for_cli at 12
% dbstop in for_cli at 13
% dbstop in for_cli at 14
% dbstop in for_cli at 15
% dbstatus()
% for_cli();
