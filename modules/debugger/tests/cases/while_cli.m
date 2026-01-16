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
TEST_CONDITION = true;
while TEST_CONDITION
    disp('Inside while loop');
    disp('next line');
    TEST_CONDITION = true;
end
disp('After while loop');
%=============================================================================
% addpath([modulepath('debugger', 'tests'), '/cases/']);
% dbclear all
% dbstop in while_cli at 12
% while_cli
% do a breakpoint within the while loop with text editor
%=============================================================================