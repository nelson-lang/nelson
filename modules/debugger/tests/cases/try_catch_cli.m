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
try
    disp('In try block');
    a = 10;
    b = 0;
    c = a / b; % This will cause a division by zero error
    disp(['Result: ', num2str(c)]);
catch ME
    disp('An error occurred:');
    disp(ME.message);
end
disp('End of try-catch test');
%=============================================================================
% addpath([modulepath('debugger', 'tests'), '/cases/']);
% dbstop in try_catch_cli at 12