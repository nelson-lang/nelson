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
if true
    disp('Condition is true');
    disp('next line');
    if false
        disp('Nested condition is false');
        disp('next line');
    end
else
    disp('Condition is false');
    disp('next line');
    if true
        disp('Nested condition is true');
        disp('next line');
    end
end
disp('after if else end')

end
%=============================================================================
% addpath([modulepath('debugger', 'tests'), '/cases/']);
% for_if_cli();
