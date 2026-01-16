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
function  function_cli()
    disp("In function_cli");
    a = 10;
    b = 20;
    c = a + b;
    disp("Sum: " + string(c));
   fun1(1)
   disp('last in function_cli')
end
%=============================================================================
function fun1(x)
    disp("In fun1 with x = " + string(x));
    y = x * 2;
    disp("y = " + string(y));
    fun2(x, y);
   disp('last in fun1')
end
%=============================================================================
function fun2(x, y)
    disp("In fun2 with x = " + string(x) + " and y = " + string(y));
    z = x + y;
    disp("z = " + string(z));
    for k = 1:3
        if k == 2
            disp("k is equal to 2");
        else
            disp("k is not equal to 2");
        end
        disp("Loop k = " + string(k));
    end
  disp('last in fun2')
end
%=============================================================================
% addpath([modulepath('debugger', 'tests'), '/cases/']);
% edit function_cli
% dbstop in function_cli
% function_cli();