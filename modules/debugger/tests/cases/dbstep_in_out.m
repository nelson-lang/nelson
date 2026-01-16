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
function n = dbstep_in_out(x)
n = myfunction(x-1);
end
%=============================================================================
function z = myfunction(y)
z = 2/y;
end
%=============================================================================
% addpath([modulepath('debugger', 'tests'), '/cases/']);
% edit dbstep_in_out
% dbstop in dbstep_in_out
% dbstep_in_out(2);
% dbstep in
% dbstep out