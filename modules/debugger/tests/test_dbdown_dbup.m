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
addpath([modulepath('debugger', 'tests'), '/cases/']);
dbstop('in','dbstep_in_out>myfunction')
dbstep_in_out(1)
whos
%   Name      Size            Bytes  Class   Attributes
%   y         1×1                8  double
dbup
% In workspace belonging to dbstep_in_out
whos
%   Name      Size            Bytes  Class   Attributes
%   x         1×1                8  double
dbdown
% In workspace belonging to myfunction
whos
%   Name      Size            Bytes  Class   Attributes
%   y         1×1                8  double
