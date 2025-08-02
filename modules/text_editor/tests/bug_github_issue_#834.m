%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/834
% <-- Short Description -->
% VariableCompleter was not filtered by prefix.
%=============================================================================
% <--INTERACTIVE TEST-->
%=============================================================================
A = 3;
AA = 33;
AAV = 31;
B = 2;
BB = 32;
edit
% type A<TAB>, you should see only A && AAV as proposed text
