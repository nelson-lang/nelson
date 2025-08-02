%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
%=============================================================================
% some tests to check completion behavior
surf(p<TAB>
% on windows
cd c:/<TAB>
cd('c:/<TAB>
p<TAB>
%=============================================================================
a = dir
a.<TAB>
a.n<TAB>
%=============================================================================
% creates two files foo1.m and foo2.m
%=============================================================================
foo<TAB>   foo1 and foo2 are proposed and not foo1.m and foo2.m
d:/foo<TAB> foo1.m and foo2.m are proposed and not foo1 and foo2

