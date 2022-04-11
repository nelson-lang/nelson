%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WINDOWS ONLY-->
% <--C/C++ COMPILER REQUIRED-->
%=============================================================================
assert_istrue(havecompiler());
r = removecompilerconf();
assert_isfalse(havecompiler());