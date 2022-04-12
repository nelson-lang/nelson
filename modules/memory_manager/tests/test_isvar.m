%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear A
assert_isfalse(isvar('A'));
A = 3;
assert_istrue(isvar('A'));
assert_istrue(isvar('local', 'A'));
assert_isfalse(isvar('global', 'B'));
global B
assert_istrue(isvar('global', 'B'));
%=============================================================================