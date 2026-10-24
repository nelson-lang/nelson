%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a table shows logical values as words, even if a logical array is displayed
% with 1 and 0
%=============================================================================
T = table([true; false], [true false; false true]);
R = string(splitlines(evalc('disp(T)')));
asserts.isequal(R(4), "    true     true   false");
asserts.isequal(R(5), "    false    false  true ");
%=============================================================================
