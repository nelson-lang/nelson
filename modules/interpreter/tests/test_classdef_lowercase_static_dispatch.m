%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A static method call on a class whose name starts with a lowercase letter
% must dispatch to the static method, not be mistaken for a constructor call.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% Called from a compiled function body before the class is loaded.
assert_isequal(call_lowercasestatichost(4), 40);
%=============================================================================
% Called again once the class is known.
assert_isequal(call_lowercasestatichost(5), 50);
%=============================================================================
