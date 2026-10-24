%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([modulepath('function_handle', 'root'), '/examples/parameterized_function.m']);
assert(modelValues(1) == 3);
assert(all(diff(modelValues) < 0));
assert(abs(modelValues(end) - (2 + exp(-2.25))) < eps);
%=============================================================================
