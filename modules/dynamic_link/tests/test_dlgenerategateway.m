%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dlgenerategateway(tempdir(), 'test_gw', {{'cpp_sum', 1, 2, 'CPP_BUILTIN'}, {'cpp_sub', 3, 4}, {'cpp_sub2', 3, 4, 'CPP_BUILTIN_WITH_EVALUATOR'}});
assert_istrue(isfile([tempdir(), 'Gateway.cpp']));
txt = fileread([tempdir(), 'Gateway.cpp']);
assert_istrue(contains(txt, '{ "cpp_sum", (void*)cpp_sumBuiltin, 1, 2, CPP_BUILTIN },'));
assert_istrue(contains(txt, '{ "cpp_sub", (void*)cpp_subBuiltin, 3, 4, CPP_BUILTIN },'));
assert_istrue(contains(txt, '{ "cpp_sub2", (void*)cpp_sub2Builtin, 3, 4, CPP_BUILTIN_WITH_EVALUATOR },'));
%=============================================================================