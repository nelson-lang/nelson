%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--JULIA ENVIRONMENT REQUIRED-->
%=============================================================================
R = jlrun('A = Base','A');
assert_isequal(R.ceil(3.145), 4);
%=============================================================================
julia_code = ["mutable struct Person";
"    name::String";
"    greetings::Function";
"end";
"p = Person(""Alice"", () -> println(""Hello, my name is Alice""))"];
R = jlrun(julia_code, "p");
assert_istrue(contains(evalc('R.greetings();'), "Hello, my name is Alice"));
%=============================================================================
