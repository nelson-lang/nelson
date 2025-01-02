%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--JULIA ENVIRONMENT REQUIRED-->
%=============================================================================
R = jlrun('using SparseArrays;n, m = 4, 4 ; I = [1, 2, 3]; J = [1, 3, 4] ;V = [1.0, 2.0, 3.0]; S = sparse(I, J, V, n, m)', 'S');
REF = {'numeric'; 'double'; 'single'};
assert_isequal(methods(R), REF);
%=============================================================================
julia_code = ["mutable struct Person";
"    name::String";
"    age::Int64";
"    getAge::Function";
"";
"    function Person(name::String, age::Int64)";
"        self = new(name, age)";
"        self.getAge = () -> self.age";
"        return self";
"    end";
"end";
"";
"p = Person(""Alice"", 42)"];
R = jlrun(julia_code, "p");
REF = {'getAge'};
assert_isequal(methods(R), REF);
%=============================================================================
