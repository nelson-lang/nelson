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
R = jlrun('using SparseArrays;n, m = 4, 4 ; I = [1, 2, 3]; J = [1, 3, 4] ;V = [1.0, 2.0, 3.0]; S = sparse(I, J, V, n, m)', 'S');
REF = {'typeof'; 'm'; 'n';  'colptr'; 'rowval'; 'nzval'};
assert_isequal(properties(R), REF);
%=============================================================================
R = jlrun('A = [Complex{Real}(1, 2.1) Complex{Real}(1, 2.1);Complex{Real}(1, 2.1) Complex{Real}(1, 2.1)]', 'A');
REF = {'typeof'; 'ref'; 'size'};
assert_isequal(properties(R), REF);
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
REF = {'typeof'; 'name'; 'age'};
assert_isequal(properties(R), REF);
%=============================================================================
