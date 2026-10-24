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
juliaRequireEngineForTest();
%=============================================================================
% Regression: cell() cast on a Julia Vector{String} used to fail with
% "Wrong type for argument #1: double expected." because the cast-method
% routing did not advertise 'cell' for textual element vectors.
cols = jlrun('c = String.(["x","y"])', 'c');
asserts.isequal(cols.typeof(), 'Vector{String}');
%=============================================================================
% builtin form: cell(handle)
R = cell(cols);
asserts.isequal(R, {'x'; 'y'});
%=============================================================================
% method form: handle.cell()
R = cols.cell();
asserts.isequal(R, {'x'; 'y'});
%=============================================================================
% 'cell' is now listed as an available method alongside 'string'.
asserts.istrue(any(strcmp(methods(cols), 'cell')));
asserts.istrue(any(strcmp(methods(cols), 'string')));
%=============================================================================
% string() cast is still available and unchanged.
asserts.isequal(cols.string(), ["x"; "y"]);
%=============================================================================
% Matrix{String} also supports the cell() cast.
M = jlrun('m = ["a" "b"; "c" "d"]', 'm');
asserts.isequal(M.typeof(), 'Matrix{String}');
asserts.isequal(cell(M), {'a', 'b'; 'c', 'd'});
%=============================================================================
