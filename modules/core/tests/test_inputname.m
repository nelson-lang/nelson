%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/core/tests/']);
%=============================================================================
asserts.isequal(nargin('inputname'), 1);
asserts.isequal(nargout('inputname'), 1);
%=============================================================================
A = 1;
B = 2;
C = 3;
R = inputname_1(A, B, C);
REF = ["A", "B", "C"];
asserts.isequal(R, REF);
%=============================================================================
R = inputname_1(A, 33, C);
REF = ["A", "", "C"];
asserts.isequal(R, REF);
%=============================================================================
A = ones(3, 3);
C = {'a', 'b'; 'c', 'd'};
R = inputname_1(A(1, 1), {'a', 'b'; 'c', 'd'}, C{2, 2});
REF = ["", "", ""];
asserts.isequal(R, REF);
%=============================================================================
addpath([modulepath('functions_manager'), '/tests']);
firstValue = 1;
secondValue = 2;
inputNames = runtime_inputnames_fixture(firstValue, 99, secondValue);
asserts.isequal(inputNames, ["firstValue", "", "secondValue"]);
%=============================================================================
x = [7; 8];
y = 1;
eval('y;');
R = inputname_1(y, x);
asserts.isequal(R, ["y", "x"]);
c = {x};
R = inputname_1(y, c{1});
asserts.isequal(R, ["y", ""]);
s = struct('a', {x});
R = inputname_1(y, s.a);
asserts.isequal(R, ["y", ""]);
%=============================================================================
R = inputname_2(x, y);
asserts.isequal(R, ["", ""]);
%=============================================================================
% a builtin storing or returning a named argument does not pass the name on
s = struct('a', x);
R = inputname_1(y, s.a);
asserts.isequal(R, ["y", ""]);
R = inputname_1(y, getfield(s, 'a'));
asserts.isequal(R, ["y", ""]);
R = inputname_1(y, double(x));
asserts.isequal(R, ["y", ""]);
c = struct2cell(s);
R = inputname_1(y, c{:});
asserts.isequal(R, ["y", ""]);
R = feval('inputname_1', y, x);
asserts.isequal(R, ["y", "x"]);
R = evalc('getfield(s, ''a'')');
asserts.istrue(contains(R, 'ans ='));
asserts.isfalse(contains(R, 'x ='));
%=============================================================================
