%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isfield'), 2);
asserts.isequal(nargout('isfield'), 1);
%=============================================================================
S.a = 'r';
S.b = 1;
asserts.istrue(isfield(S, 'a'));
asserts.istrue(isfield(S, 'b'));
asserts.isfalse(isfield(S, 'c'));
%=============================================================================
S.a = 'r';
S.b = 1;
S.One = 1;
S.Two = 2;
S.Three = pi;
R = isfield(S, {'a', 'One', 'Three', 3.14, 'two', 'Two'});
REF = [true true true false false true];
asserts.isequal(R, REF);
%=============================================================================
R = isfield(S, {'a', 'One', 'Three'; 3.14, 'two', 'Two'});
REF = logical([1, 1, 1; 0, 0, 1]);
asserts.isequal(R, REF);
%=============================================================================
R = isfield(S, {'a', 'One'; 'Three', 3.14; 'two', 'Two'});
REF = logical([1, 1; 1, 0; 0, 1]);
asserts.isequal(R, REF);
%=============================================================================
R = isfield(S, {});
asserts.isfalse(R);
%=============================================================================
S = struct();
R = isfield(S, '');
asserts.isfalse(R);
%=============================================================================
S = struct();
R = isfield(S, {});
asserts.isfalse(R);
%=============================================================================
R = isfield(1, {});
asserts.isfalse(R);
%=============================================================================
R = isfield(1, 1);
asserts.isfalse(R);
%=============================================================================
S = struct('b', 2, 'a', 1);
asserts.istrue(isfield(S, 'a'));
asserts.isfalse(isfield(S, 'missing'));
%=============================================================================
% classdef object without isfield overload: no error, returns false
obj = IsfieldFixtureCls();
R = isfield(obj, 'x');
asserts.isequal(class(R), 'logical');
asserts.isequal(size(R), [1 1]);
asserts.isfalse(R);
R = isfield(obj, {'x', 'y'});
asserts.isequal(size(R), [1 1]);
asserts.isfalse(R);
%=============================================================================
% isfield overload defined by the classdef is dispatched
ov = IsfieldFixtureOverloadCls();
asserts.istrue(isfield(ov, 'custom'));
asserts.isfalse(isfield(ov, 'x'));
%=============================================================================
% old-style class object without isfield overload: no error, returns false
addpath([nelsonroot(), '/modules/overload/examples/complex']);
c = complexObj(3, 4);
asserts.isequal(class(c), 'complexObj');
R = isfield(c, 'r');
asserts.isequal(size(R), [1 1]);
asserts.isfalse(R);
%=============================================================================
% handle object without isfield overload: no error, returns false
if ismodule('dynamic_link')
  lib = dlopen(modulepath('dynamic_link', 'builtin'));
  R = isfield(lib, 'x');
  asserts.isequal(class(R), 'logical');
  asserts.isfalse(R);
  dlclose(lib);
end
%=============================================================================
% string scalar and string array field names
s = struct('a', 1, 'b', 2);
asserts.istrue(isfield(s, "a"));
asserts.isfalse(isfield(s, "c"));
asserts.isequal(isfield(s, ["a", "c"]), [true, false]);
asserts.isequal(isfield(s, ["a"; "b"]), [true; true]);
asserts.isequal(isfield(s, {'a'; 'c'}), [true; false]);
asserts.isequal(isfield(s, strings(0, 0)), logical(zeros(0, 0)));
asserts.isfalse(isfield(s, {}));
asserts.isfalse(isfield(s, string(missing)));
%=============================================================================
