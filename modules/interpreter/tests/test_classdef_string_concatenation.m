%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Next to a string array, the object class decides the concatenation (value
% or handle class, even an empty object): the class constructor converts each
% string operand, even an empty one, and a failure of the constructor names
% the conversion. A concatenation method of the class still wins.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
idU = 'Nelson:UnableToConvert';
fmtU = _('The following error occurred converting from %s to %s:\n%s');
%=============================================================================
% value class
V = 'ClassdefStringCatValue';
o = ClassdefStringCatValue(1);
r = [o, "s"];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 2]);
assert_isequal(r(2).v, "s");
r = ["s"; o];
assert_isequal(class(r), V);
assert_isequal(size(r), [2 1]);
assert_isequal(r(1).v, "s");
r = cat(3, o, "s");
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1 2]);
r = [o, "s", o];
assert_isequal(size(r), [1 3]);
r = [o, ["a", "b"]];
assert_isequal(size(r), [1 2]);
assert_isequal(r(2).v, ["a", "b"]);
%=============================================================================
% an empty string goes through the constructor too
r = [o, string.empty];
assert_isequal(size(r), [1 2]);
assert_isequal(class(r(2).v), 'string');
assert_isequal(size(r(2).v), [0 0]);
r = [string.empty, o];
assert_isequal(size(r), [1 2]);
r = [o, strings(1, 0)];
assert_isequal(size(r), [1 2]);
assert_isequal(size(r(2).v), [1 0]);
r = [o; strings(0, 3)];
assert_isequal(size(r), [2 1]);
assert_isequal(size(r(2).v), [0 3]);
r = cat(3, o, string.empty);
assert_isequal(size(r), [1 1 2]);
assert_isequal(size(r(2).v), [0 0]);
%=============================================================================
% an empty object still decides the class
e = ClassdefStringCatValue.empty();
r = [e, "s"];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
assert_isequal(r.v, "s");
r = ["s"; e];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
r = [e, string.empty];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
r = cat(3, e, "s");
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
%=============================================================================
% handle class
H = 'ClassdefStringCatHandle';
h = ClassdefStringCatHandle(1);
r = [h, "s"];
assert_isequal(class(r), H);
assert_isequal(size(r), [1 2]);
assert_isequal(r(2).v, "s");
r = ["s"; h];
assert_isequal(class(r), H);
assert_isequal(size(r), [2 1]);
assert_isequal(r(1).v, "s");
r = cat(1, h, "s");
assert_isequal(size(r), [2 1]);
r = cat(3, "s", h);
assert_isequal(class(r), H);
assert_isequal(size(r), [1 1 2]);
r = [h, string.empty];
assert_isequal(size(r), [1 2]);
assert_isequal(size(r(2).v), [0 0]);
r = cat(3, h, string.empty);
assert_isequal(size(r), [1 1 2]);
r = [h, "s", 2];
assert_isequal(size(r), [1 3]);
assert_isequal(r(3).v, 2);
r = [h, []];
assert_isequal(class(r), H);
assert_isequal(size(r), [1 1]);
he = ClassdefStringCatHandle.empty();
r = [he, "s"];
assert_isequal(class(r), H);
assert_isequal(size(r), [1 1]);
%=============================================================================
% the constructor failure names the conversion
msg = sprintf(fmtU, 'string', 'ClassdefStringCatRefuse', 'refused string');
assert_checkerror('[ClassdefStringCatRefuse(), "s"]', msg, idU);
assert_checkerror('["s"; ClassdefStringCatRefuse()]', msg, idU);
assert_checkerror('cat(3, ClassdefStringCatRefuse(), "s")', msg, idU);
assert_checkerror('[ClassdefStringCatRefuse(), string.empty]', msg, idU);
assert_checkerror('[ClassdefStringCatRefuse.empty(), "s"]', msg, idU);
msg = sprintf(fmtU, 'double', 'ClassdefStringCatRefuse', 'refused double');
assert_checkerror('[1, "s", ClassdefStringCatRefuse()]', msg, idU);
%=============================================================================
try
  ClassdefStringCatNoArg("s");
catch ex
  inner = ex.message;
end
msg = sprintf(fmtU, 'string', 'ClassdefStringCatNoArg', inner);
assert_checkerror('[ClassdefStringCatNoArg(), "s"]', msg, idU);
assert_checkerror('cat(3, "s", ClassdefStringCatNoArg())', msg, idU);
msg = sprintf(fmtU, 'string', 'ClassdefStringCatHandleNoArg', inner);
assert_checkerror('[ClassdefStringCatHandleNoArg(), "s"]', msg, idU);
assert_checkerror('["s"; ClassdefStringCatHandleNoArg()]', msg, idU);
assert_checkerror('[ClassdefStringCatHandleNoArg.empty(), string.empty]', msg, idU);
%=============================================================================
% a concatenation method of the class receives the string
m = ClassdefStringCatHandleMethod();
assert_isequal([m, "s"], 'horzcat:ClassdefStringCatHandleMethod:string');
assert_isequal(["s", m], 'horzcat:string:ClassdefStringCatHandleMethod');
%=============================================================================
