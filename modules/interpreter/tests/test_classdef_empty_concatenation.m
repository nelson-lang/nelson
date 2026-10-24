%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The first object class decides a concatenation, even from an empty object:
% the class constructor converts every other nonempty operand. Next to a
% nonempty operand, horzcat and vertcat drop every 2-D empty operand whatever
% its size, an N-D one cannot be concatenated. When every operand is empty, the
% result is an empty object of the size of the empty concatenation.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
idU = 'Nelson:UnableToConvert';
fmtU = _('The following error occurred converting from %s to %s:\n%s');
V = 'ClassdefStringCatValue';
H = 'ClassdefStringCatHandle';
o = ClassdefStringCatValue(1);
e = ClassdefStringCatValue.empty();
%=============================================================================
% an empty object decides the class
r = [e, 5];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
assert_isequal(r.v, 5);
r = [5, e];
assert_isequal(class(r), V);
r = [e; 5];
assert_isequal(class(r), V);
r = [e, [1 2]];
assert_isequal(size(r), [1 1]);
assert_isequal(r.v, [1 2]);
r = [e, 1, 2];
assert_isequal(size(r), [1 2]);
r = [e, 'c'];
assert_isequal(r.v, 'c');
r = [e, {1}];
assert_isequal(r.v, {1});
r = cat(1, e, 5);
assert_isequal(class(r), V);
r = cat(3, e, 5);
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
r = [ClassdefStringCatValue.empty(0, 3); [1 2 3]];
assert_isequal(size(r), [1 1]);
assert_isequal(r.v, [1 2 3]);
%=============================================================================
% every operand empty: an empty object of the empty concatenation size
r = [e, []];
assert_isequal(class(r), V);
assert_isequal(size(r), [0 0]);
r = [[], e];
assert_isequal(class(r), V);
assert_isequal(size(r), [0 0]);
r = [e, zeros(1, 0)];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 0]);
r = [zeros(1, 0), e];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 0]);
r = [e, zeros(0, 3)];
assert_isequal(class(r), V);
assert_isequal(size(r), [0 3]);
r = [e, {}];
assert_isequal(class(r), V);
assert_isequal(size(r), [0 0]);
r = [e, ClassdefStringCatNoArg.empty()];
assert_isequal(class(r), V);
r = [ClassdefStringCatNoArg.empty(), e];
assert_isequal(class(r), 'ClassdefStringCatNoArg');
%=============================================================================
% next to a nonempty object, every 2-D empty operand is dropped
r = [o, zeros(1, 0)];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
r = [zeros(1, 0), o];
assert_isequal(size(r), [1 1]);
r = [o; zeros(1, 0)];
assert_isequal(size(r), [1 1]);
r = [o, zeros(0, 3)];
assert_isequal(size(r), [1 1]);
r = [o; zeros(3, 0)];
assert_isequal(size(r), [1 1]);
r = [o, cell(1, 0)];
assert_isequal(size(r), [1 1]);
r = [o, char(zeros(1, 0))];
assert_isequal(size(r), [1 1]);
r = [o, int8(zeros(1, 0))];
assert_isequal(size(r), [1 1]);
r = [o, true(1, 0)];
assert_isequal(size(r), [1 1]);
r = [o, struct([])];
assert_isequal(size(r), [1 1]);
r = [o, ClassdefStringCatValue.empty(1, 0)];
assert_isequal(size(r), [1 1]);
r = [o; ClassdefStringCatValue.empty(0, 3)];
assert_isequal(size(r), [1 1]);
r = [o, ClassdefStringCatNoArg.empty(1, 0)];
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
r = [[o; o], zeros(1, 0)];
assert_isequal(size(r), [2 1]);
r = [ClassdefStringCatNoArg(), zeros(1, 0)];
assert_isequal(class(r), 'ClassdefStringCatNoArg');
assert_checkerror('[o, zeros(1, 0, 2)]', _('Dimensions of arrays being concatenated are not consistent.'), 'Nelson:catenate:dimensionMismatch');
%=============================================================================
% cat along a dimension above 2 only drops the 0x0 operands
r = cat(3, o, zeros(1, 1, 0));
assert_isequal(class(r), V);
assert_isequal(size(r), [1 1]);
assert_checkerror('cat(3, o, zeros(1, 0))', _('Dimensions of arrays being concatenated are not consistent.'), 'Nelson:catenate:dimensionMismatch');
%=============================================================================
% the constructor failure names the conversion
try
  ClassdefStringCatNoArg(1);
catch ex
  inner = ex.message;
end
msg = sprintf(fmtU, 'double', 'ClassdefStringCatNoArg', inner);
assert_checkerror('[ClassdefStringCatNoArg.empty(), 1]', msg, idU);
msg = sprintf(fmtU, V, 'ClassdefStringCatNoArg', inner);
assert_checkerror('[ClassdefStringCatNoArg.empty(), o]', msg, idU);
r = [ClassdefStringCatNoArg.empty(), []];
assert_isequal(class(r), 'ClassdefStringCatNoArg');
assert_isequal(size(r), [0 0]);
%=============================================================================
% handle class
h = ClassdefStringCatHandle(1);
he = ClassdefStringCatHandle.empty();
r = [he, 5];
assert_isequal(class(r), H);
assert_isequal(size(r), [1 1]);
assert_isequal(r.v, 5);
r = [h, zeros(1, 0)];
assert_isequal(class(r), H);
assert_isequal(size(r), [1 1]);
r = [h; zeros(3, 0)];
assert_isequal(size(r), [1 1]);
r = [he, zeros(1, 0)];
assert_isequal(class(r), H);
assert_isequal(size(r), [1 0]);
r = [he, []];
assert_isequal(class(r), H);
assert_isequal(size(r), [0 0]);
%=============================================================================
