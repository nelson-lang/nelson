%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% a Constant property default expression runs once per defining class;
% every access returns the stored value (same handle)
h1 = ClassdefConstantOnce.Shared;
h2 = ClassdefConstantOnce.Shared;
asserts.istrue(h1 == h2);
%=============================================================================
% a default with a side effect runs only once
s1 = ClassdefConstantOnce.Stamp;
s2 = ClassdefConstantOnce.Stamp;
asserts.isequal(s1, s2);
probe = classdef_default_stamp();
asserts.isequal(probe, s1 + 1);
%=============================================================================
% mutating the shared handle is visible through later accesses
h1.Id = 42;
tmp = ClassdefConstantOnce.Shared;
asserts.isequal(tmp.Id, 42);
%=============================================================================
% subclasses reuse the value stored by the defining class
asserts.istrue(ClassdefConstantOnceSub.Shared == h1);
asserts.isequal(ClassdefConstantOnceSub.Stamp, s1);
%=============================================================================
% instance access resolves to the same stored value
k = ClassdefConstantOnce();
asserts.istrue(k.Shared == h1);
asserts.isequal(k.Stamp, s1);
%=============================================================================
% non-literal value expressions are stored too
asserts.isequal(ClassdefConstantOnce.Vec, [1 2 3]);
asserts.isequal(ClassdefConstantOnce.Vec, [1 2 3]);
%=============================================================================
% literal constants keep their plain inline path
asserts.isequal(ClassdefConstantOnce.Plain, 7);
asserts.isequal(ClassdefConstantOnce.Plain, 7);
%=============================================================================
% clearing classes drops the stored constants: expressions run again
clear('h1', 'h2', 'tmp', 'k');
clear('classes');
r1 = ClassdefConstantOnce.Shared;
asserts.isequal(r1.Id, 0);
r2 = ClassdefConstantOnce.Shared;
asserts.istrue(r1 == r2);
st1 = ClassdefConstantOnce.Stamp;
st2 = ClassdefConstantOnce.Stamp;
asserts.isequal(st1, st2);
%=============================================================================
clear('r1', 'r2');
rmpath(path_test);
%=============================================================================
