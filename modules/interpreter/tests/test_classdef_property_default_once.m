%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% a property default expression runs once per defining class; every
% instance shares the stored value
a = ClassdefDefaultOnce();
b = ClassdefDefaultOnce();
asserts.istrue(a.Shared == b.Shared);
asserts.isequal(a.Stamp, b.Stamp);
a.Shared.Id = 42;
asserts.isequal(b.Shared.Id, 42);
%=============================================================================
% later instantiations do not run the expression again
before = classdef_default_stamp();
c = ClassdefDefaultOnce();
after = classdef_default_stamp();
asserts.isequal(after, before + 1);
asserts.isequal(c.Stamp, a.Stamp);
asserts.istrue(c.Shared == a.Shared);
%=============================================================================
% defaults with a validation expression share the evaluation too and are
% still validated per instance
asserts.isequal(a.Vec, [1 2 3]);
asserts.isequal(class(a.Vec), 'double');
asserts.isequal(c.Vec, a.Vec);
%=============================================================================
% literal defaults keep their plain per-instance assignment
asserts.isequal(a.Plain, 7);
a.Plain = 8;
asserts.isequal(b.Plain, 7);
%=============================================================================
% inherited properties reuse the value stored by the defining class
s = ClassdefDefaultOnceSub();
asserts.istrue(s.Shared == a.Shared);
asserts.isequal(s.Stamp, a.Stamp);
asserts.isequal(s.Own, 3);
%=============================================================================
% value classes share the evaluation as well; each defining class stores
% its own value
v1 = ClassdefDefaultOnceValue();
v2 = ClassdefDefaultOnceValue();
asserts.istrue(v1.Shared == v2.Shared);
asserts.isequal(v1.Stamp, v2.Stamp);
asserts.isfalse(v1.Shared == a.Shared);
asserts.isequal(v1.Shared.Id, 0);
%=============================================================================
% instances hold independent copies of the stored value
v1.Data(1) = 99;
asserts.isequal(v2.Data, [1 2 3]);
v3 = ClassdefDefaultOnceValue();
asserts.isequal(v3.Data, [1 2 3]);
%=============================================================================
% clearing classes drops the stored defaults: expressions run again
clear('a', 'b', 'c', 's', 'v1', 'v2', 'v3');
clear('classes');
d = ClassdefDefaultOnce();
e = ClassdefDefaultOnce();
asserts.istrue(d.Shared == e.Shared);
asserts.isequal(d.Shared.Id, 0);
%=============================================================================
% a property default that references another property fails at construction,
% wrapped against the offending property and class (the default cannot see the
% other properties). Matches expected behavior wrapper "Invalid default value for
% property 'y' in class '...':<newline><inner>".
clear('d', 'e');
failed = false;
try
  ClassdefDefaultSiblingReference();
catch err
  failed = true;
  asserts.istrue(startsWith(err.message, ...
    ['Invalid default value for property ''y'' in class ', ...
    '''ClassdefDefaultSiblingReference'':', char(10)]));
end
asserts.istrue(failed);
%=============================================================================
rmpath(path_test);
%=============================================================================
