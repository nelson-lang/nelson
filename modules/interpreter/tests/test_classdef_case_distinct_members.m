%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Member names (properties, methods, enumerations) are case-sensitive, so
% members whose names differ only in letter case are distinct and a classdef
% that declares them must load successfully.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% A private 'foo' property and a dependent 'Foo' property are distinct: the
% class constructs and the dependent getter returns the private value.
o = ClassdefCaseDistinctMembers();
assert_isequal(o.Foo, 42);
%=============================================================================
% An exact-case duplicate property must still be rejected.
id = '';
try
  ClassdefDuplicateProperty();
catch me
  id = me.identifier;
end
assert_isequal(id, 'Nelson:class:duplicateProperty');
%=============================================================================
rmpath(path_test);
%=============================================================================
