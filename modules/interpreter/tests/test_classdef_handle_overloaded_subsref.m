%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A handle class overloading subsref/subsasgn: a single-level dot access from
% outside its methods calls the overloads, also when the name matches a
% declared property (Hidden or public). Inside the methods of the class, or of
% a subclass, h.prop keeps reading and writing the property.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
h = ClassdefHandleHiddenSubsref();
assert_isequal(h.store, 'subsref:store');
assert_isequal(h.Pub, 'subsref:Pub');
assert_isequal(h.nothere, 'subsref:nothere');
name = 'store';
assert_isequal(h.(name), 'subsref:store');
% subsref forwarding to builtin('subsref'): the getter dispatches its calls
assert_isequal(h.InnerKeys, {'k1', 'k2'});
%=============================================================================
% methods of the class read and write the properties directly
assert_isequal(readStore(h), 'internal');
assert_isequal(readStoreDynamic(h), 'internal');
assert_isequal(readPub(h), 'public');
writeStore(h, 'w');
assert_isequal(readStore(h), 'w');
writePub(h, 'p');
assert_isequal(readPub(h), 'p');
%=============================================================================
% builtin subsref/subsasgn in a method use the default indexing
assert_isequal(readViaBuiltin(h, 'store'), 'w');
writeViaBuiltin(h, 'store', 'b');
assert_isequal(readStore(h), 'b');
writeViaBuiltin(h, 'Pub', 'bp');
assert_isequal(readPub(h), 'bp');
%=============================================================================
% assignments from outside go through subsasgn
h1 = ClassdefHandleHiddenSubsref();
h1.store = 'x';
assert_isequal(readStore(h1), 'internal');
assert_isequal(readPub(h1), 'subsasgn:store=x');
h2 = ClassdefHandleHiddenSubsref();
h2.Pub = 'y';
assert_isequal(readPub(h2), 'subsasgn:Pub=y');
h3 = ClassdefHandleHiddenSubsref();
h3.(name) = 'd';
assert_isequal(readStore(h3), 'internal');
assert_isequal(readPub(h3), 'subsasgn:store=d');
%=============================================================================
% a subclass inherits the overloads; its methods and the inherited ones see
% the properties
c = ClassdefHandleHiddenSubsrefChild();
assert_isequal(c.store, 'subsref:store');
assert_isequal(childReadStore(c), 'internal');
assert_isequal(readStore(c), 'internal');
childWriteStore(c, 'cw');
assert_isequal(readStore(c), 'cw');
c.store = 'q';
assert_isequal(childReadStore(c), 'cw');
assert_isequal(readPub(c), 'subsasgn:store=q');
%=============================================================================
rmpath(path_test);
%=============================================================================
