%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A class overloading subsref/subsasgn: a single-level dot access from outside
% its methods calls the overloads, also when the name matches a declared
% property (Hidden or public, in any letter case). Inside the methods of the
% class, or of a subclass, obj.prop keeps reading and writing the property.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefHiddenSubsref();
assert_isequal(o.store, 'subsref:store');
assert_isequal(o.Pub, 'subsref:Pub');
assert_isequal(o.STORE, 'subsref:STORE');
assert_isequal(o.pub, 'subsref:pub');
assert_isequal(o.nothere, 'subsref:nothere');
name = 'store';
assert_isequal(o.(name), 'subsref:store');
%=============================================================================
% methods of the class read the properties directly
assert_isequal(readStore(o), 'internal');
assert_isequal(readStoreDynamic(o), 'internal');
assert_isequal(readPub(o), 'public');
assert_isequal(readStore(writeStore(o, 'w')), 'w');
assert_isequal(readPub(writePub(o, 'p')), 'p');
%=============================================================================
% assignments from outside go through subsasgn
o1 = o;
o1.store = 'x';
assert_isequal(readStore(o1), 'internal');
assert_isequal(readPub(o1), 'subsasgn:store=x');
o2 = o;
o2.Pub = 'y';
assert_isequal(readPub(o2), 'subsasgn:Pub=y');
o3 = o;
o3.STORE = 'z';
assert_isequal(readStore(o3), 'internal');
assert_isequal(readPub(o3), 'subsasgn:STORE=z');
o4 = o;
o4.(name) = 'd';
assert_isequal(readStore(o4), 'internal');
assert_isequal(readPub(o4), 'subsasgn:store=d');
%=============================================================================
% a subclass inherits the overloads; its methods and the inherited ones see
% the properties
c = ClassdefHiddenSubsrefChild();
assert_isequal(c.store, 'subsref:store');
assert_isequal(childReadStore(c), 'internal');
assert_isequal(readStore(c), 'internal');
assert_isequal(readStore(childWriteStore(c, 'cw')), 'cw');
c.store = 'q';
assert_isequal(childReadStore(c), 'internal');
assert_isequal(readPub(c), 'subsasgn:store=q');
%=============================================================================
rmpath(path_test);
%=============================================================================
