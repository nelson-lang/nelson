%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% set.X(obj, v) form runs once and its inner write stores the value
p = ClassdefHandlePlainSetter();
p.Flag = 3;
asserts.isequal(p.Flag, true);
asserts.isequal(p.Calls, {'Flag'});
%=============================================================================
% obj = set.X(obj, v) form
p = ClassdefHandlePlainSetter();
p.Level = -4;
asserts.isequal(p.Level, 0);
p.Level = 7;
asserts.isequal(p.Level, 7);
asserts.isequal(p.Calls, {'Level', 'Level'});
%=============================================================================
% a setter writing another property with a setter runs that setter too
p = ClassdefHandlePlainSetter();
p.Twice = 5;
asserts.isequal(p.Twice, 10);
asserts.isequal(p.Flag, true);
asserts.isequal(p.Calls, {'Twice', 'Flag'});
%=============================================================================
% a property without a setter keeps the direct store
p = ClassdefHandlePlainSetter();
p.Plain = 42;
asserts.isequal(p.Plain, 42);
asserts.isequal(p.Calls, {});
%=============================================================================
% the object is shared: a copy of the handle sees the setter result
p = ClassdefHandlePlainSetter();
q = p;
q.Flag = 0;
asserts.isequal(p.Flag, false);
asserts.isequal(p.Calls, {'Flag'});
%=============================================================================
% assignment from a function
p = ClassdefHandlePlainSetter();
assignFlagInFunction(p, 2);
asserts.isequal(p.Flag, true);
asserts.isequal(p.Calls, {'Flag'});
%=============================================================================
% inherited setter, from outside and from a subclass method
d = ClassdefHandlePlainSetterDerived();
d.Flag = 1;
asserts.isequal(d.Flag, true);
d.resetFlag();
asserts.isequal(d.Flag, false);
asserts.isequal(d.Calls, {'Flag', 'Flag'});
%=============================================================================
% PreSet and PostSet fire once around the setter, defaults skip it
p = ClassdefHandlePlainSetter();
pre = addlistener(p, 'Watched', 'PreSet', @(src, evt) recordEvent(src, 'PreSet'));
post = addlistener(p, 'Watched', 'PostSet', @(src, evt) recordEvent(src, 'PostSet'));
asserts.isequal(p.Watched, 0);
p.Watched = 1;
asserts.isequal(p.Watched, 2);
asserts.isequal(p.Calls, {'PreSet', 'Watched', 'PostSet'});
delete(pre);
delete(post);
%=============================================================================
% set() of a SetGet class calls the setter
s = ClassdefSetGetPlainSetter();
set(s, 'Name', 'abc');
asserts.isequal(s.Name, 'ABC');
asserts.isequal(s.Count, 1);
s.Name = 'xy';
asserts.isequal(get(s, 'Name'), 'XY');
asserts.isequal(s.Count, 2);
%=============================================================================
function assignFlagInFunction(obj, value)
  obj.Flag = value;
end
%=============================================================================
function recordEvent(obj, name)
  obj.Calls{end + 1} = name;
end
%=============================================================================
