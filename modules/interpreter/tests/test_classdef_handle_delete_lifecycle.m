%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% The delete method of a handle class runs exactly once, when the last
% reference to the object goes away (clear, reassignment, scope exit, a
% consumed temporary, a container being released) or on an explicit
% delete(obj). Expected logs are the observed expected behavior.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
global CLASSDEF_LIFECYCLE_LOG
CLASSDEF_LIFECYCLE_LOG = {};
%=============================================================================
% last clear: isvalid is already false inside the destructor
w = ClassdefLifecycle('w1');
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear w
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'w1:0'});
%=============================================================================
% reassignment overwriting the last holder
CLASSDEF_LIFECYCLE_LOG = {};
w = ClassdefLifecycle('w2');
w = 1;
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'w2:0'});
w = ClassdefLifecycle('w3a');
w = ClassdefLifecycle('w3b');
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'w2:0', 'w3a:0'});
clear w
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'w2:0', 'w3a:0', 'w3b:0'});
%=============================================================================
% function-local scope exit: locals die after the body, in declaration order
CLASSDEF_LIFECYCLE_LOG = {};
classdefLifecycleScopeCase();
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'body-done', 's1:0', 's2:0', 's3:0'});
%=============================================================================
% reassignment inside a function frees the previous holder immediately
CLASSDEF_LIFECYCLE_LOG = {};
classdefLifecycleReassignCase();
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'f1:0', 'after-reassign', 'f2:0', 'after-overwrite'});
%=============================================================================
% temporaries: a value consumed inside an expression dies before the
% enclosing call proceeds; a bare expression statement is gone before the
% next statement starts
CLASSDEF_LIFECYCLE_LOG = {};
n = numel(CLASSDEF_LIFECYCLE_LOG);
c = class(ClassdefLifecycle('t2'));
asserts.isequal(c, 'ClassdefLifecycle');
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'t2:0'});
ClassdefLifecycle('t1');
ans = 0;
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'t2:0', 't1:0'});
%=============================================================================
% two variables sharing one handle: only the last release runs delete
CLASSDEF_LIFECYCLE_LOG = {};
a = ClassdefLifecycle('sh');
b = a;
clear a
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear b
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'sh:0'});
%=============================================================================
% explicit delete then clear: no second run; delete of a deleted handle is a no-op
CLASSDEF_LIFECYCLE_LOG = {};
d = ClassdefLifecycle('d1');
delete(d);
asserts.isfalse(isvalid(d));
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'d1:0'});
delete(d);
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'d1:0'});
clear d
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'d1:0'});
%=============================================================================
% handles stored in containers: releasing the container releases them
CLASSDEF_LIFECYCLE_LOG = {};
c = {ClassdefLifecycle('cell1')};
clear c
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'cell1:0'});
CLASSDEF_LIFECYCLE_LOG = {};
s.h = ClassdefLifecycle('struct1');
clear s
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'struct1:0'});
CLASSDEF_LIFECYCLE_LOG = {};
s = struct('h', ClassdefLifecycle('sh1'));
s.h = 5;
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'sh1:0'});
CLASSDEF_LIFECYCLE_LOG = {};
deep.c{1}.h = ClassdefLifecycle('deep');
deep = [];
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'deep:0'});
%=============================================================================
% handle held by the property of another handle: owner first, then the child
CLASSDEF_LIFECYCLE_LOG = {};
o = ClassdefLifecycle('outer');
o.Child = ClassdefLifecycle('inner');
clear o
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'outer:0', 'inner:0'});
CLASSDEF_LIFECYCLE_LOG = {};
p = ClassdefLifecycleParent('par');
p.Kid = ClassdefLifecycle('kid');
clear p
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'parent:par:kid=kid:1', 'kid:0'});
%=============================================================================
% handle returned from a function survives the callee scope
CLASSDEF_LIFECYCLE_LOG = {};
r = classdefLifecycleMakeOne();
asserts.istrue(isvalid(r));
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'body-done', 'local:0'});
clear r
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'body-done', 'local:0', 'ret:0'});
%=============================================================================
% handle passed as argument: shared with the caller, nothing dies at return
CLASSDEF_LIFECYCLE_LOG = {};
w = ClassdefLifecycle('arg');
classdefLifecycleArgCase(w);
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'arg:arg'});
clear w
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'arg:arg', 'arg:0'});
%=============================================================================
% handle captured by an anonymous function is kept alive by the closure
CLASSDEF_LIFECYCLE_LOG = {};
w = ClassdefLifecycle('cap');
f = @() w.Name;
clear w
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
asserts.isequal(f(), 'cap');
clear f
%=============================================================================
% exception thrown inside delete: contained as a warning, teardown continues
CLASSDEF_LIFECYCLE_LOG = {};
lastwarn('');
e = ClassdefLifecycleThrows('ex');
clear e
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'throws:ex'});
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:classdef:destructorError');
asserts.istrue(contains(msg, 'ClassdefLifecycleThrows'));
asserts.istrue(contains(msg, 'boom in delete'));
asserts.isfalse(exist('e', 'var') == 1);
% explicit delete propagates the error and still invalidates the object
e = ClassdefLifecycleThrows('ex2');
asserts.checkerror('delete(e)', 'boom in delete');
asserts.isfalse(isvalid(e));
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'throws:ex', 'throws:ex2'});
clear e
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'throws:ex', 'throws:ex2'});
%=============================================================================
% value class and handle class without delete: nothing runs
CLASSDEF_LIFECYCLE_LOG = {};
v = ClassdefPoint(1, 2);
clear v
n = ClassdefBenchHandle(1);
n = 1;
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
%=============================================================================
% subclass destructor runs first, then the superclass one
CLASSDEF_LIFECYCLE_LOG = {};
x = ClassdefLifecycleDerived('der');
clear x
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'derived:der', 'der:0'});
CLASSDEF_LIFECYCLE_LOG = {};
x = ClassdefLifecycleDerived('der2');
delete(x);
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'derived:der2', 'der2:0'});
%=============================================================================
% ObjectBeingDestroyed is notified before the destructor on implicit destruction
CLASSDEF_LIFECYCLE_LOG = {};
l = ClassdefLifecycle('ev');
addlistener(l, 'ObjectBeingDestroyed', @classdefLifecycleEventCallback);
clear l
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'event:ev:0', 'ev:0'});
%=============================================================================
% loop reassignment frees each previous iteration value
CLASSDEF_LIFECYCLE_LOG = {};
for k = 1:3
  w = ClassdefLifecycle(sprintf('loop%d', k));
end
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'loop1:0', 'loop2:0'});
clear w
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'loop1:0', 'loop2:0', 'loop3:0'});
%=============================================================================
% object arrays: element order; deleting an element frees it
CLASSDEF_LIFECYCLE_LOG = {};
arr = [ClassdefLifecycle('a1'), ClassdefLifecycle('a2')];
clear arr
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'a1:0', 'a2:0'});
CLASSDEF_LIFECYCLE_LOG = {};
arr = [ClassdefLifecycle('b1'), ClassdefLifecycle('b2')];
arr(1) = [];
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'b1:0'});
clear arr
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'b1:0', 'b2:0'});
%=============================================================================
% error unwinding: the callee local dies before the catch block runs
CLASSDEF_LIFECYCLE_LOG = {};
try
  classdefLifecycleErrCase();
catch
  CLASSDEF_LIFECYCLE_LOG{end + 1} = 'catch';
end
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'errlocal:0', 'catch'});
%=============================================================================
% clear variables releases every holder, in workspace order
CLASSDEF_LIFECYCLE_LOG = {};
p1 = ClassdefLifecycle('p1');
q1 = ClassdefLifecycle('q1');
clear variables
global CLASSDEF_LIFECYCLE_LOG
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'p1:0', 'q1:0'});
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
%=============================================================================
% command form and function form of clear inside a function
CLASSDEF_LIFECYCLE_LOG = {};
classdefLifecycleClearFormCase();
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'fa:0', 'after-clear-fn', 'fb:0', 'after-clear-cmd'});
%=============================================================================
% global variable: clearing the local link keeps it, clear global frees it
CLASSDEF_LIFECYCLE_LOG = {};
global CLASSDEF_LIFECYCLE_G
CLASSDEF_LIFECYCLE_G = ClassdefLifecycle('glob');
clear CLASSDEF_LIFECYCLE_G
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear global CLASSDEF_LIFECYCLE_G
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'glob:0'});
%=============================================================================
% a destructor that stores its object elsewhere: the handle stays invalid
CLASSDEF_LIFECYCLE_LOG = {};
global CLASSDEF_LIFECYCLE_RESURRECTED
r = ClassdefLifecycleResurrect('res');
clear r
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'resurrect:res'});
asserts.isfalse(isvalid(CLASSDEF_LIFECYCLE_RESURRECTED));
clear global CLASSDEF_LIFECYCLE_RESURRECTED
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'resurrect:res'});
%=============================================================================
clear global CLASSDEF_LIFECYCLE_LOG
rmpath(path_test);
%=============================================================================
