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
% An anonymous function definition (and the workspace it captured) lives
% exactly as long as a function handle value referencing it: it is released
% with the last copy of the value, not kept forever.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Portable runtime classdef handle lifecycle cleanup is not aligned yet.');
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
global CLASSDEF_LIFECYCLE_LOG
CLASSDEF_LIFECYCLE_LOG = {};
%=============================================================================
% a captured handle object dies with the last handle value referencing it
obj = ClassdefLifecycle('c1');
f = @() obj.Name;
clear obj
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
asserts.isequal(f(), 'c1');
clear f
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'c1:0'});
%=============================================================================
% copies of the value share the definition: only the last copy releases it
CLASSDEF_LIFECYCLE_LOG = {};
obj = ClassdefLifecycle('c2');
f = @() obj;
g = f;
c = {f};
s.h = f;
clear obj f
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear g
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear c
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
r = s.h();
asserts.isequal(r.Name, 'c2');
clear r s
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'c2:0'});
%=============================================================================
% reassignment of the variable releases the previous definition
CLASSDEF_LIFECYCLE_LOG = {};
obj = ClassdefLifecycle('c3');
f = @() obj;
clear obj
f = @() 1;
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'c3:0'});
asserts.isequal(f(), 1);
clear f
%=============================================================================
% a handle capturing another handle: releasing the outer one releases the chain
CLASSDEF_LIFECYCLE_LOG = {};
obj = ClassdefLifecycle('c4');
inner = @() obj;
outer = @() inner();
clear obj inner
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
r = outer();
asserts.isequal(r.Name, 'c4');
clear r outer
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'c4:0'});
%=============================================================================
% deep copies of the value (repmat) and element deletion share the definition
CLASSDEF_LIFECYCLE_LOG = {};
obj = ClassdefLifecycle('c5');
f = @() obj;
arr = repmat(f, 1, 3);
clear obj f
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
arr(1) = [];
asserts.isequal(size(arr), [1 2]);
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear arr
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'c5:0'});
%=============================================================================
% a handle created in a function releases its captures when the function
% returns; a handle returned by the function keeps them alive
CLASSDEF_LIFECYCLE_LOG = {};
r = anonymousFunctionLifetimeScopeCase();
asserts.isequal(r, 'scope');
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'scope:0'});
CLASSDEF_LIFECYCLE_LOG = {};
f = anonymousFunctionLifetimeReturnCase();
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
r = f();
asserts.isequal(r.Name, 'returned');
clear r f
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'returned:0'});
%=============================================================================
% temporary handles consumed by cellfun, arrayfun and feval are released
% after the call; the captured object dies with its own last reference
CLASSDEF_LIFECYCLE_LOG = {};
obj = ClassdefLifecycle('c8');
r = cellfun(@(x)[x, obj.Name], {'a', 'b'}, 'UniformOutput', false);
asserts.isequal(r, {'ac8', 'bc8'});
r = arrayfun(@(x) x + numel(obj.Name), [1 2]);
asserts.isequal(r, [3 4]);
r = feval(@() obj.Name);
asserts.isequal(r, 'c8');
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear obj
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'c8:0'});
%=============================================================================
% a handle stored in a handle object property is released with the object
CLASSDEF_LIFECYCLE_LOG = {};
holder = ClassdefLifecycle('holder');
captured = ClassdefLifecycle('captured');
holder.Child = @() captured.Name;
clear captured
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
child = holder.Child;
asserts.isequal(child(), 'captured');
clear child
clear holder
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'holder:0', 'captured:0'});
%=============================================================================
% the body of a running handle may drop the last variable holding it
r = anonymousFunctionLifetimeSelfClearCase();
asserts.isequal(r, 0);
%=============================================================================
% named handles and str2func / func2str round trips keep working
CLASSDEF_LIFECYCLE_LOG = {};
obj = ClassdefLifecycle('c10');
f = str2func('@()obj.Name');
asserts.isequal(func2str(f), '@()obj.Name');
asserts.isequal(f(), 'c10');
g = str2func('numel');
asserts.isequal(g([1 2 3]), 3);
clear f g
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {});
clear obj
asserts.isequal(CLASSDEF_LIFECYCLE_LOG, {'c10:0'});
%=============================================================================
% a handle saved to a file and reloaded is an independent definition
a = 10;
f = @(x) x + a;
filename = [tempname(), '.nh5'];
save(filename, 'f');
clear f a
loaded = load(filename);
asserts.isequal(loaded.f(5), 15);
clear loaded
delete(filename);
%=============================================================================
% the definition is not retained: many handles do not accumulate memory
m0 = memory();
for i = 1:20000
  block = zeros(1000, 1);
  f = @() block;
end
clear block f
m1 = memory();
asserts.istrue((m1.MemUsedNelson - m0.MemUsedNelson) < 40e6);
%=============================================================================
