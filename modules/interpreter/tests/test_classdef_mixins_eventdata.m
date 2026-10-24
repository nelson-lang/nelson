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
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% event.EventData subclass: typed payload with Source/EventName filled at notify
c = ClassdefCounter(0);
payload = ClassdefEventPayload('hello');
asserts.istrue(isa(payload, 'event.EventData'));
asserts.istrue(isempty(payload.Source));
lh = addlistener(c, 'CountChanged', @(src, evt) src.recordEvent([evt.Name, '|', evt.EventName, '|', class(evt.Source)]));
notify(c, 'CountChanged', ClassdefEventPayload('hello'));
asserts.isequal(c.LastEvent, 'hello|CountChanged|ClassdefCounter');
delete(lh);
%=============================================================================
% non EventData payloads are rejected
asserts.checkerror('notify(c, ''CountChanged'', struct(''Name'', ''x''));', ...
  _('Invalid input argument for function ''notify''.'));
asserts.checkerror('notify(c, ''CountChanged'', 42);', ...
  _('Invalid input argument for function ''notify''.'));
delete(c);
%=============================================================================
% nelson.mixin.SetGet: handle semantics + set/get forms
d = ClassdefSetGetDevice();
asserts.istrue(isa(d, 'handle'));
asserts.istrue(isa(d, 'nelson.mixin.SetGet'));
set(d, 'Name', 'probe');
asserts.isequal(get(d, 'Name'), 'probe');
set(d, 'Name', 'x', 'Value', 3);
asserts.isequal(get(d, 'Value'), 3);
% case-insensitive matching
set(d, 'value', 4);
asserts.isequal(get(d, 'VALUE'), 4);
% get(obj) returns a struct of properties
s = get(d);
asserts.isequal(s.Name, 'x');
asserts.isequal(s.Value, 4);
% cell form of get
values = get(d, {'Name', 'Value'});
asserts.isequal(values, {'x', 4});
% set(obj) lists settable properties
names = set(d);
asserts.istrue(any(strcmp(names, 'Name')));
asserts.checkerror('set(d, ''DoesNotExist'', 1);', ...
  sprintf(_('Unrecognized property ''%s''.'), 'DoesNotExist'));
delete(d);
%=============================================================================
% nelson.mixin.Heterogeneous: mixed subclasses concatenate as the common root
arr = [ClassdefHeteroA(), ClassdefHeteroB()];
asserts.isequal(class(arr), 'ClassdefHeteroBase');
asserts.isequal(numel(arr), 2);
asserts.isequal(arr(1).Tag, 'A');
asserts.isequal(arr(2).Tag, 'B');
col = [ClassdefHeteroA(); ClassdefHeteroB(); ClassdefHeteroA()];
asserts.isequal(class(col), 'ClassdefHeteroBase');
asserts.isequal(size(col), [3 1]);
%=============================================================================
% same-class concatenation keeps the subclass
same = [ClassdefHeteroA(), ClassdefHeteroA()];
asserts.isequal(class(same), 'ClassdefHeteroA');
asserts.isequal(same(2).PA, 1);
%=============================================================================
