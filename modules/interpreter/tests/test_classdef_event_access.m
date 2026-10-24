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
asserts.isequal(parsefile([path_test, '/ClassdefEventAccessBase.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefEventAccessChild.m']), 'classdef');
%=============================================================================
obj = ClassdefEventAccessBase();
publicEvents = events(obj);
asserts.istrue(any(strcmp(publicEvents, 'PublicEvent')));
asserts.istrue(any(strcmp(publicEvents, 'ObjectBeingDestroyed')));
asserts.isfalse(any(strcmp(publicEvents, 'HiddenEvent')));
asserts.isfalse(any(strcmp(publicEvents, 'PrivateEvent')));
asserts.isfalse(any(strcmp(publicEvents, 'ProtectedEvent')));
asserts.isequal(events('ClassdefEventAccessBase'), publicEvents);
childPublicEvents = events('ClassdefEventAccessChild');
asserts.istrue(any(strcmp(childPublicEvents, 'PublicEvent')));
asserts.istrue(any(strcmp(childPublicEvents, 'ObjectBeingDestroyed')));
asserts.isfalse(any(strcmp(childPublicEvents, 'HiddenEvent')));
asserts.isfalse(any(strcmp(childPublicEvents, 'PrivateEvent')));
asserts.isfalse(any(strcmp(childPublicEvents, 'ProtectedEvent')));
publicListener = addlistener(obj, 'PublicEvent', @(src, eventData) src.recordEvent(eventData.EventName));
notify(obj, 'PublicEvent');
asserts.isequal(obj.LastEvent, 'PublicEvent');
delete(publicListener);
%=============================================================================
privateListener = obj.listenPrivate();
obj.triggerPrivate();
asserts.isequal(obj.LastEvent, 'PrivateEvent');
delete(privateListener);
asserts.checkerror('addlistener(obj, ''PrivateEvent'', @(src, eventData) src.recordEvent(eventData.EventName));', ...
  'Cannot listen event: PrivateEvent');
asserts.checkerror('notify(obj, ''PrivateEvent'');', 'Cannot notify event: PrivateEvent');
%=============================================================================
child = ClassdefEventAccessChild();
protectedListener = child.listenProtected();
child.triggerProtected();
asserts.isequal(child.LastEvent, 'ProtectedEvent');
delete(protectedListener);
asserts.checkerror('addlistener(child, ''ProtectedEvent'', @(src, eventData) src.recordEvent(eventData.EventName));', ...
  'Cannot listen event: ProtectedEvent');
asserts.checkerror('notify(child, ''ProtectedEvent'');', 'Cannot notify event: ProtectedEvent');
%=============================================================================
meta = metaclass('ClassdefEventAccessBase');
asserts.isequal({meta.EventList.Name}', publicEvents);
eventNames = {meta.EventDetails.Name};
hiddenEvent = meta.EventDetails(find(strcmp(eventNames, 'HiddenEvent')));
asserts.istrue(hiddenEvent.Hidden);
privateEvent = meta.EventDetails(find(strcmp(eventNames, 'PrivateEvent')));
asserts.isequal(privateEvent.ListenAccess, 'private');
asserts.isequal(privateEvent.NotifyAccess, 'private');
protectedEvent = meta.EventDetails(find(strcmp(eventNames, 'ProtectedEvent')));
asserts.isequal(protectedEvent.ListenAccess, 'protected');
asserts.isequal(protectedEvent.NotifyAccess, 'protected');
childMeta = metaclass('ClassdefEventAccessChild');
asserts.isequal({childMeta.EventList.Name}', childPublicEvents);
childEventNames = {childMeta.EventDetails.Name};
asserts.istrue(any(strcmp(childEventNames, 'HiddenEvent')));
asserts.istrue(any(strcmp(childEventNames, 'PrivateEvent')));
asserts.istrue(any(strcmp(childEventNames, 'ProtectedEvent')));
%=============================================================================
rmpath(path_test);
%=============================================================================
