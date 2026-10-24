%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% addlistener binds the listener lifetime to the source object: the callback
% must keep firing even when the returned listener value is not retained.
c = ClassdefCounter(0);
addlistener(c, 'CountChanged', @(src, eventData) src.recordEvent(eventData.EventName));
c.increment();
asserts.isequal(c.LastEvent, 'CountChanged');
c.recordEvent('');
c.increment();
asserts.isequal(c.LastEvent, 'CountChanged');
%=============================================================================
% deleting the source invalidates its bound listeners
lh = addlistener(c, 'CountChanged', @(src, eventData) src.recordEvent(eventData.EventName));
delete(c);
asserts.isfalse(isvalid(lh));
%=============================================================================
% listener lifetime follows the returned listener object only
c2 = ClassdefCounter(0);
listener(c2, 'CountChanged', @(src, eventData) src.recordEvent(eventData.EventName));
c2.increment();
asserts.isequal(c2.LastEvent, '');
%=============================================================================
lh2 = listener(c2, 'CountChanged', @(src, eventData) src.recordEvent(eventData.EventName));
c2.increment();
asserts.isequal(c2.LastEvent, 'CountChanged');
c2.recordEvent('');
clear lh2
c2.increment();
asserts.isequal(c2.LastEvent, '');
delete(c2);
%=============================================================================
