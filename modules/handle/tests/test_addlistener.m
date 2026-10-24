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
c = ClassdefCounter(0);
lh = addlistener(c, 'CountChanged', @(src, eventData) src.recordEvent(eventData.EventName));
c.increment();
asserts.isequal(c.LastEvent, 'CountChanged');
asserts.istrue(isvalid(lh));
delete(lh);
asserts.isfalse(isvalid(lh));
delete(c);
%=============================================================================
