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
c = ClassdefCounter();
lh = addlistener(c, 'CountChanged', @(src, eventData) src.recordEvent(eventData.EventName));
notify(c, 'CountChanged');
asserts.isequal(c.LastEvent, 'CountChanged');
delete(lh);
delete(c);
%=============================================================================
