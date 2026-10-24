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
dyn = ClassdefDynamicProps();
token = addprop(dyn, 'Extra');
asserts.istrue(isvalid(c));
asserts.istrue(isvalid(lh));
asserts.istrue(isvalid(token));
delete(lh);
delete(token);
delete(c);
asserts.isfalse(isvalid(lh));
asserts.isfalse(isvalid(token));
asserts.isfalse(isvalid(c));
%=============================================================================
