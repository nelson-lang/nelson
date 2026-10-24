%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
delete(timerfindall());
t = timer('Tag', 'visible_search_direct');
asserts.isequal(length(timerfind('Tag', 'visible_search_direct')), 1);
delete(t);
delete(timerfindall());
%=============================================================================
