%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isglobal'), 1);
asserts.isequal(nargout('isglobal'), 1);
%=============================================================================
clear global nelsonIsGlobalTestValue
asserts.isfalse(isglobal('nelsonIsGlobalTestValue'));
global nelsonIsGlobalTestValue
asserts.istrue(isglobal('nelsonIsGlobalTestValue'));
clear global nelsonIsGlobalTestValue
asserts.isfalse(isglobal('nelsonIsGlobalTestValue'));
%=============================================================================
