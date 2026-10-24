%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('global'), -1);
asserts.isequal(nargout('global'), 0);
%=============================================================================
clear global nelsonGlobalTestValue
asserts.isfalse(isglobal('nelsonGlobalTestValue'));
global nelsonGlobalTestValue
nelsonGlobalTestValue = 42;
asserts.istrue(isglobal('nelsonGlobalTestValue'));
asserts.isequal(nelsonGlobalTestValue, 42);
clear global nelsonGlobalTestValue
asserts.isfalse(isglobal('nelsonGlobalTestValue'));
%=============================================================================
