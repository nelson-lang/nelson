%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = dictionary();
assert_isfalse(isConfigured(d));
%=============================================================================
d = configureDictionary("string", "double");
assert_istrue(isConfigured(d));
assert_isequal(d.types(), "string");
%=============================================================================