%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
assert_istrue(isValidGraphicsProperty('axes', 'Type'));
assert_istrue(isValidGraphicsProperty('line', 'Type'));
assert_istrue(isValidGraphicsProperty('image', 'Type'));
assert_istrue(isValidGraphicsProperty('root', 'Type'));
assert_istrue(isValidGraphicsProperty('text', 'Type'));
assert_istrue(isValidGraphicsProperty('figure', 'Type'));
%=============================================================================
assert_isfalse(isValidGraphicsProperty('axes', 'TypeA'));
assert_isfalse(isValidGraphicsProperty('line', 'TypeA'));
assert_isfalse(isValidGraphicsProperty('image', 'TypeA'));
assert_isfalse(isValidGraphicsProperty('root', 'TypeA'));
assert_isfalse(isValidGraphicsProperty('text', 'TypeA'));
assert_isfalse(isValidGraphicsProperty('figure', 'TypeA'));
%=============================================================================