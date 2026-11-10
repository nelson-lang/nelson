%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_istrue(isUnderlyingType(single(1), 'single'));
assert_istrue(isUnderlyingType(single(1), 'numeric'));
assert_istrue(isUnderlyingType(single(1), 'float'));
assert_isfalse(isUnderlyingType(single(1), 'integer'));
assert_istrue(isUnderlyingType(int8(1), 'integer'));
assert_isfalse(isUnderlyingType(int8(1), 'foo'));
assert_isfalse(isUnderlyingType('a', 'numeric'));
assert_istrue(isUnderlyingType(true, "logical"));
assert_checkerror('isUnderlyingType(1, 2)', [sprintf(_('Invalid input argument at position %d.'), 2), char(10), _('Value must be a character vector or string scalar.')], 'Nelson:validators:mustBeTextScalar');
%=============================================================================
