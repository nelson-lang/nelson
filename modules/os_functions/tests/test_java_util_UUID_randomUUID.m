%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
u = char(java.util.UUID.randomUUID);
assert_istrue(ischar(u));
assert_isequal(size(u), [1 36]);
%=============================================================================
% canonical 8-4-4-4-12 layout with hyphens at fixed positions
assert_isequal(u([9 14 19 24]), '----');
%=============================================================================
% only lowercase hexadecimal digits and hyphens
hexAndDash = all(ismember(u, '0123456789abcdef-'));
assert_istrue(hexAndDash);
%=============================================================================
% version 4 nibble and RFC 4122 variant nibble
assert_isequal(u(15), '4');
assert_istrue(any(u(20) == '89ab'));
%=============================================================================
% two successive identifiers differ
u2 = char(java.util.UUID.randomUUID);
assert_isfalse(strcmp(u, u2));
%=============================================================================
% callable with explicit parentheses as well
u3 = char(java.util.UUID.randomUUID());
assert_isequal(size(u3), [1 36]);
%=============================================================================
