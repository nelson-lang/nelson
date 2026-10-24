%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('underlyingType'), 1);
asserts.isequal(nargout('underlyingType'), 1);
%=============================================================================
asserts.isequal(underlyingType(3), 'double');
asserts.isequal(underlyingType(single(2)), 'single');
asserts.isequal(underlyingType(int32(3)), 'int32');
asserts.isequal(underlyingType(uint8(5)), 'uint8');
asserts.isequal(underlyingType('a'), 'char');
asserts.isequal(underlyingType(true), 'logical');
asserts.isequal(underlyingType(1i), 'double');
asserts.isequal(underlyingType([1 2 3]), 'double');
%=============================================================================
% an enumeration built on a fundamental type reports that type
asserts.isequal(underlyingType(TestEnumColor.Red), 'int32');
%=============================================================================
