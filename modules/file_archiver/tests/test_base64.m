%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Known RFC 4648 vectors (char input is UTF-8 encoded before encoding).
asserts.isequal(nelson.io.base64encode('Man'), 'TWFu');
asserts.isequal(nelson.io.base64encode('M'), 'TQ==');
asserts.isequal(nelson.io.base64encode('Ma'), 'TWE=');
asserts.isequal(nelson.io.base64encode('hello'), 'aGVsbG8=');
%=============================================================================
% Empty input encodes to the empty string.
asserts.isequal(nelson.io.base64encode(''), '');
%=============================================================================
% Decoding a known value yields the original bytes.
asserts.isequal(uint8(nelson.io.base64decode('TWFu')), uint8([77 97 110]));
%=============================================================================
% Round-trip over every byte value is lossless.
data = uint8(0:255);
encoded = nelson.io.base64encode(data);
asserts.istrue(ischar(encoded));
decoded = uint8(nelson.io.base64decode(encoded));
asserts.isequal(decoded, data);
%=============================================================================
% Round-trip over ASCII text is lossless.
asserts.isequal(char(nelson.io.base64decode(nelson.io.base64encode('Nelson 123'))), 'Nelson 123');
%=============================================================================
