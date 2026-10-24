%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_if_reduced_characters_encoding_backend();
%=============================================================================
asserts.isequal(nargin('unicode2native'), 2);
asserts.isequal(nargout('unicode2native'), 1);
%=============================================================================
R1 = unicode2native('éléve');
R2 = unicode2native('éléve', '');
asserts.isequal(R1, R2);
%=============================================================================
R = unicode2native('azerty', 'ascii');
REF = uint8([97 122 101 114 116 121]);
asserts.isequal(R, REF);
%=============================================================================
R = unicode2native('éléve', 'ascii');
REF = uint8([26 108 26 118 101]);
asserts.isequal(R, REF);
%=============================================================================
R = unicode2native('éléve', 'UTF-8');
REF = uint8([195 169 108 195 169 118 101]);
asserts.isequal(R, REF);
%=============================================================================
R = native2unicode(uint8([240 159 152 128]), 'UTF-8');
R = unicode2native(R, 'UTF-8');
REF = uint8([240 159 152 128]);
asserts.isequal(R, REF);
%=============================================================================
R = unicode2native('éléve', 'Windows-1252');
REF = uint8([233 108 233 118 101]);
asserts.isequal(R, REF);
%=============================================================================
R = unicode2native("Виртуальная", 'Windows-1251');
REF = uint8([194 232 240 242 243 224 235 252 237 224 255]);
asserts.isequal(R, REF);
%=============================================================================
R = unicode2native('片仮名', 'SHIFT_JIS');
REF = uint8([149 208 137 188 150 188]);
%=============================================================================
asserts.checkerror('R = unicode2native(''Виртуальная'', ''_M$_'');', [_('Invalid charset: '), '_M$_'])
asserts.checkerror('R = unicode2native(''Виртуальная'', ''_M$_'');', [_('Invalid charset: '), '_M$_'], 'Nelson:unicode2native:invalidCharset')
%=============================================================================
