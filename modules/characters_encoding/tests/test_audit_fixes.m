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
% unicode2native must not truncate at the first NUL byte of the output
R = unicode2native('AB', 'UTF-16LE');
REF = uint8([65 0 66 0]);
asserts.isequal(R, REF);
%=============================================================================
R = unicode2native('AB', 'UTF-16BE');
REF = uint8([0 65 0 66]);
asserts.isequal(R, REF);
%=============================================================================
% embedded NUL characters in the input are preserved
R = unicode2native(['a' char(0) 'b'], 'UTF-8');
REF = uint8([97 0 98]);
asserts.isequal(R, REF);
%=============================================================================
% native2unicode must not overread nor truncate on NUL bytes
R = native2unicode(uint8([104 101 0 108 108 111]), 'UTF-8');
asserts.isequal(double(R), [104 101 0 108 108 111]);
%=============================================================================
R = native2unicode(uint8([]), 'UTF-8');
asserts.istrue(isempty(R));
%=============================================================================
% nativecharset must not overread on a buffer containing NUL bytes
c = nativecharset(uint8([104 101 0 108 108 111]));
asserts.istrue(iscell(c));
%=============================================================================
% non regression: round-trip without NUL
R = unicode2native('éléve', 'UTF-8');
REF = uint8([195 169 108 195 169 118 101]);
asserts.isequal(R, REF);
asserts.isequal(native2unicode(REF, 'UTF-8'), 'éléve');
%=============================================================================
