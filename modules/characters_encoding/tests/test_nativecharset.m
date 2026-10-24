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
asserts.isequal(nargin('nativecharset'), 1);
asserts.isequal(nargout('nativecharset'), 1);
%=============================================================================
C = nativecharset(uint8([65 66 67]));
asserts.istrue(iscell(C));
asserts.istrue(~isempty(C));
asserts.istrue(ischar(C{1}));
%=============================================================================
C = nativecharset('Nelson');
asserts.istrue(iscell(C));
asserts.istrue(~isempty(C));
%=============================================================================
