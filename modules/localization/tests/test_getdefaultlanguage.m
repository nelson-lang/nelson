%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('getdefaultlanguage'), 0);
asserts.isequal(nargout('getdefaultlanguage'), 1);
%=============================================================================
language = getdefaultlanguage();
asserts.isequal(class(language), 'char');
asserts.istrue(~isempty(language));
asserts.istrue(any(strcmp(language, getavailablelanguages())));
%=============================================================================
