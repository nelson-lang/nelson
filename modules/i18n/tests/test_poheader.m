%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('poheader'), 2);
asserts.isequal(nargout('poheader'), 1);
%=============================================================================
header = poheader('DOMAIN', 'fr_FR');
asserts.isequal(class(header), 'cell');
asserts.isequal(size(header), [8 1]);
asserts.isequal(header{1}, 'msgid ""');
asserts.isequal(header{2}, 'msgstr ""');
asserts.isequal(header{3}, '"Project-Id-Version: DOMAIN\n"');
asserts.isequal(header{4}, '"Language: fr_FR\n"');
asserts.isequal(header{8}, '');
%=============================================================================
