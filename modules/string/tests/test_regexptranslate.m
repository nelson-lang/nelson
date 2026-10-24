%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('regexptranslate'), -2);
asserts.isequal(nargout('regexptranslate'), 1);
%=============================================================================
r = regexptranslate('escape', 'a+b*c?.m');
asserts.isequal(r, 'a\+b\*c\?\.m');
%=============================================================================
r = regexptranslate('wildcard', '*.m');
asserts.isequal(r, '.*\.m');
%=============================================================================
r = regexptranslate('wildcard', {'*.m', 'test?.mat'});
asserts.isequal(r, {'.*\.m', 'test.\.mat'});
%=============================================================================
r = regexptranslate('escape', "a+b");
asserts.isequal(r, "a\+b");
%=============================================================================
