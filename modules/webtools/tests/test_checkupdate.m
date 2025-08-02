%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[res, msg, html_url] = checkupdate('url', [modulepath('webtools'), '/tests/test_checkupdate.json']);
assert_istrue(res);
assert_isequal(msg, [_('A new version is available:'), ' ', 'v99.99.0']);
assert_isequal(html_url, 'https://github.com/nelson-lang/nelson/releases/tag/v99.99.99');
%=============================================================================
cmd = "checkupdate('url', [modulepath('webtools'), '/tests/test_checkupdate.json'], 'forcenogui', true)";
res = evalc(cmd);
REF = sprintf(['\n', [_('A new version is available:'), ' ', 'v99.99.0'], '\n', 'https://github.com/nelson-lang/nelson/releases/tag/v99.99.99', '\n']);
assert_isequal(res, REF);
%=============================================================================