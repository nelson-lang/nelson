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
% Generated sources must compile with the typst compiler when it is installed.
[st, out] = system('typst --version');
skip_testsuite(st ~= 0, 'typst compiler not available')
%=============================================================================
source = [modulepath('help_tools', 'root'), '/tests/xml_nested'];
destination = tempname();
mkdir(destination);
[status, msg] = xmldoctotypst(source, destination, 'nested_help', true);
asserts.istrue(status, msg);
pdf = [destination, '/main.pdf'];
[st, out] = system(['typst compile "', destination, '/main.typ" "', pdf, '"']);
asserts.istrue(st == 0, out);
asserts.istrue(isfile(pdf));
rmdir(destination, 's');
%=============================================================================
