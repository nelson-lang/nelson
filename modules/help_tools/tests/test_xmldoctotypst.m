%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
source = [modulepath('help_tools', 'root'), '/tests/xml_nested'];
destination = tempname();
mkdir(destination);
[status, msg] = xmldoctotypst(source, destination, 'nested_help', true);
asserts.istrue(status, msg);
asserts.istrue(isfile([destination, '/main.typ']));
asserts.istrue(isfile([destination, '/toc.typ']));
asserts.istrue(isfile([destination, '/nelson_help.typ']));
asserts.istrue(isfile([destination, '/rootplot.typ']));
asserts.istrue(isfile([destination, '/plots/surf.typ']));
asserts.istrue(isfile([destination, '/plots/advanced/mesh.typ']));
asserts.isfalse(isfile([destination, '/help_toc_summary.xml']));
%=============================================================================
% nested page: import relative to the module root, label <module:path>
content = fileread([destination, '/plots/surf.typ']);
asserts.istrue(contains(content, '#import "../nelson_help.typ": *'));
asserts.istrue(contains(content, '= surf <nested_help:plots.surf>'));
asserts.istrue(contains(content, '== Syntax'));
asserts.istrue(contains(content, '- #raw("surf(X)")'));
asserts.istrue(contains(content, '#nlink(<nested_help:plots.advanced.mesh>)[mesh]'));
asserts.istrue(contains(content, '== See also'));
%=============================================================================
% module root document includes every page below its chapters
content = fileread([destination, '/main.typ']);
asserts.istrue(contains(content, '#import "nelson_help.typ": *'));
asserts.istrue(contains(content, '= Nested Help Module'));
asserts.istrue(contains(content, '#nested['));
asserts.istrue(contains(content, '#include "plots/surf.typ"'));
asserts.istrue(contains(content, '#include "plots/advanced/mesh.typ"'));
asserts.istrue(contains(content, '#nlink(<nested_help:plots.surf>)[surf]'));
%=============================================================================
content = fileread([destination, '/toc.typ']);
asserts.istrue(contains(content, '#nlink(<nested_help:rootplot>)[rootplot]'));
%=============================================================================
rmdir(destination, 's');
%=============================================================================
