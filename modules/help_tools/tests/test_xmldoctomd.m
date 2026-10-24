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
asserts.istrue(xmldoctomd(source, destination, 'nested_help', true));
asserts.istrue(isfile([destination, '/README.md']));
asserts.istrue(isfile([destination, '/SUMMARY.md']));
rmdir(destination, 's');
%=============================================================================
