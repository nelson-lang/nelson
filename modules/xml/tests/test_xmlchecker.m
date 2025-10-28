%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('xmlchecker'), 2);
assert_isequal(nargout('xmlchecker'), -1);
%=============================================================================
xml_filename = [modulepath('xml'), '/tests/test_xml.xml'];
xsd_filename = [modulepath('xml'), '/tests/test_xml.xsd'];
[is_valid, errors] = xmlchecker(xml_filename, xsd_filename);
assert_istrue(is_valid);
%=============================================================================
