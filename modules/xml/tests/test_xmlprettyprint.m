%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('xmlprettyprint'), -1);
assert_isequal(nargout('xmlprettyprint'), 0);
%=============================================================================
current_directory = fileparts(mfilename('fullpathext'));
xml_text = [current_directory, '/test_xmlprettyprint.xml.ref'];
xmlprettyprint(xml_text);
xmlprettyprint(xml_text, false);
xmlprettyprint(xml_text, true);
