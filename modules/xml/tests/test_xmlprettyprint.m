%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('xmlprettyprint'), -1);
asserts.isequal(nargout('xmlprettyprint'), 0);
%=============================================================================
current_directory = fileparts(mfilename('fullpathext'));
xml_text = [current_directory, '/test_xmlprettyprint.xml'];
xml_copy = [tempname(), '.xml'];
copyfile(xml_text, xml_copy);
xmlprettyprint(xml_copy);
content = fileread(xml_copy);
REF = '<?xml version="1.0" encoding="UTF-8"?>
<xmldoc>
  <copyright>SAME AS NELSON SOFTWARE</copyright>
  <language>en_US</language>
  <module_name>xml</module_name>
  <chapter>XmlProcessing</chapter>
  <chapter_description>
    <p>The XML module provides functions to create, convert, and manage XML documents for Nelson.</p>
  </chapter_description>
</xmldoc>
';
asserts.isequal(content, REF);
%=============================================================================
xml_copy = [tempname(), '.xml'];
copyfile(xml_text, xml_copy);
xmlprettyprint(xml_copy, false);
content = fileread(xml_copy);
REF = '<?xml version="1.0" encoding="UTF-8"?><xmldoc><copyright>SAME AS NELSON SOFTWARE</copyright><language>en_US</language><module_name>xml</module_name><chapter>XmlProcessing</chapter><chapter_description><p>The XML module provides functions to create, convert, and manage XML documents for Nelson.</p></chapter_description></xmldoc>';
asserts.isequal(content, REF);
rmfile(xml_copy);
%=============================================================================
xml_copy = [tempname(), '.xml'];
copyfile(xml_text, xml_copy);
xmlprettyprint(xml_copy, true);
content = fileread(xml_copy);
REF = '<?xml version="1.0" encoding="UTF-8"?>
<xmldoc>
  <copyright>SAME AS NELSON SOFTWARE</copyright>
  <language>en_US</language>
  <module_name>xml</module_name>
  <chapter>XmlProcessing</chapter>
  <chapter_description>
    <p>The XML module provides functions to create, convert, and manage XML documents for Nelson.</p>
  </chapter_description>
</xmldoc>
';
asserts.isequal(content, REF);
rmfile(xml_copy);
%=============================================================================
