%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('xmltransform'), -3);
asserts.isequal(nargout('xmltransform'), -1);
%=============================================================================
skip_testsuite(~isempty(getenv('CONDA_PREFIX')) && ismac(), ...
  'Skipping xmltransform tests in conda environment on macOS due to libxslt issues.');
xml_filename = [modulepath('xml'), '/tests/test_xml.xml'];
xsl_filename = [modulepath('xml'), '/tests/test_xml_to_text.xslt'];
output_filename = [tempname(), '.html'];
R = xmltransform(xml_filename, xsl_filename, output_filename);
asserts.istrue(R);
R = fileread(output_filename);
REF =  'TABLE DATA REPORT
===================

Gender | Age    | State | Active
-------|--------|-------|-------
Male   | 45     | NY    | Yes (Alt: 45)
Female | 41     | CA    | No (Alt: 32)
Male   | 40     | MA    | No (Alt: 34)

SUMMARY:
--------
Total records: 3
Male records: 2
Female records: 1
Active records: 1
';
asserts.isequal(R, REF);
%=============================================================================
source_filename = [tempname(), '.txt'];
source_content = ['line one', newline(), 'line two'];
filewrite(source_filename, source_content);
xml_source_filename = [tempname(), '.xml'];
xsl_source_filename = [tempname(), '.xslt'];
output_source_filename = [tempname(), '.html'];
xml_content = ['<root path="', source_filename, '"/>'];
xsl_content = ['<?xml version="1.0" encoding="UTF-8"?>', newline(), ...
  '<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ext="http://io.github.nelson_lang/ext" version="1.0">', newline(), ...
  '<xsl:output method="text" encoding="UTF-8"/>', newline(), ...
  '<xsl:template match="/">', newline(), ...
  '<xsl:value-of select="ext:source_code(/root/@path)"/>', newline(), ...
  '</xsl:template>', newline(), ...
  '</xsl:stylesheet>', newline()];
filewrite(xml_source_filename, xml_content);
filewrite(xsl_source_filename, xsl_content);
R = xmltransform(xml_source_filename, xsl_source_filename, output_source_filename);
asserts.istrue(R);
R = fileread(output_source_filename);
asserts.isequal(R, source_content);
%=============================================================================
% ext:source_code(path, start, end): only the lines from the first line
% containing `start` to the first following line containing `end`.
range_source_filename = [tempname(), '.txt'];
filewrite(range_source_filename, ['alpha', newline(), 'c = entry(''CCC'', ...', newline(), '  ''desc'');', newline(), 'omega']);
xml_range_filename = [tempname(), '.xml'];
xsl_range_filename = [tempname(), '.xslt'];
output_range_filename = [tempname(), '.html'];
xsl_range_content = ['<?xml version="1.0" encoding="UTF-8"?>', newline(), ...
  '<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ext="http://io.github.nelson_lang/ext" version="1.0">', newline(), ...
  '<xsl:output method="text" encoding="UTF-8"/>', newline(), ...
  '<xsl:template match="/">', newline(), ...
  '<xsl:value-of select="ext:source_code(/root/@path, string(/root/@start), string(/root/@end))"/>', newline(), ...
  '</xsl:template>', newline(), ...
  '</xsl:stylesheet>', newline()];
filewrite(xsl_range_filename, xsl_range_content);
filewrite(xml_range_filename, ['<root path="', range_source_filename, '" start="entry(''CCC''" end="'');"/>']);
R = xmltransform(xml_range_filename, xsl_range_filename, output_range_filename);
asserts.istrue(R);
R = fileread(output_range_filename);
asserts.isequal(R, ['c = entry(''CCC'', ...', newline(), '  ''desc'');']);
% empty markers keep the whole file
filewrite(xml_range_filename, ['<root path="', range_source_filename, '"/>']);
R = xmltransform(xml_range_filename, xsl_range_filename, output_range_filename, true);
asserts.istrue(R);
R = fileread(output_range_filename);
asserts.isequal(R, fileread(range_source_filename));
% a marker that does not exist is an error, not a silent empty block
filewrite(xml_range_filename, ['<root path="', range_source_filename, '" start="entry(''Nope''" end="'');"/>']);
try
  xmltransform(xml_range_filename, xsl_range_filename, output_range_filename, true);
  asserts.istrue(false);
catch ME
  asserts.isequal(ME.identifier, 'Nelson:xmltransform:transformFailed');
end
%=============================================================================
