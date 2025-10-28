%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('xmltransform'), -3);
assert_isequal(nargout('xmltransform'), -1);
%=============================================================================
skip_testsuite(~isempty(getenv('CONDA_PREFIX')) && ismac(), ...
  'Skipping xmltransform tests in conda environment on macOS due to libxslt issues.');
xml_filename = [modulepath('xml'), '/tests/test_xml.xml'];
xsl_filename = [modulepath('xml'), '/tests/test_xml_to_text.xslt'];
output_filename = [tempdir(), 'test_xml_output.html'];
R = xmltransform(xml_filename, xsl_filename, output_filename);
assert_istrue(R);
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
assert_isequal(R, REF);
%=============================================================================