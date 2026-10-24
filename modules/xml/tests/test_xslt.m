%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('xslt'), -1);
asserts.isequal(nargout('xslt'), -1);
%=============================================================================
if ismac() && strcmp(getenv('CONDA_PREFIX'), '') == false
  skip_testsuite(true, 'Skipping xslt tests in conda environment on macOS due to libxslt issues.');
end
%=============================================================================
xml_filename = [modulepath('xml'), '/tests/test_xml.xml'];
xsl_filename = [modulepath('xml'), '/tests/test_xml_to_text.xslt'];
out_filename = [tempname(), '.txt'];
result_filename = xslt(xml_filename, xsl_filename, out_filename);
asserts.isequal(result_filename, out_filename);
asserts.istrue(isfile(out_filename));
%=============================================================================
txt = xslt(xml_filename, xsl_filename, '-tostring');
asserts.istrue(ischar(txt));
asserts.isfalse(isempty(txt));
%=============================================================================
doc = xmlread(xml_filename);
txt_from_doc = xslt(doc, xsl_filename, '-tostring');
asserts.istrue(ischar(txt_from_doc));
asserts.isfalse(isempty(txt_from_doc));
%=============================================================================
txt_with_engine = xslt(xml_filename, xsl_filename, '-tostring', 'XMLEngine', 'auto');
asserts.istrue(ischar(txt_with_engine));
asserts.isfalse(isempty(txt_with_engine));
%=============================================================================
generated_filename = xslt(xml_filename, xsl_filename);
asserts.istrue(isfile(generated_filename));
%=============================================================================
asserts.checkerror('xslt(xml_filename, xsl_filename, ''-tostring'', ''Bad'', true)', _('Property name not supported.'));
%=============================================================================
