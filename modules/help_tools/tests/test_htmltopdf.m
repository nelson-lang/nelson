%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('htmltopdf'), 2);
asserts.isequal(nargout('htmltopdf'), 0);
%=============================================================================
html_file = [tempdir(), 'test_htmltopdf_in.html'];
pdf_file = [tempdir(), 'test_htmltopdf_out.pdf'];
txt = {'## Nelson'; '>html to pdf conversion test'};
html = markdown(txt);
fid = fopen(html_file, 'wt');
fwrite(fid, html);
fclose(fid);
if isfile(pdf_file)
  delete(pdf_file);
end
%=============================================================================
% The conversion needs the web stack and a Chromium browser; skip cleanly when
% the backend is unavailable (e.g. nelson-cli) or no browser is installed.
available = true;
try
  htmltopdf(html_file, pdf_file);
catch e
  if strcmp(e.identifier, 'Nelson:htmltopdf:notAvailable') || ...
     strcmp(e.identifier, 'Nelson:htmltopdf:conversionFailed')
    available = false;
  else
    rethrow(e);
  end
end
skip_testsuite(~available, 'HTML to PDF conversion backend is not available.');
%=============================================================================
asserts.istrue(isfile(pdf_file));
info = dir(pdf_file);
asserts.istrue(info.bytes > 0);
%=============================================================================
